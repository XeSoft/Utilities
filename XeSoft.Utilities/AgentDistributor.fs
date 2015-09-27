namespace XeSoft.Utilities.Agents

open System.Threading

type private DistributorOp<'key, 'message, 'result when 'key : comparison> =
| Distribute of 'key * 'message * reply:('result -> unit)
| CompleteRun of 'key
| Shutdown of signalComplete:(unit -> unit)

type private AgentCounter<'message, 'result> = {
    mutable MessageCount: int;
    Agent: Agent<'message, 'result>;
}

type private DistributorResult<'result> =
| MessageDistributed
| DistributionFailed
| AgentStatsUpdated
| ShutdownCompleted

type AgentDistributor<'key, 'message, 'result when 'key : comparison> =
    private {
        AgentsByKey: System.Collections.Generic.Dictionary<'key, AgentCounter<'message,'result>>;
        Canceller: CancellationTokenSource;
        Mailbox: MailboxProcessor<DistributorOp<'key, 'message, Async<'result>>>;
        HashFn: 'message -> 'key;
    }

module AgentDistributor =
    
    let create (processFn:'message -> Async<'result>) (failFn: exn -> 'result) (hashFn: 'message -> 'key) =

        let startAgent t f = MailboxProcessor.Start (f, cancellationToken = t)
        let canceller = new CancellationTokenSource ()
    
        let agentsByKey = new System.Collections.Generic.Dictionary<'key, AgentCounter<_,_>> ()

        let getAgentCounter key =
            match agentsByKey.TryGetValue key with
            | (true, agentCounter) -> agentCounter
            | _ -> // new agent
                let agent = Agent.create processFn failFn
                let agentCounter = { MessageCount = 0; Agent = agent }
                agentsByKey.Add (key, agentCounter)
                agentCounter

        let getAgent key = (getAgentCounter key).Agent

        let oneAdded key = 
            let counter = getAgentCounter key
            counter.MessageCount <- counter.MessageCount + 1

        let oneFinished key =
            let counter = getAgentCounter key
            counter.MessageCount <- counter.MessageCount - 1
            // remove if no more messages
            if counter.MessageCount = 0 then
                Agent.stopNow counter.Agent
                agentsByKey.Remove key |> ignore

        let runTurn op =
            match op with
            | Shutdown signalComplete ->
                agentsByKey.Values
                |> Seq.map (fun agentCounter -> Agent.stop agentCounter.Agent)
                |> Async.Parallel
                |> Async.Ignore
                |> Async.RunSynchronously
                signalComplete ()
                ShutdownCompleted
            | Distribute (key, message, reply) ->
                try
                    let agent = getAgent key
                    oneAdded key
                    let result = Agent.send message agent
                    reply result
                    MessageDistributed
                with
                    ex ->
                        let failResult = async { return failFn ex }
                        reply failResult
                        DistributionFailed
            | CompleteRun key ->
                oneFinished key
                AgentStatsUpdated

        let mailbox =
            startAgent
                <| canceller.Token
                <| fun inbox -> // agent boilerplate
                    let rec loop () =
                        async {
                            let! op = inbox.Receive ()
                            match runTurn op with
                            | ShutdownCompleted -> return () // exit
                            | MessageDistributed
                            | DistributionFailed
                            | AgentStatsUpdated -> return! loop () // continue
                        }
                    loop () // start the message processing loop

        {
            AgentsByKey = agentsByKey;
            Canceller = canceller;
            Mailbox = mailbox;
            HashFn = hashFn;
        }

    let send (m:'message) (d:AgentDistributor<'key, 'message, 'result>) =
        let key = d.HashFn m
        let distributionResultAsync = d.Mailbox.PostAndAsyncReply (fun channel -> Distribute (key, m, channel.Reply))
        async {
            let! distributionResult = distributionResultAsync // distribution complete
            let! runResult = distributionResult // agent result
            d.Mailbox.Post (CompleteRun key) // notify distributor of completed message
            return runResult // return result to caller
        }

    let stop (d:AgentDistributor<'key, 'message, 'result>) =
        d.Mailbox.PostAndAsyncReply (fun channel -> Shutdown channel.Reply)

    let stopNow (d:AgentDistributor<'key, 'message, 'result>) =
        d.AgentsByKey.Values
        |> Seq.iter (fun agentCounter -> Agent.stopNow agentCounter.Agent)
        d.Canceller.Cancel ()
        d.Mailbox.Post (Shutdown ignore)
        // must post the stop message to trigger the cancel check in case queue is empty

    let agentCount (d:AgentDistributor<'key, 'message, 'result>) =
        d.AgentsByKey.Count

    let messageCount (d:AgentDistributor<'key, 'message, 'result>) =
        d.AgentsByKey.Values
        |> Seq.map (fun x -> x.MessageCount)
        |> Seq.sum
        |> fun s -> s + d.Mailbox.CurrentQueueLength
