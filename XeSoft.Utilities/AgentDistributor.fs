namespace XeSoft.Utilities.Agents

open System.Threading

type private DistributorOp<'key, 'message, 'result when 'key : comparison> =
| Distribute of 'key * 'message * reply:('result -> unit)
| CompleteRun of 'key
| GetAgentCount of reply:(int -> unit)
| Shutdown of signalComplete:(unit -> unit)

type private AgentCounter<'message, 'result> = {
    mutable MessageCount: int;
    Agent: Agent<'message, 'result>;
}

type private DistributorResult<'result> =
| QueryAnswered
| MessageDistributed
| DistributionFailed
| AgentStatsUpdated
| ShutdownCompleted

type AgentDistributor<'key, 'message, 'result when 'key : comparison> =
    private {
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
                canceller.Cancel () // call cancel to signal stopped
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
            | GetAgentCount reply ->
                reply agentsByKey.Count
                QueryAnswered

        let stopAllAgentsNow () =
            agentsByKey.Values
            |> Array.ofSeq
            |> Array.iter (fun agentCounter -> Agent.stopNow agentCounter.Agent)

        let mailbox =
            startAgent
                <| canceller.Token
                <| fun inbox -> // agent boilerplate
                    let rec loop () =
                        async {
                            use! holder = Async.OnCancel stopAllAgentsNow
                            let! op = inbox.Receive ()
                            match runTurn op with
                            | ShutdownCompleted -> return () // exit
                            | QueryAnswered
                            | MessageDistributed
                            | DistributionFailed
                            | AgentStatsUpdated -> return! loop () // continue
                        }
                    loop () // start the message processing loop

        {
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
        d.Canceller.Cancel ()
        d.Mailbox.Post (Shutdown ignore)
        // must post message to trigger the cancel check in case queue is empty

    let agentCount (d:AgentDistributor<'key, 'message, 'result>) =
        if d.Canceller.IsCancellationRequested then 0
        else
            d.Mailbox.PostAndAsyncReply (fun channel -> GetAgentCount channel.Reply)
            |> Async.RunSynchronously

    let messageCount (d:AgentDistributor<'key, 'message, 'result>) =
        d.Mailbox.CurrentQueueLength
