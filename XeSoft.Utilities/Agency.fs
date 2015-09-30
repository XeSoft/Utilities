namespace XeSoft.Utilities.Agents

open System.Threading

type private AgencyOp<'key, 'message, 'result when 'key : equality> =
| Distribute of 'key * 'message * reply:('result -> unit)
| CompleteRun of 'key
| Shutdown of signalComplete:(unit -> unit)

type private AgentCounter<'message, 'result> = {
    mutable MessageCount: int;
    Agent: Agent<'message, 'result>;
}

type private AgencyOpResult<'result> =
| MessageDistributed
| DistributionFailed
| AgentStatsUpdated
| ShutdownCompleted

type AgencyStatistics = {
    AgentCount: int;
    PeakAgentCount: int;
    QueueSize: int;
    PeakQueueSize: int;
    Processed: int64;
}

type private AgencyStatsEvent =
| StatsQueried of deliver:(AgencyStatistics -> unit)
| MessageReceived
| MessageProcessed
| AgentCommissioned
| AgentDecommissioned
| StatsStopped

type private AgencyStats = 
    private {
        mutable AgentCount : int;
        mutable PeakAgentCount: int;
        mutable QueueSize : int;
        mutable PeakQueueSize : int;
        mutable Processed : int64;
    }
    with
        static member Create () =
            { AgentCount = 0; PeakAgentCount = 0; QueueSize = 0; PeakQueueSize = 0; Processed = 0L; }

        member me.ToReadOnly () =
            { AgencyStatistics.AgentCount = me.AgentCount; PeakAgentCount = me.PeakAgentCount; QueueSize = me.QueueSize; PeakQueueSize = me.PeakQueueSize; Processed = me.Processed; }

type Agency<'key, 'message, 'result when 'key : equality> =
    private {
        Canceller: CancellationTokenSource;
        Mailbox: MailboxProcessor<AgencyOp<'key, 'message, Async<'result>>>;
        HashFn: 'message -> 'key;
        Stats: MailboxProcessor<AgencyStatsEvent>;
        FinalStats: AgencyStatistics ref;
    }

module Agency =
    
    /// Create an agent distributor to process messages.
    /// Messages are first run through the hashFn to decide which agent will process them.
    /// Each message to the same agent (hash value) will be processed in order.
    /// processFn will be run on each submitted message in order.
    /// failFn will be called if processFn thrown an exception.
    /// hashFn will be used do decide which agent should process the message.
    let create (processFn:'message -> Async<'result>) (failFn: exn -> 'result) (hashFn: 'message -> 'key) =

        let startAgent t f = MailboxProcessor.Start (f, cancellationToken = t)
        let canceller = new CancellationTokenSource ()
    
        let agentsByKey = new System.Collections.Generic.Dictionary<'key, AgentCounter<_,_>> ()

        let stats = AgencyStats.Create ()
        let finalStats = ref (stats.ToReadOnly())

        let updateStats op =
            match op with
            | StatsQueried deliver ->
                deliver(stats.ToReadOnly())
            | StatsStopped ->
                finalStats := stats.ToReadOnly()
            | MessageReceived ->
                stats.QueueSize <- stats.QueueSize + 1
                if stats.QueueSize > stats.PeakQueueSize then
                    stats.PeakQueueSize <- stats.QueueSize
            | MessageProcessed ->
                stats.QueueSize <- stats.QueueSize - 1
                stats.Processed <- stats.Processed + 1L
            | AgentCommissioned ->
                stats.AgentCount <- stats.AgentCount + 1
                if stats.AgentCount > stats.PeakAgentCount then
                    stats.PeakAgentCount <- stats.AgentCount
            | AgentDecommissioned ->
                stats.AgentCount <- stats.AgentCount - 1
            match op with
            | StatsStopped -> false
            | _ -> true

        let statsInbox =
            MailboxProcessor.Start
            <| fun inbox ->
                let rec loop () =
                    async {
                        let! op = inbox.Receive ()
                        let keepGoing = updateStats op
                        match keepGoing with
                        | false -> return ()
                        | true -> return! loop ()
                    }
                loop ()

        let getAgentCounter key =
            match agentsByKey.TryGetValue key with
            | (true, agentCounter) -> agentCounter
            | _ -> // new agent
                let agent = Agent.create processFn failFn
                let agentCounter = { MessageCount = 0; Agent = agent }
                agentsByKey.Add (key, agentCounter)
                statsInbox.Post AgentCommissioned
                agentCounter

        let getAgent key = (getAgentCounter key).Agent

        let oneAdded key = 
            let counter = getAgentCounter key
            counter.MessageCount <- counter.MessageCount + 1

        let oneFinished key =
            let counter = getAgentCounter key
            counter.MessageCount <- counter.MessageCount - 1
            statsInbox.Post MessageProcessed
            // remove if no more messages
            if counter.MessageCount = 0 then
                Agent.stopNow counter.Agent
                agentsByKey.Remove key |> ignore
                statsInbox.Post AgentDecommissioned

        let runTurn op =
            match op with
            | Shutdown signalComplete ->
                agentsByKey.Values
                |> Seq.map (fun agentCounter -> Agent.stop agentCounter.Agent)
                |> Async.Parallel
                |> Async.Ignore
                |> Async.RunSynchronously
                statsInbox.Post StatsStopped
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
                            | MessageDistributed
                            | DistributionFailed
                            | AgentStatsUpdated -> return! loop () // continue
                        }
                    loop () // start the message processing loop

        {
            Canceller = canceller;
            Mailbox = mailbox;
            HashFn = hashFn;
            Stats = statsInbox;
            FinalStats = finalStats;
        }

    /// Submit a message to a distributor for processing.
    /// Returns an async that will complete with the result when the message is processed.
    let send (m:'message) (d:Agency<'key, 'message, 'result>) =
        let key = d.HashFn m
        d.Stats.Post MessageReceived
        let distributionResultAsync = d.Mailbox.PostAndAsyncReply (fun channel -> Distribute (key, m, channel.Reply))
        async {
            let! distributionResult = distributionResultAsync // distribution complete
            let! runResult = distributionResult // agent result
            d.Mailbox.Post (CompleteRun key) // notify distributor of completed message
            return runResult // return result to caller
        }

    /// Stop a distributor after all remaining messages have been processed.
    /// Returns an async that will complete when all remaining messages have been processed.
    let stop (d:Agency<'key, 'message, 'result>) =
        d.Mailbox.PostAndAsyncReply (fun channel -> Shutdown channel.Reply)

    /// Stop a distributor immediately.
    /// Any messages remaining in queue will not be processed.
    let stopNow (d:Agency<'key, 'message, 'result>) =
        d.Canceller.Cancel ()
        d.Mailbox.Post (Shutdown ignore)
        // must post message to trigger the cancel check in case queue is empty

    /// Returns the stats for the distributor.
    let stats (d:Agency<'key, 'message, 'result>) =
        if d.Canceller.IsCancellationRequested then
            !d.FinalStats
        else
            d.Stats.PostAndReply (fun channel -> StatsQueried channel.Reply)

// convenience methods
type Agency<'key, 'message, 'result when 'key : equality> with
    /// Submit a message for processing.
    /// Returns an async that will complete with the result when the message is processed.
    member me.Send m = Agency.send m me
    /// Stop the distributor after all remaining messages have been processed.
    /// Returns an async that will complete when all remaining messages have been processed.
    member me.Stop () = Agency.stop me
    /// Stop the distributor immediately.
    /// Any messages remaining in queue will not be processed.
    member me.StopNow () = Agency.stopNow me
    /// Returns the stats for the distributor.
    member me.GetStats () = Agency.stats me
