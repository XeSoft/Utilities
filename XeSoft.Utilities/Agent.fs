namespace XeSoft.Utilities.Agents

open System.Threading

type private AgentOp<'message, 'result> =
| Stop of signalComplete:(unit -> unit)
| Process of message:'message * reply:('result -> unit)

type private AgentOpResult =
| Processed
| Stopped

type AgentStatistics = {
    QueueSize: int;
    PeakQueueSize: int;
    Processed: int64;
}

type private AgentStatsEvent =
| StatsQueried of deliver:(AgentStatistics -> unit)
| MessageReceived
| MessageProcessed
| StatsStopped

type private AgentStats = 
    {
        mutable QueueSize : int;
        mutable PeakQueueSize : int;
        mutable Processed : int64;
    }
    with
        static member Create () =
            { QueueSize = 0; PeakQueueSize = 0; Processed = 0L; }
        member x.ToReadOnly () =
            { AgentStatistics.QueueSize = x.QueueSize; PeakQueueSize = x.PeakQueueSize; Processed = x.Processed; }

type Agent<'message, 'result> =
    private {
        Mailbox: MailboxProcessor<AgentOp<'message,'result>>;
        Canceller: CancellationTokenSource;
        Stats: MailboxProcessor<AgentStatsEvent>;
        FinalStats: AgentStatistics ref; 
    }

module Agent =

    /// Create an agent to process messages with the provided function.
    /// The processFn is performed on each submitted message in order.
    /// The failFn is called when the processFn throws an exception.
    let create (processFn:'message -> Async<'result>) (failFn:exn -> 'result) =

        let startAgent t f = MailboxProcessor.Start (f, cancellationToken = t)
        let canceller = new System.Threading.CancellationTokenSource ()

        let stats = AgentStats.Create ()
        let finalStats = ref (stats.ToReadOnly())

        let runTurn op =
            match op with
            | Stop signalComplete ->
                signalComplete ()
                async { return Stopped }
            | Process (message, reply) ->
                async {
                    let! result = try processFn message with ex -> async { return failFn ex }
                    reply result
                    return Processed
                }

        let updateStats op =
            match op with
            | StatsStopped ->
                finalStats := stats.ToReadOnly()
            | StatsQueried deliver -> 
                deliver (stats.ToReadOnly())
            | MessageReceived -> 
                stats.QueueSize <- stats.QueueSize + 1
                if stats.QueueSize > stats.PeakQueueSize then
                    stats.PeakQueueSize <- stats.QueueSize
            | MessageProcessed ->
                stats.QueueSize <- stats.QueueSize - 1
                stats.Processed <- stats.Processed + 1L
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

        let mailbox =
            startAgent
            <| canceller.Token
            <| fun inbox -> // agent boilerplate
                let rec loop () =
                    async {
                        let! op = inbox.Receive ()
                        let! result = runTurn op
                        match result with
                        | Stopped ->
                            statsInbox.Post (StatsStopped)
                            return () // exit
                        | Processed ->
                            statsInbox.Post(MessageProcessed)
                            return! loop () // continue
                    }
                loop () // start the message processing loop

        { Mailbox = mailbox; Canceller = canceller; Stats = statsInbox; FinalStats = finalStats;}

    /// Submit a message for an agent to process.
    /// Returns an async that will complete with the result when the message is processed.
    let send (m:'message) (a:Agent<'message,'result>) =
        a.Stats.Post (MessageReceived)
        a.Mailbox.PostAndAsyncReply (fun channel -> Process (m, channel.Reply))

    /// Stop an agent after all remaining messages have been processed.
    /// Returns an async that will complete when all remaining messages have been processed.
    let stop (a:Agent<'message, 'result>) =
        a.Mailbox.PostAndAsyncReply (fun channel -> Stop channel.Reply)

    /// Stop an agent immediately.
    /// Any messages remaining in queue will not be processed.
    let stopNow (a:Agent<'message, 'result>) =
        a.Stats.Post (StatsStopped)
        a.Canceller.Cancel ()
        a.Mailbox.Post (Stop ignore)
        // must post the stop message to trigger the cancel check in case queue is empty

    /// Get the agent statistics.
    let stats (a:Agent<'message, 'result>) =
        if a.Canceller.IsCancellationRequested then
            !a.FinalStats
        else
            a.Stats.PostAndReply (fun channel -> StatsQueried channel.Reply)

// convenience methods
type Agent<'message, 'result> with
    /// Submit a message for processing.
    /// Returns an async that will complete with the result when the message is processed.
    member me.Send m = Agent.send m me
    /// Stop the agent after all remaining messages have been processed.
    /// Returns an async that will complete when all remaining messages have been processed.
    member me.Stop () = Agent.stop me
    /// Stop the agent immediately.
    /// Any messages remaining in queue will not be processed.
    member me.StopNow () = Agent.stopNow me
    /// Get the agent statistics.
    member me.GetStats () = Agent.stats me