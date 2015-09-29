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

[<Sealed>]
type private AgentStats =
    val mutable QueueSize : int;
    val mutable PeakQueueSize : int;
    val mutable Processed : int64;
    new () =
        { QueueSize = 0; PeakQueueSize = 0; Processed = 0L }
    member me.ToStatistics () =
        { AgentStatistics.QueueSize = me.QueueSize; PeakQueueSize = me.PeakQueueSize; Processed = me.Processed; }

type Agent<'message, 'result> =
    private {
        Mailbox: MailboxProcessor<AgentOp<'message,'result>>;
        Canceller: CancellationTokenSource;
        Stats: AgentStats;
    }

module Agent =

    /// Create an agent to process messages with the provided function.
    /// The processFn is performed on each submitted message in order.
    /// The failFn is called when the processFn throws an exception.
    let create (processFn:'message -> Async<'result>) (failFn:exn -> 'result) =

        let startAgent t f = MailboxProcessor.Start (f, cancellationToken = t)
        let canceller = new System.Threading.CancellationTokenSource ()

        let stats = new AgentStats ()

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

        let updateStats queueSize =
            stats.QueueSize <- queueSize
            if stats.QueueSize > stats.PeakQueueSize then
                stats.PeakQueueSize <- stats.QueueSize
            stats.Processed <- stats.Processed + 1L

        let mailbox =
            startAgent
                <| canceller.Token
                <| fun inbox -> // agent boilerplate
                    let rec loop () =
                        async {
                            updateStats inbox.CurrentQueueLength
                            let! op = inbox.Receive ()
                            let! result = runTurn op
                            match result with
                            | Stopped -> return () // exit
                            | Processed -> return! loop () // continue
                        }
                    stats.Processed <- -1L
                    loop () // start the message processing loop

        { Mailbox = mailbox; Canceller = canceller; Stats = stats;}

    /// Submit a message for an agent to process.
    /// Returns an async that will complete with the result when the message is processed.
    let send (m:'message) (a:Agent<'message,'result>) =
        a.Mailbox.PostAndAsyncReply (fun channel -> Process (m, channel.Reply))

    /// Stop an agent after all remaining messages have been processed.
    /// Returns an async that will complete when all remaining messages have been processed.
    let stop (a:Agent<'message, 'result>) =
        a.Mailbox.PostAndAsyncReply (fun channel -> Stop channel.Reply)

    /// Stop an agent immediately.
    /// Any messages remaining in queue will not be processed.
    let stopNow (a:Agent<'message, 'result>) =
        a.Canceller.Cancel ()
        a.Mailbox.Post (Stop ignore)
        // must post the stop message to trigger the cancel check in case queue is empty

    /// Get the agent statistics.
    let stats (a:Agent<'message, 'result>) =
        a.Stats.ToStatistics ()

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