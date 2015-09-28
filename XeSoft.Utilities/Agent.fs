namespace XeSoft.Utilities.Agents

open System.Threading

type private AgentOp<'message, 'result> =
| Stop of signalComplete:(unit -> unit)
| Process of message:'message * reply:('result -> unit)

type private AgentOpResult =
| Processed
| Stopped

type Agent<'message, 'result> =
    private {
        Mailbox: MailboxProcessor<AgentOp<'message,'result>>;
        Canceller: CancellationTokenSource;
    }

module Agent =

    /// Create an agent to process messages with the provided function.
    /// The processFn is performed on each submitted message in order.
    /// The failFn is called when the processFn throws an exception.
    let create (processFn:'message -> Async<'result>) (failFn:exn -> 'result) =

        let startAgent t f = MailboxProcessor.Start (f, cancellationToken = t)
        let canceller = new System.Threading.CancellationTokenSource ()

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

        let mailbox =
            startAgent
                <| canceller.Token
                <| fun inbox -> // agent boilerplate
                    let rec loop () =
                        async {
                            let! op = inbox.Receive ()
                            let! result = runTurn op
                            match result with
                            | Stopped -> return () // exit
                            | Processed -> return! loop () // continue
                        }
                    loop () // start the message processing loop

        { Mailbox = mailbox; Canceller = canceller;}

    /// Submit a message for the agent to process.
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

    /// Get the count of messages waiting in queue on the agent.
    let messageCount (a:Agent<'message, 'result>) =
        a.Mailbox.CurrentQueueLength
