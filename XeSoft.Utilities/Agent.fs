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

    let send (m:'message) (a:Agent<'message,'result>) =
        a.Mailbox.PostAndAsyncReply (fun channel -> Process (m, channel.Reply))

    let stop (a:Agent<'message, 'result>) =
        a.Mailbox.PostAndAsyncReply (fun channel -> Stop channel.Reply)

    let stopNow (a:Agent<'message, 'result>) =
        a.Canceller.Cancel ()
        a.Mailbox.Post (Stop ignore)
        // must post the stop message to trigger the cancel check in case queue is empty

    let messageCount (a:Agent<'message, 'result>) =
        a.Mailbox.CurrentQueueLength