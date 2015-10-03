namespace XeSoft.Utilities.Agents

open System.Threading

type private AgentOp<'message, 'result> =
| Stop of signalComplete:(unit -> unit)
| Process of message:'message * reply:('result -> unit)

// these are mainly events which affect memory profile or component status
type AgentEvent =
| AgentReceivedMessage
| AgentProcessedMessage
| AgentStopped

type Agent<'message, 'result> =
    private {
        Mailbox: MailboxProcessor<AgentOp<'message,'result>>;
        Canceller: CancellationTokenSource;
        StatsFn: AgentEvent -> unit;
    }

module Agent =

    /// Create an agent to process messages with the provided function.
    /// The processFn is performed on each submitted message in order.
    /// The failFn is called when the processFn throws an exception.
    /// The statsFn is called when events occur in the agent
    let createWithStats (processFn:'message -> 'result) (failFn:exn -> 'result) (statsFn:AgentEvent -> unit) =

        // forward composition tee
        let (>|>) f g x = g x; f x

        let startAgent t f = MailboxProcessor.Start (f, cancellationToken = t)
        let canceller = new System.Threading.CancellationTokenSource ()

        let run op =
            match op with
            | Stop signalComplete ->
                statsFn AgentStopped
                signalComplete ()
            | Process (message, reply) ->
                let result = try processFn message with ex -> failFn ex
                reply result
                statsFn AgentProcessedMessage

        let processResult op =
            match op with
            | Stop _ -> false
            | _ -> true

        let runTurn = processResult >|> run

        let mailbox =
            startAgent
            <| canceller.Token
            <| fun inbox -> // agent boilerplate
                let rec loop () =
                    async {
                        let! op = inbox.Receive ()
                        match runTurn op with
                        | false -> return () // exit
                        | true -> return! loop () // continue
                    }
                loop () // start the message processing loop

        { Mailbox = mailbox; Canceller = canceller; StatsFn = statsFn;}

    /// Create an agent to process messages with the provided function.
    /// The processFn is performed on each submitted message in order.
    /// The failFn is called when the processFn throws an exception.
    let create (processFn:'message -> 'result) (failFn:exn -> 'result) =
        createWithStats processFn failFn ignore

    /// Submit a message for an agent to process.
    /// Returns an async that will complete with the result when the message is processed.
    let send (m:'message) (a:Agent<'message,'result>) =
        a.StatsFn AgentReceivedMessage
        a.Mailbox.PostAndAsyncReply (fun channel -> Process (m, channel.Reply))

    /// Stop an agent after all remaining messages have been processed.
    /// Returns an async that will complete when all remaining messages have been processed.
    let stop (a:Agent<'message, 'result>) =
        a.Mailbox.PostAndAsyncReply (fun channel -> Stop channel.Reply)

    /// Stop an agent immediately.
    /// Any messages remaining in queue will not be processed.
    let stopNow (a:Agent<'message, 'result>) =
        a.Canceller.Cancel ()
        // must post the stop message to trigger the cancel check in case queue is empty
        a.Mailbox.Post (Stop ignore)
        a.StatsFn AgentStopped

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
