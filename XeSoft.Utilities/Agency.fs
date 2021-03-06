﻿namespace XeSoft.Utilities.Agents

open System.Threading

type private AgencyOp<'key, 'message, 'result when 'key : equality> =
| Distribute of 'key * 'message * reply:('result -> unit)
| CompleteRun of 'key
| Shutdown of signalComplete:(unit -> unit)

type private AgentTracker<'message, 'result> = {
    mutable MessageCount: int;
    Agent: Agent<'message, 'result>;
}

// these are mainly events which affect memory profile or component status
type AgencyEvent =
| AgencyReceivedMessage
| AgencyDistributedMessage
| AgencyAgentEvent of AgentEvent
| AgencyCommissionedAgent
| AgencyDecommissionedAgent
| AgencyStopped

type Agency<'key, 'message, 'result when 'key : equality> =
    private {
        Canceller: CancellationTokenSource;
        Mailbox: MailboxProcessor<AgencyOp<'key, 'message, 'result>>;
        HashFn: 'message -> 'key;
        StatsFn: AgencyEvent -> unit;
    }

module Agency =
    
    /// Create an agent distributor to process messages.
    /// Messages are first run through the hashFn to decide which agent will process them.
    /// Each message to the same agent (hash value) will be processed in order.
    /// processFn will be run on each submitted message in order.
    /// failFn will be called if processFn throws an exception.
    /// hashFn will be used do decide which agent should process the message.
    /// statsFn will be called whenever an agency event occurs
    let createWithStats (processFn:'message -> Async<'result>) (failFn: exn -> 'result) (hashFn: 'message -> 'key) (statsFn:AgencyEvent -> unit) =

        // forward composition tee
        let (>|>) f g x = g x; f x

        let startAgent t f = MailboxProcessor.Start (f, cancellationToken = t)
        let canceller = new CancellationTokenSource ()
    
        let trackersByKey = new System.Collections.Generic.Dictionary<'key, AgentTracker<_,_>> ()

        let agentStatsFn e =
            statsFn (AgencyAgentEvent e)

        let trackerExistsFor key =
            trackersByKey.ContainsKey key

        let commissionTrackerWith key =
            let agent = Agent.createWithStats processFn failFn agentStatsFn
            let tracker = { MessageCount = 0; Agent = agent }
            trackersByKey.Add (key, tracker)
            tracker

        let decommission key tracker =
            Agent.stopNow tracker.Agent
            trackersByKey.Remove key |> ignore

        let getTrackerFor key =
            trackersByKey.[key]

        let run (mailbox:MailboxProcessor<_>) op =
            match op with
            | Shutdown signalComplete ->
                trackersByKey.Values
                |> Seq.map (fun tracker -> Agent.stop tracker.Agent)
                |> Async.Parallel
                |> Async.Ignore
                |> Async.RunSynchronously
                statsFn AgencyStopped
                signalComplete ()
            | Distribute (key, message, reply) ->
                let tracker =
                    if trackerExistsFor key
                    then getTrackerFor key
                    else statsFn AgencyCommissionedAgent
                         commissionTrackerWith key
                tracker.MessageCount <- tracker.MessageCount + 1
                // deliver message to agent
                let resultAsync = Agent.send message tracker.Agent
                statsFn AgencyDistributedMessage
                // setup notification and delivery when agent completes work
                async {
                    let! result = resultAsync // get result
                    mailbox.Post (CompleteRun key) // notify distributor of completed message
                    reply result // deliver result
                } |> Async.Start
            | CompleteRun key ->
                let tracker = getTrackerFor key
                tracker.MessageCount <- tracker.MessageCount - 1
                // remove if no more messages
                if tracker.MessageCount = 0 then
                    decommission key tracker
                    statsFn AgencyDecommissionedAgent

        let processRequest =
            function
            | Shutdown _ -> false
            | _ -> true

        let stopAllAgentsNow () =
            trackersByKey.Values
            |> Seq.iter (fun tracker -> Agent.stopNow tracker.Agent)

        let mailbox =
            startAgent
            <| canceller.Token
            <| fun inbox ->
                let runTurn  = processRequest >|> run inbox
                let rec loop () =
                    async {
                        use! holder = Async.OnCancel stopAllAgentsNow
                        let! op = inbox.Receive ()
                        match runTurn op with
                        | false -> return () // exit
                        | true -> return! loop () // continue
                    }
                loop ()

        {
            Canceller = canceller;
            Mailbox = mailbox;
            HashFn = hashFn;
            StatsFn = statsFn;
        }

    /// Create an agent distributor to process messages.
    /// Messages are first run through the hashFn to decide which agent will process them.
    /// Each message to the same agent (hash value) will be processed in order.
    /// processFn will be run on each submitted message in order.
    /// failFn will be called if processFn throws an exception.
    /// hashFn will be used do decide which agent should process the message.
    let create (processFn:'message -> Async<'result>) (failFn: exn -> 'result) (hashFn: 'message -> 'key) =
        createWithStats processFn failFn hashFn ignore

    /// Send a message to a distributor for processing.
    /// Returns an async that will complete with the result when the message is processed.
    let send (m:'message) (d:Agency<'key, 'message, 'result>) =
        let key = d.HashFn m
        d.StatsFn AgencyReceivedMessage
        d.Mailbox.PostAndAsyncReply (fun channel -> Distribute (key, m, channel.Reply))

    /// Queue a message to a distributor for processing.
    let queue (m:'message) (d:Agency<'key, 'message, 'result>) =
        let key = d.HashFn m
        d.StatsFn AgencyReceivedMessage
        d.Mailbox.Post (Distribute (key, m, ignore))

    /// Stop a distributor after all remaining messages have been processed.
    /// Returns an async that will complete when all remaining messages have been processed.
    let stop (d:Agency<'key, 'message, 'result>) =
        d.Mailbox.PostAndAsyncReply (fun channel -> Shutdown channel.Reply)

    /// Stop a distributor immediately.
    /// Any messages remaining in queue will not be processed.
    let stopNow (d:Agency<'key, 'message, 'result>) =
        d.StatsFn AgencyStopped
        d.Canceller.Cancel ()
        // must post message to trigger the cancel check in case queue is empty
        d.Mailbox.Post (Shutdown ignore)

// convenience methods
type Agency<'key, 'message, 'result when 'key : equality> with
    /// Send a message for processing.
    /// Returns an async that will complete with the result when the message is processed.
    member me.Send m = Agency.send m me
    /// Queue a message for processing.
    member me.Queue m = Agency.queue m me
    /// Stop the distributor after all remaining messages have been processed.
    /// Returns an async that will complete when all remaining messages have been processed.
    member me.Stop () = Agency.stop me
    /// Stop the distributor immediately.
    /// Any messages remaining in queue will not be processed.
    member me.StopNow () = Agency.stopNow me
