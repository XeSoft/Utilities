namespace XeSoft.Utilities.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open XeSoft.Utilities.Agents

type private DistributorTestMessage = {
    Id: int;
    Result: int;
}

type private Stats = {
    Delivered: int;
    Processed: int;
    Stopped: int;
    Received: int;
    Distributed: int;
    Commissioned: int;
    Decommissioned: int;
    Shutdown: int;
}

type private StatsMessage =
| StatsMessage of e:AgencyEvent
| GetStats of reply:(Stats -> unit)
| StopStats of reply:(Stats -> unit)

[<TestClass>]
type AgencyTests () =

    let runFn m = async { return Some m.Result }
    let runBadFn m = failwith "asdf"; runFn m
    let hashFn m = m.Id
    let failFn _ = None

    let between min max x =
        min <= x && x <= max

    let statsInbox =
        MailboxProcessor.Start
        <| fun inbox ->
            let rec loop stats =
                async {
                    let! e = inbox.Receive ()
                    match e with
                    | StopStats reply ->
                        reply stats
                        return ()
                    | GetStats reply ->
                        reply stats
                        return! loop stats
                    | StatsMessage e ->
                        let s =
                            match e with
                            | AgencyReceivedMessage -> {stats with Received = stats.Received + 1}
                            | AgencyDistributedMessage -> {stats with Distributed = stats.Distributed + 1}
                            | AgencyAgentEvent e ->
                                match e with
                                | AgentReceivedMessage -> {stats with Delivered = stats.Delivered + 1}
                                | AgentProcessedMessage -> {stats with Processed = stats.Processed + 1}
                                | AgentStopped -> {stats with Stopped = stats.Stopped + 1}
                            | AgencyCommissionedAgent -> {stats with Commissioned = stats.Commissioned + 1}
                            | AgencyDecommissionedAgent -> {stats with Decommissioned = stats.Decommissioned + 1}
                            | AgencyStopped -> {stats with Shutdown = stats.Shutdown + 1}
                        return! loop s
                }
            loop {
                Delivered = 0;
                Processed = 0;
                Stopped = 0;
                Received = 0;
                Distributed = 0;
                Commissioned = 0;
                Decommissioned = 0;
                Shutdown = 0;
            } // start the message processing loop

    let getStats () =
        statsInbox.PostAndReply (fun chan -> GetStats chan.Reply)

    let stopStats () =
        statsInbox.PostAndReply (fun chan -> StopStats chan.Reply)

    let statsFn e = statsInbox.Post (StatsMessage e)

    let printStats x stats =
        printfn "Agency stats after %s: delivered %i processed %i stopped %i received %i distributed %i commissioned %i decommissioned %i shutdown %i"
        <| x
        <| stats.Delivered
        <| stats.Processed
        <| stats.Stopped
        <| stats.Received
        <| stats.Distributed
        <| stats.Commissioned
        <| stats.Decommissioned
        <| stats.Shutdown

    [<TestMethod>]
    member __.``Agency - message actually gets processed`` () =
        let dist = Agency.create runFn failFn hashFn
        let expected = Some 5
        let actual = dist |> Agency.send {Id = 0; Result = 5} |> Async.RunSynchronously
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member __.``Agency - if processing fn throws then return fail value`` () =
        let dist = Agency.create runBadFn failFn hashFn
        let expected = None
        let actual = dist |> Agency.send {Id = 0; Result = 5} |> Async.RunSynchronously
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member __.``Agency - when processing many messages new agents spawned and messages queued`` () =
        let agentsMax = 10
        let max = 1000

        let agency = Agency.createWithStats runFn failFn hashFn statsFn
        let stats = getStats ()
        printStats "creation" stats
        Assert.AreEqual(0, stats.Delivered)
        Assert.AreEqual(0, stats.Processed)
        Assert.AreEqual(0, stats.Stopped)
        Assert.AreEqual(0, stats.Received)
        Assert.AreEqual(0, stats.Distributed)
        Assert.AreEqual(0, stats.Commissioned)
        Assert.AreEqual(0, stats.Decommissioned)
        Assert.AreEqual(0, stats.Shutdown)

        let resultsAsync = [| for i in 0 .. max - 1 do yield Agency.send { Id = i % agentsMax; Result = i } agency |]
        let stats = getStats ()
        printStats "sending" stats
        Assert.IsTrue(between 0 max stats.Delivered)
        Assert.IsTrue(between 0 max stats.Processed)
        Assert.AreEqual(0, stats.Stopped)
        Assert.AreEqual(max, stats.Received)
        Assert.IsTrue(between 0 max stats.Distributed)
        Assert.IsTrue(between 0 agentsMax stats.Commissioned)
        Assert.AreEqual(0, stats.Decommissioned)
        Assert.AreEqual(0, stats.Shutdown)

        resultsAsync
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.iteri (fun i x -> Assert.AreEqual(Some i, x))
        let stats = getStats ()
        printStats "processing" stats
        Assert.AreEqual(max, stats.Delivered)
        Assert.AreEqual(max, stats.Processed)
        Assert.AreEqual(0, stats.Stopped)
        Assert.AreEqual(max, stats.Received)
        Assert.AreEqual(max, stats.Distributed)
        Assert.AreEqual(agentsMax, stats.Commissioned)
        Assert.AreEqual(0, stats.Decommissioned)
        Assert.AreEqual(0, stats.Shutdown)

        Agency.stop agency |> Async.RunSynchronously
        let stats = stopStats ()
        printStats "stopped" stats
        Assert.AreEqual(max, stats.Delivered)
        Assert.AreEqual(max, stats.Processed)
        Assert.AreEqual(agentsMax, stats.Stopped)
        Assert.AreEqual(max, stats.Received)
        Assert.AreEqual(max, stats.Distributed)
        Assert.AreEqual(agentsMax, stats.Commissioned)
        Assert.AreEqual(agentsMax, stats.Decommissioned)
        Assert.AreEqual(1, stats.Shutdown)

