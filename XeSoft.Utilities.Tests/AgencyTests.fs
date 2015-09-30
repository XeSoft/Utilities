namespace XeSoft.Utilities.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open XeSoft.Utilities
open XeSoft.Utilities.Agents

type private DistributorTestMessage = {
    Id: int;
    Result: int;
}

[<TestClass>]
type AgencyTests () =

    let runFn m = Async.retn (Some m.Result)
    let runBadFn m = failwith "asdf"; runFn m
    let hashFn m = m.Id
    let failFn _ = None

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
        let agency = Agency.create runFn failFn hashFn
        let stats = Agency.stats agency
        printfn "Agency stats after creation: %A" stats
        Assert.AreEqual(0, stats.AgentCount)
        Assert.AreEqual(0, stats.PeakAgentCount)
        Assert.AreEqual(0, stats.QueueSize)
        Assert.AreEqual(0, stats.PeakQueueSize)
        Assert.AreEqual(0L, stats.Processed)
        let resultsAsync = [| for i in 0 .. max - 1 do yield Agency.send { Id = i % agentsMax; Result = i } agency |]
        let stats = Agency.stats agency
        printfn "Agency stats after sending: %A" stats
        Assert.AreEqual(agentsMax, stats.AgentCount)
        Assert.AreEqual(agentsMax, stats.PeakAgentCount)
        Assert.IsTrue(1 <= stats.QueueSize && stats.QueueSize <= max)
        Assert.IsTrue(1 <= stats.PeakQueueSize && stats.PeakQueueSize <= max)
        Assert.IsTrue(0L <= stats.Processed && stats.Processed <= int64 max)
        Assert.AreEqual(int64 max, int64 stats.QueueSize + stats.Processed)
        resultsAsync
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.iteri (fun i x -> Assert.AreEqual(Some i, x))
        Agency.stop agency |> Async.RunSynchronously
        let stats = Agency.stats agency
        printfn "Agency stats after processing: %A" stats
        Assert.AreEqual(0, stats.AgentCount)
        Assert.AreEqual(agentsMax, stats.PeakAgentCount)
        Assert.AreEqual(0, stats.QueueSize)
        Assert.IsTrue(1 <= stats.PeakQueueSize && stats.PeakQueueSize <= max)
        Assert.AreEqual(int64 max, stats.Processed)
