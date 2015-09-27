namespace XeSoft.Utilities.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open XeSoft.Utilities
open XeSoft.Utilities.Agents

type private DistributorTestMessage = {
    Id: int;
    Result: int;
}

[<TestClass>]
type AgentDistributorTests () =

    let runFn m = Async.retn (Some m.Result)
    let runBadFn m = failwith "asdf"; runFn m
    let hashFn m = m.Id
    let failFn _ = None

    [<TestMethod>]
    member __.``AgentDistributor - message actually gets processed`` () =
        let dist = AgentDistributor.create runFn failFn hashFn
        let expected = Some 5
        let actual = dist |> AgentDistributor.send {Id = 0; Result = 5} |> Async.RunSynchronously
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member __.``AgentDistributor - if processing fn throws then return fail value`` () =
        let dist = AgentDistributor.create runBadFn failFn hashFn
        let expected = None
        let actual = dist |> AgentDistributor.send {Id = 0; Result = 5} |> Async.RunSynchronously
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member __.``AgentDistributor - when processing many messages new agents spawned and messages queued`` () =
        let dist = AgentDistributor.create runFn failFn hashFn
        let agentsMax = 10
        let max = 1000
        let resultsAsync = [| for i in 0 .. max - 1 do yield AgentDistributor.send { Id = i % agentsMax; Result = i } dist |]
        let messageCount = AgentDistributor.messageCount dist
        printfn "Distributor message queue count %i" messageCount
        Assert.IsTrue(1 <= messageCount && messageCount <= max)
        Async.Sleep 10 |> Async.RunSynchronously // give agents a chance to start processing
        let agentCount = AgentDistributor.agentCount dist
        printfn "Distributor agent count %i" agentCount
        Assert.AreEqual(agentsMax, agentCount)
        resultsAsync
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.iteri (fun i x -> Assert.AreEqual(Some i, x))
        Async.Sleep 10 |> Async.RunSynchronously // give counters a chance to update
        Assert.AreEqual(0, AgentDistributor.messageCount dist)
        Assert.AreEqual(0, AgentDistributor.agentCount dist)