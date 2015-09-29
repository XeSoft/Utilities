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
        let dist = Agency.create runFn failFn hashFn
        let agentsMax = 10
        let max = 1000
        let resultsAsync = [| for i in 0 .. max - 1 do yield Agency.send { Id = i % agentsMax; Result = i } dist |]
        let messageCount = Agency.messageCount dist
        printfn "Distributor message queue count %i" messageCount
        Assert.IsTrue(1 <= messageCount && messageCount <= max)
        let agentCount = Agency.agentCount dist
        printfn "Distributor agent count %i" agentCount
        Assert.AreEqual(agentsMax, agentCount)
        resultsAsync
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.iteri (fun i x -> Assert.AreEqual(Some i, x))
        Agency.stop dist |> Async.RunSynchronously
        Assert.AreEqual(0, Agency.messageCount dist)
        Assert.AreEqual(0, Agency.agentCount dist)