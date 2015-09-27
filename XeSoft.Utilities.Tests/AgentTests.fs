namespace XeSoft.Utilities.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open XeSoft.Utilities
open XeSoft.Utilities.Agents

type private AgentTestMessage = {
    Result: int;
}

[<TestClass>]
type AgentTests () =

    let runFn m = Async.retn (Some m.Result)
    let runBadFn m = failwith "asdf"; runFn m
    let failFn _ = None

    [<TestMethod>]
    member __.``Agent - message actually gets processed`` () =
        let agent = Agent.create runFn failFn
        let expected = Some 5
        let actual = agent |> Agent.send {Result = 5} |> Async.RunSynchronously
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member __.``Agent - if processing fn throws then return fail value`` () =
        let agent = Agent.create runBadFn failFn
        let expected = None
        let actual = agent |> Agent.send {Result = 5} |> Async.RunSynchronously
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member __.``Agent - when processing many messages some get queued`` () =
        let agent = Agent.create runFn failFn
        let max = 1000
        let resultsAsync = [|  for i in 0 .. max - 1 do yield Agent.send { Result = i } agent |]
        let count = Agent.messageCount agent
        printfn "Agent queue count: %i" count
        Assert.IsTrue(1 <= count && count <= max)
        resultsAsync
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.iteri (fun i x -> Assert.AreEqual(Some i, x))
        Assert.AreEqual(0, Agent.messageCount agent)