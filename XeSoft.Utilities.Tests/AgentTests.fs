namespace XeSoft.Utilities.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open XeSoft.Utilities.Agents

type private AgentTestMessage = {
    Result: int;
}

[<TestClass>]
type AgentTests () =

    let runFn m = async { return Some m.Result }
    let runBadFn m = failwith "asdf"; runFn m
    let failFn _ = None

    let mutable received = 0;
    let mutable processed = 0;
    let mutable stopped = false;

    let statsFn =
        function
        | AgentReceivedMessage -> received <- received + 1
        | AgentProcessedMessage -> processed <- processed + 1
        | AgentStopped -> stopped <- true

    let printStats x =
        printf "Agent stats after %s:" x
        printf " received %i" received
        printf " processed %i" processed
        printfn " stopped %b" stopped

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
        let max = 1000
        let agent = Agent.createWithStats runFn failFn statsFn
        printStats "creation"
        Assert.AreEqual(0, received)
        Assert.AreEqual(0, processed)
        Assert.AreEqual(false, stopped)
        let resultsAsync = [|  for i in 0 .. max - 1 do yield Agent.send { Result = i } agent |]
        printStats "sending"
        Assert.AreEqual(max, received)
        Assert.IsTrue(0 <= processed && processed < max)
        Assert.AreEqual(false, stopped)
        resultsAsync
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.iteri (fun i x -> Assert.AreEqual(Some i, x))
        printStats "processed"
        Assert.AreEqual(max, received)
        Assert.AreEqual(max, processed)
        Assert.AreEqual(false, stopped)
        Agent.stop agent |> Async.RunSynchronously
        printStats "stopped"
        Assert.AreEqual(max, received)
        Assert.AreEqual(max, processed)
        Assert.AreEqual(true, stopped)
