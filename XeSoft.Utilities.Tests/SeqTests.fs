namespace XeSoft.Utilities.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open XeSoft.Utilities

[<TestClass>]
type SeqTests () =

    let orig = ['a';'b';'c']
    let stats =
        new System.Collections.Generic.Dictionary<string,int> (
            dict [
                ("abc", 0);
                ("acb", 0);
                ("bac", 0);
                ("bca", 0);
                ("cab", 0);
                ("cba", 0);
            ])

    [<TestMethod>]
    member __.``Seq shuffle 3 item array when rng always returns 0`` () =
        // a, b, c* -> c, b, a
        // c, b*, a -> b, c, a
        let r = orig |> Seq.shuffle (fun _ -> 0) |> List.ofSeq
        Assert.AreEqual(orig.[0], r.[2])
        Assert.AreEqual(orig.[1], r.[0])
        Assert.AreEqual(orig.[2], r.[1])

    [<TestMethod>]
    member __.``Seq shuffle 3 item array when rng always returns max`` () =
        let r = orig |> Seq.shuffle (fun i -> i) |> List.ofSeq
        Assert.AreEqual(orig.[0], r.[0])
        Assert.AreEqual(orig.[1], r.[1])
        Assert.AreEqual(orig.[2], r.[2])

    [<TestMethod>]
    member __.``Seq shuffle 3 item array when rng returns 1 if possible`` () =
        let r = orig |> Seq.shuffle (fun i -> if i > 0 then 1 else 0) |> List.ofSeq
        Assert.AreEqual(orig.[0], r.[0])
        Assert.AreEqual(orig.[1], r.[2])
        Assert.AreEqual(orig.[2], r.[1])

    [<TestMethod>]
    member __.``Seq shuffle 3 item array when rng returns 0 then 1`` () =
        let r = orig |> Seq.shuffle (fun i -> if i = 1 then 1 else 0) |> List.ofSeq
        Assert.AreEqual(orig.[0], r.[2])
        Assert.AreEqual(orig.[1], r.[1])
        Assert.AreEqual(orig.[2], r.[0])

    [<TestMethod>]
    member __.``Seq shuffleSeeded 3 item seq produces repeatable shuffles`` () =

        for i in 0 .. 7919 .. 79190 do
            let list1 = orig |> Seq.shuffleSeeded i |> List.ofSeq
            let list2 = orig |> Seq.shuffleSeeded i |> List.ofSeq
            Assert.AreEqual(list1, list2)

    [<TestMethod>]
    member __.``Seq shuffleFairly 3 item seq produces all permutations in 10000 tries`` () =
        let rec loop maxN maxFound n foundPermutations =
            if n = maxN || foundPermutations = maxFound then (n, foundPermutations)
            else
                // using list for equality
                let sequence = orig |> Seq.shuffleCrypto
                let key = String.Join(String.Empty, sequence)
                let stat = stats.[key]
                let found = if stat = 0 then foundPermutations + 1 else foundPermutations
                stats.[key] <- stat + 1
                loop maxN maxFound (n + 1) found
        let (iteration, found) = loop 10000 stats.Count 0 0
        Assert.AreEqual(stats.Count, found)
        Assert.AreNotEqual(10000, iteration)
        printf "Seq.shuffleFairly found all %i permutations in %i iterations" stats.Count iteration
