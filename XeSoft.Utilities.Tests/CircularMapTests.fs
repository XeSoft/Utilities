namespace TrainSafe.Data.Service.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open XeSoft.Utilities

[<TestClass>]
type CircularMapTests () = 
    let testGuids = [ for i in 1 .. 10 -> (Guid.NewGuid(), i) ]
    let test3 = List.take 3 testGuids
    let create capacity = CircularMap.create<Guid, int> capacity
    let rec loop before after guids m =
        match guids with
        | [] -> ()
        | h::t ->
            let (key, value) = h
            before key value m
            let m = CircularMap.add key value m
            after key value m
            loop before after t m
    let dummy _ _ _ = ()

    [<TestMethod>]
    member __.``Circular Map default indexer works`` () =
        let f key value m = Assert.AreEqual(value, CircularMap.find key m)
        loop dummy f test3 (create 3)

    [<TestMethod>]
    member __.``Circular Map Count works`` () =
        let m = create 3
        Assert.AreEqual(0, CircularMap.count m)
        let f _ value m = Assert.AreEqual(value, CircularMap.count m)
        loop dummy f test3 m

    [<TestMethod>]
    member __.``Circular Map ContainsKey works`` () =
        let before k _ m = Assert.AreEqual(false, CircularMap.containsKey k m)
        let after k _ m = Assert.AreEqual(true, CircularMap.containsKey k m)
        loop before after test3 (create 3)

    [<TestMethod>]
    member __.``Circular Map Keys works`` () = 
        let after _ v m =
            let expected = test3 |> List.take (v) |> List.map fst |> List.sort
            let actual = CircularMap.keys m |> Seq.sort |> List.ofSeq
            Assert.AreEqual(expected, actual)
        loop dummy after test3 (create 3)

    [<TestMethod>]
    member __.``Circular Map Values works`` () =
        let after _ v m =
            let expected = test3 |> List.take (v) |> List.map snd |> List.sort
            let actual = CircularMap.values m |> Seq.sort |> List.ofSeq
            Assert.AreEqual(expected, actual)
        loop dummy after test3 (create 3)

    [<TestMethod>]
    member __.``Circular Map observes capacity limit and overwrites oldest values`` () =
        let before k v m =
            let beforeV = if v < 4 then v - 1 else 3
            Assert.AreEqual(false, CircularMap.containsKey k m) // not in map before add
            Assert.AreEqual(beforeV, CircularMap.count m) // no increased count before add
        let after k v m =
            let afterV = if v < 3 then v else 3
            Assert.AreEqual(true, CircularMap.containsKey k m) // in map after add
            Assert.AreEqual(afterV, CircularMap.count m) // increased count after add
            for (key, value) in List.take v testGuids do
                let isLast3 = v - value < 3 // is this in last 3?
                // not last 3, not in map; in last 3, in map
                Assert.AreEqual(isLast3, CircularMap.containsKey key m)
        loop before after testGuids (create 3)