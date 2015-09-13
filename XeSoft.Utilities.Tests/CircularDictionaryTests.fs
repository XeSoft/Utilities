namespace TrainSafe.Data.Service.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open System.Linq
open XeSoft.Utilities

[<TestClass>]
type CircularDictionaryTests () = 
    let testGuids = [ for i in 1 .. 10 -> (Guid.NewGuid(), i.ToString()) ]

    [<TestMethod>]
    member __.``Circular Dictionary default indexer works`` () = 
        let cd = CircularDictionary.create<Guid, string>(3)
        for i in 0 .. 2 do
            let (key, value) = testGuids.[i]
            cd.Add(key, value)
            Assert.AreEqual(value, cd.[key])

    [<TestMethod>]
    member __.``Circular Dictionary Count works`` () = 
        let cd = CircularDictionary.create<Guid, string>(3)
        Assert.AreEqual(0, cd.Count)
        for i in 0 .. 2 do
            let (key, value) = testGuids.[i]
            cd.Add(key, value)
            Assert.AreEqual(i + 1, cd.Count)

    [<TestMethod>]
    member __.``Circular Dictionary ContainsKey works`` () = 
        let cd = CircularDictionary.create<Guid, string>(3)
        for i in 0 .. 2 do
            let (key, value) = testGuids.[i]
            Assert.AreEqual(false, cd.ContainsKey(key))
            cd.Add(key, value)
            Assert.AreEqual(true, cd.ContainsKey(key))

    [<TestMethod>]
    member __.``Circular Dictionary Keys works`` () = 
        let cd = CircularDictionary.create<Guid, string>(3)
        for i in 0 .. 2 do
            let (key, value) = testGuids.[i]
            cd.Add(key, value)
            Assert.AreEqual(key, cd.Keys.Skip(i).First())

    [<TestMethod>]
    member __.``Circular Dictionary Values works`` () = 
        let cd = CircularDictionary.create<Guid, string>(3)
        for i in 0 .. 2 do
            let (key, value) = testGuids.[i]
            cd.Add(key, value)
            Assert.AreEqual(value, cd.Values.Skip(i).First())

    [<TestMethod>]
    member __.``Circular Dictionary observes capacity limit and overwrites oldest values`` () = 
        let cd = CircularDictionary.create<Guid, string>(3)
        Assert.AreEqual(0, cd.Count)
        cd.Add(fst testGuids.[0], snd testGuids.[0])
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[0]))
        Assert.AreEqual(1, cd.Count)
        cd.Add(fst testGuids.[1], snd testGuids.[1])
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[0]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[1]))
        Assert.AreEqual(2, cd.Count)
        cd.Add(fst testGuids.[2], snd testGuids.[2])
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[0]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[1]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[2]))
        Assert.AreEqual(3, cd.Count)
        cd.Add(fst testGuids.[3], snd testGuids.[3])
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[0]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[1]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[2]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[3]))
        Assert.AreEqual(3, cd.Count)
        cd.Add(fst testGuids.[4], snd testGuids.[4])
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[0]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[1]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[2]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[3]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[4]))
        Assert.AreEqual(3, cd.Count)
        cd.Add(fst testGuids.[5], snd testGuids.[5])
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[0]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[1]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[2]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[3]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[4]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[5]))
        Assert.AreEqual(3, cd.Count)
        cd.Add(fst testGuids.[6], snd testGuids.[6])
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[0]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[1]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[2]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[3]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[4]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[5]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[6]))
        Assert.AreEqual(3, cd.Count)
        cd.Add(fst testGuids.[7], snd testGuids.[7])
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[0]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[1]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[2]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[3]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[4]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[5]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[6]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[7]))
        Assert.AreEqual(3, cd.Count)
        cd.Add(fst testGuids.[8], snd testGuids.[8])
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[0]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[1]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[2]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[3]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[4]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[5]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[6]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[7]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[8]))
        Assert.AreEqual(3, cd.Count)
        cd.Add(fst testGuids.[9], snd testGuids.[9])
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[0]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[1]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[2]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[3]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[4]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[5]))
        Assert.IsFalse(cd.ContainsKey(fst testGuids.[6]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[7]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[8]))
        Assert.IsTrue(cd.ContainsKey(fst testGuids.[9]))
        Assert.AreEqual(3, cd.Count)
