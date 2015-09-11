namespace XeSoft.Utilities.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open XeSoft.Utilities

[<TestClass>]
type RandomBufferTests () =

    let initArrayFirstZero (arr:byte array) =
        for i = 0 to arr.Length - 1 do
            if i < 4 then arr.[i] <- 0uy
            else arr.[i] <- 255uy

    [<TestMethod>]
    member __.``RandomBuffer with an item buffer size of int.MaxValue does not fail`` () =
        let rng = new RandomBuffer((fun _ -> ()), System.Int32.MaxValue)
        let ratio = rng.GetRandomRatio ()
        Assert.AreEqual(0.0, ratio)

    [<TestMethod>]
    member __.``RandomBuffer with an item buffer size of int.MaxValue - 1 does not fail`` () =
        let rng = new RandomBuffer((fun _ -> ()), System.Int32.MaxValue - 1)
        let ratio = rng.GetRandomRatio ()
        Assert.AreEqual(0.0, ratio)

    [<TestMethod>]
    member __.``RandomBuffer resets when overflowed`` () =
        let bufferItems = 3
        let rng = new RandomBuffer(initArrayFirstZero, bufferItems)
        for i = 0 to 5 do
            let ratio = rng.GetRandomRatio ()
            if i % bufferItems = 0 then // resets every bufferItems
                Assert.AreEqual(0.0, ratio)
            else
                Assert.AreEqual(1.0, ratio)
