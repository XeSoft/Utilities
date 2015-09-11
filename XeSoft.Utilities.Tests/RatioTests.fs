namespace XeSoft.Utilities.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open XeSoft.Utilities

[<TestClass>]
type RatioTests () =

    [<TestMethod>]
    member __.``Ratio scaleBetween 9 and 1 to scale 0.5 generates 5`` () =
        let number = Ratio.scaleBetween 9 1 0.5
        Assert.AreEqual(5, number)

    [<TestMethod>]
    member __.``Ratio scaleBetween 1 and 9 to scale 0.5 generates 5`` () =
        let number = Ratio.scaleBetween 1 9 0.5
        Assert.AreEqual(5, number)

    [<TestMethod>]
    member __.``Ratio scaleBetween 1 and 9 to scale -0.5 generates 5`` () =
        let number = Ratio.scaleBetween 1 9 -0.5
        Assert.AreEqual(5, number)

    [<TestMethod>]
    member __.``Ratio scaleBetween 1 and 9 to scale 1.0 generates 9`` () =
        let number = Ratio.scaleBetween 1 9 1.0
        Assert.AreEqual(9, number)

    [<TestMethod>]
    member __.``Ratio scaleBetween 1 and 9 to scale 0.89 generates 9`` () =
        let number = Ratio.scaleBetween 1 9 0.89
        Assert.AreEqual(9, number)

    [<TestMethod>]
    member __.``Ratio scaleBetween 1 and 9 to scale 0.88 generates 8`` () =
        let number = Ratio.scaleBetween 1 9 0.88
        Assert.AreEqual(8, number)

    [<TestMethod>]
    member __.``Ratio scaleBetween 1 and 9 to scale 0.0 generates 1`` () =
        let number = Ratio.scaleBetween 1 9 0.0
        Assert.AreEqual(1, number)

    [<TestMethod>]
    member __.``Ratio scaleBetween 1 and 9 to scale 0.11 generates 1`` () =
        let number = Ratio.scaleBetween 1 9 0.11
        Assert.AreEqual(1, number)

    [<TestMethod>]
    member __.``Ratio scaleBetween 1 and 9 to scale 0.12 generates 2`` () =
        let number = Ratio.scaleBetween 1 9 0.12
        Assert.AreEqual(2, number)

    [<TestMethod>]
    member __.``Ratio scaleBetween 1 and 9 to scale 5.0 generates 9`` () =
        let number = Ratio.scaleBetween 1 9 5.0
        Assert.AreEqual(9, number)

    [<TestMethod>]
    member __.``Ratio scaleBetween 1 and 9 to scale -5.0 generates 9`` () =
        let number = Ratio.scaleBetween 1 9 -5.0
        Assert.AreEqual(9, number)

