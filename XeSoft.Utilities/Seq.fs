namespace XeSoft.Utilities

module Seq =
    open System.Security.Cryptography
    open System

    /// Shuffle a sequence with a given RNG.
    /// Uses the provided random number generator and the Durstenfeld shuffle algorithm.
    /// getRandomIndexUpTo is a function which generates a random number between 0 and the given number (both inclusive).
    /// s is the sequence to be shuffled.
    /// Returns a new sequence which is shuffled.
    /// NOTE: does not work on infinite sequences as the sequence is enumerated
    let shuffle (getRandomIndexUpTo:int -> int) (s:'a seq) =
        let arr = Array.ofSeq s
        let lastIndex = (Array.length arr) - 1
        for maxIndex = lastIndex downto 1 do
            let randomJ = getRandomIndexUpTo maxIndex
            Array.swapUnsafe maxIndex randomJ arr
        arr |> Seq.ofArray

    /// Shuffle a sequence with a seeded pseudo-RNG.
    /// Uses a seeded psuedo random number generator and the Durstenfeld shuffle algorithm.
    /// seed is the seed value.
    /// s is the sequence to be shuffled.
    /// Returns a new sequence which is shuffled.
    /// NOTE: does not work on infinite sequences as the sequence is enumerated
    let shuffleSeeded seed s =
        let rngSeeded = new Random(seed)
        let getInt max = rngSeeded.Next(max + 1) // because it's exclusive max, shuffle expects inclusive
        s |> shuffle getInt

    /// Shuffle a sequence.
    /// Uses a cryptographic random number generator and the Durstenfeld shuffle algorithm.
    /// Returns a new sequence which is shuffled in random order.
    /// NOTE: does not work on infinite sequences as the sequence is enumerated
    let shuffleCrypto s =
        let rngCsp = new RNGCryptoServiceProvider ()
        let rng = new RandomBuffer(rngCsp.GetBytes, Seq.length s)
        let getInt max =
            rng.GetRandomRatio ()
            |> Ratio.scaleBetween 0 max
        s |> shuffle getInt


