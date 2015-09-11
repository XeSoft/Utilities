namespace XeSoft.Utilities

open System

/// A byte-based buffer to get random floating point numbers between 0 and 1 inclusive.
/// Such a number is suitable for scaling to the desired range.
/// fillRandomBytes - the method to fill the byte array with random bytes.
/// numberOfBufferedItems - how many random numbers to buffer at a time (default is 128).
type RandomBuffer (fillRandomBytes:byte array -> unit, ?numberOfBufferedItems:int) =
    static let defaultBufferedItems = 128
    static let intBufferSize = sizeof<uint32> // pulling this many bytes out of buffer at a time
    // largest array length is int.MaxValue, but that can still overflow
    // so further reduce max number by half
    static let maxNumberIntegers = Int32.MaxValue / intBufferSize / 2

    // try to choose a buffer size. if too large, it will be halved until it doesn't overflow
    let rec chooseBufferSize (idealNumberOfIntegers:int) =
        if idealNumberOfIntegers >= maxNumberIntegers
        then chooseBufferSize (idealNumberOfIntegers / 2)
        else idealNumberOfIntegers * intBufferSize

    let requestedBufferedItems = defaultArg numberOfBufferedItems defaultBufferedItems

    let bufferSize = chooseBufferSize requestedBufferedItems
    let buffer = Array.zeroCreate<byte> bufferSize
    let mutable bufferPosition = bufferSize // start maxed out to cause a reset

    // fill the buffer with random bytes, reset buffer position
    let reset () =
        fillRandomBytes buffer
        bufferPosition <- 0

    /// Get a random Double between 0.0 and 1.0 inclusive
    member __.GetRandomRatio () =
        if bufferPosition >= buffer.Length then
            reset ()
        let value = BitConverter.ToUInt32 (buffer, bufferPosition)
        bufferPosition <- bufferPosition + intBufferSize
        (float value) / (float System.UInt32.MaxValue) // float between 0.0 and 1.0 inclusive
