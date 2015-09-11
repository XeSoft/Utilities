namespace XeSoft.Utilities

module Array =

    /// Returns true if i is a valid index in the array a.
    /// Otherwise returns false.
    let hasIndex i (a:'a array) =
        a.Length > 0
        && 0 <= i
        && i < a.Length

    /// Swap two values in an array, modifying the array (hence Unsafe).
    /// i is the location of one value to swap.
    /// j is the location of the other value to swap.
    let swapUnsafe i j (a:'a array) =
        if a|> hasIndex i && a|> hasIndex j && i <> j then
            let temp = a.[j]
            a.[j] <- a.[i]
            a.[i] <- temp
