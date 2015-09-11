namespace XeSoft.Utilities

module Ratio =

    open System

    /// Scale the given Double between a and b
    /// scale should be a Double between 0.0 and 1.0 inclusive of both
    let scaleBetween (a:int) (b:int) (scale:float) =
        let (min, max) =
            if a > b
            then (b, a)
            else (a, b)
        if min = max
        then min
        else
            let scale = Math.Abs scale // negative scale doesn't make sense here
            if scale > 1.0 // scale greater than 1 doesn't make sense
            then max
            else
                let values = max - min
                let scaled =
                    float (values + 1) * scale // generate a float on continuum from 0 <-> values + 1
                    |> Math.Floor // move float down to closest integer, 
                    |> Convert.ToInt32 // convert to int32 for compatibility with array index
                // infinitesimal chance for values + 1, but account for it
                let scaled = if scaled > values then values else scaled
                scaled + min // shift scaled up to minimum

