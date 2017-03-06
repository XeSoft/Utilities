namespace XeSoft.Utilities

module Ratio =

    open System

    /// Scale the given ratio between a and b
    /// scale should be between 0.0 and 1.0, can be inclusive of both
    let scaleBetween (a:int) (b:int) (scale:float) =
        let (min, max) =
            if a > b
            then (b, a)
            else (a, b)
        if min = max
        then min
        else
            let scale = Math.Abs scale // negative scale doesn't make sense here
            if scale >= 1.0 // scale greater than 1 doesn't make sense
            then max
            else
                // visualize min = 0, max = 3, dots being all floats between
                // | 0 . . . | 1 . . . | 2 . . . | 3
                // | 0 . . . | 1 . . . | 2 . . . | 3 . . . | 4
                // must scale to 4 and floor to actually get fair scaling from 0 to 3
                // 1.0 scale is thrown out above, so not possible to get 4
                let values = max - min
                let scaled =
                    float (values + 1) * scale // generate a float on continuum from 0 <-> values + 1
                    |> Math.Floor // move float down to closest integer, 
                    |> Convert.ToInt32 // convert to int32 for compatibility with array index
                scaled + min // shift scaled up to minimum

