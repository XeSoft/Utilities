namespace XeSoft.Utilities

open System.Collections.Generic

/// A combination dictionary and circular buffer.
/// When the dictionary is full, any new additions will overwrite the oldest entry.
type CircularDictionary<'key, 'value when 'key : equality>(capacity: int) =

    let dict = new System.Collections.Generic.Dictionary<'key, 'value>(capacity)
    let maxIndex = capacity - 1
    let buffer = Array.zeroCreate<'key> capacity
    let mutable isFull : bool = false
    let mutable next : int = 0

    //interface System.Collections.Generic.IDictionary<'key, 'value> with
    /// Add the key and value to the dictionary.
    /// When capacity is reached, subsequent additions will overwrite the oldest entry.
    member __.Add (key:'key, value:'value) =
        let put () =
            dict.Add (key, value)
            buffer.[next] <- key
        
        let moveNext () =
            if next = maxIndex then
                isFull <- true
                next <- 0
            else
                next <- next + 1

        let clear () =
            dict.Remove (buffer.[next])
            |> ignore

        if isFull then clear ()
        put ()
        moveNext ()

    /// Add the key value pair to the dictionary.
    /// When capacity is reached, subsequent additions will overwrite the oldest entry.
    //member me.Add (kvp:KeyValuePair<'key, 'value>) =
    //    (me :> IDictionary<'key, 'value>).Add(kvp.Key, kvp.Value)

    // ------------ parroting standard dictionary properties ------------

    /// Gets the value associated with the specified key
    member __.Item
        with get (key:'key) = dict.Item (key)
        and set _ _ = raise (new System.NotSupportedException())
    /// Get the number of key/value pairs contained in the circular dictionary
    member __.Count with get () = dict.Count
    /// Determines whether the circular dictionary contains the specified key
    member __.ContainsKey (key:'key) = dict.ContainsKey (key)
    /// Determines whether the circular dictionary contains the specified key value pair
    member __.Contains (item:KeyValuePair<'key, 'value>) = dict.ContainsKey (item.Key)
    /// Gets a collection containing the keys in the circular dictionary
    member __.Keys with get () = dict.Keys :> ICollection<'key>
    /// Gets a collection containing the values in the circular dictionary
    member __.Values with get () = dict.Values :> ICollection<'value>
    /// Gets the value associated with the specified key
    member __.TryGetValue (key:'key, value:byref<'value>) = dict.TryGetValue (key, ref value)
        
    member __.GetEnumerator () : IEnumerator<KeyValuePair<'key,'value>> = dict.GetEnumerator() :> IEnumerator<KeyValuePair<'key,'value>>
    //member __.GetEnumerator () : System.Collections.IEnumerator = (dict :> System.Collections.IEnumerable).GetEnumerator()


    member __.IsReadOnly with get () = false
    /// Remove is not supported.
    member __.Remove (_:'key) : bool = raise (new System.NotSupportedException())
    /// Remove is not supported.
    member __.Remove (_:KeyValuePair<'key,'value>) : bool = raise (new System.NotSupportedException())
    /// Clear is not supported.
    member __.Clear () : unit = raise (new System.NotSupportedException())
    /// CopyTo is not supported.
    member __.CopyTo (_, _) : unit = raise (new System.NotSupportedException())

