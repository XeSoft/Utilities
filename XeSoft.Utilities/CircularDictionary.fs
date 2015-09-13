namespace XeSoft.Utilities

module CircularDictionary =
    open System.Collections.Generic

    type CircularDictionaryState =
        private {
            IsFull: bool;
            Next: int;
        }

    let create<'key,'value when 'key : equality> capacity =
        let maxIndex = capacity - 1
        let buffer = Array.zeroCreate<'key> capacity
        let dict = new Dictionary<'key, 'value>(capacity)
        let mutable state = { IsFull = false; Next = 0 }

        let putNew key value =
            dict.Add (key, value)
            buffer.[state.Next] <- key

        let movePointer () =
            state <- 
                if state.Next = maxIndex
                then { IsFull = true; Next = 0 }
                else { state with Next = state.Next + 1 }

        let removeOld () =
            dict.Remove (buffer.[state.Next])
            |> ignore

        let add key value =
            if state.IsFull then
                removeOld ()
            putNew key value
            movePointer ()


        {new IDictionary<'key, 'value> 
            with
                /// Add the key and value to the dictionary.
                /// When capacity is reached, subsequent additions will overwrite the oldest entry.
                member __.Add (key:'key, value:'value) = add key value
                /// Add the key value pair to the dictionary.
                /// When capacity is reached, subsequent additions will overwrite the oldest entry.
                member me.Add (kvp:KeyValuePair<'key, 'value>) =
                    me.Add(kvp.Key, kvp.Value)
                /// Gets the value associated with the specified key. This value cannot be set directly.
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
                member __.GetEnumerator () : System.Collections.IEnumerator = (dict :> System.Collections.IEnumerable).GetEnumerator()
                // technically not read only since you can add to it
                member __.IsReadOnly with get () = false
                /// Remove is not supported.
                member __.Remove (_:'key) : bool = raise (new System.NotSupportedException())
                /// Remove is not supported.
                member __.Remove (_:KeyValuePair<'key,'value>) : bool = raise (new System.NotSupportedException())
                /// Clear is not supported.
                member __.Clear () : unit = raise (new System.NotSupportedException())
                /// CopyTo is not supported.
                // it probably could be, but haven't needed it
                member __.CopyTo (_, _) : unit = raise (new System.NotSupportedException())
        }
