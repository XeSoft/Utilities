namespace XeSoft.Utilities

open System
open System.Collections.Generic

[<CustomComparison>]
[<CustomEquality>]
/// A combination dictionary and circular buffer.
/// When the dictionary is full, any new additions will overwrite the oldest entry.
type CircularMap<'key, 'value when 'key : comparison and 'value : equality> =
    private {
        Hash: Map<'key,'value>;
        Buffer: 'key array;
        IsFull: bool;
        Next: int;
    }

    override m.Equals (that) =
        match that with
        | :? CircularMap<'key, 'value> as that ->
            m.Hash = that.Hash
            && m.Buffer = that.Buffer
            && m.IsFull = that.IsFull
            && m.Next = that.Next
        | _ -> false
    override m.GetHashCode () = // unchecked by default?
        17
        * (31 + m.Hash.GetHashCode ())
        * (31 + m.Buffer.GetHashCode ())
        * (31 + m.IsFull.GetHashCode ())
        * (31 + m.Next.GetHashCode ())
    override m.ToString () = m.Hash.ToString ()

    static member private toD m = m :> IDictionary<'key, 'value>
    static member private toC m = m :> ICollection<KeyValuePair<'key, 'value>>

    // for compatibility with looping, LINQ extension methods, etc.
    // ------------------------------------------------------------
    interface IEnumerable<KeyValuePair<'key, 'value>> with
        member m.GetEnumerator () = (m.Hash :> IEnumerable<KeyValuePair<'key, 'value>>).GetEnumerator ()

    interface System.Collections.IEnumerable with
        member m.GetEnumerator () = (m.Hash :> System.Collections.IEnumerable).GetEnumerator ()

    interface IDictionary<'key, 'value> with 
        member m.Item 
            with get x = (CircularMap.toD m.Hash).[x]            
            and  set x v = (CircularMap.toD m.Hash).[x] <- v
        member m.Keys = (CircularMap.toD m.Hash).Keys
        member m.Values = (CircularMap.toD m.Hash).Values
        member m.Add (k, v) = (CircularMap.toD m.Hash).Add (k,v)
        member m.ContainsKey (k) = (CircularMap.toD m.Hash).ContainsKey(k)
        member m.TryGetValue (k, r) = (CircularMap.toD m.Hash).TryGetValue (k, ref r)
        member m.Remove (k : 'key) = (CircularMap.toD m.Hash).Remove(k)

    interface ICollection<KeyValuePair<'key, 'value>> with 
        member m.Add (x) = (CircularMap.toC m.Hash).Add (x)
        member m.Clear () = (CircularMap.toC m.Hash).Clear ()
        member m.Remove (x) = (CircularMap.toC m.Hash).Remove (x)
        member m.Contains (x) = (CircularMap.toC m.Hash).Contains (x)
        member m.CopyTo (arr, i) = (CircularMap.toC m.Hash).CopyTo (arr, i)
        member m.IsReadOnly = (CircularMap.toC m.Hash).IsReadOnly
        member m.Count = (CircularMap.toC m.Hash).Count

    interface System.IComparable with 
        member m.CompareTo (obj:obj) = (m.Hash :> System.IComparable).CompareTo (obj)


namespace XeSoft.Utilities

module CircularMap =

    let containsKey k (m:CircularMap<_,_>) =
        Map.containsKey k m.Hash

    let find k (m:CircularMap<_,_>) =
        Map.find k m.Hash

    let keys (m:CircularMap<_,_>) =
        (m.Hash :> System.Collections.Generic.IDictionary<_,_>).Keys

    let values (m:CircularMap<_,_>) =
        (m.Hash :> System.Collections.Generic.IDictionary<_,_>).Values

    let count (m:CircularMap<_,_>) =
        (m.Hash :> System.Collections.Generic.IDictionary<_,_>).Count

    /// add a value to the map, removing the oldest value if the capacity is reached
    let add key value (m:CircularMap<_,_>) =
        let removeIfNeeded () =
            if m.IsFull then
                Map.remove m.Buffer.[m.Next] m.Hash
            else m.Hash
        let putNew hash =
            Array.set m.Buffer m.Next key
            Map.add key value hash
        let movePointer hash =
            if m.Next = m.Buffer.Length - 1 then
                {m with Hash = hash; IsFull = true; Next = 0}
            else
                {m with Hash = hash; Next = m.Next + 1}

        removeIfNeeded ()
        |> putNew
        |> movePointer

    /// create a new circular map with the given capacity
    let create<'key, 'value when 'key : comparison and 'value : equality> (capacity: int) =
        {
            Hash = Map.empty<'key,'value>;
            Buffer = Array.zeroCreate<'key> capacity;
            IsFull = false;
            Next = 0;
        }
