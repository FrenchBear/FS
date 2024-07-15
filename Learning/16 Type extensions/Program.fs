// 17 Type extensions
// Learning F#, Play with types extensions
//
// 2024-07-15   PV

open System
open Microsoft.FSharp.Collections

type PairList = (int*int) list
let u = typedefof<PairList>
printfn "%A" u

let v = (1,2)
let tv = v.GetType()
printfn "%s" tv.AssemblyQualifiedName

// System.ValueTuple
let w = struct(1,2)
let tw = w.GetType()
printfn "%s" tw.AssemblyQualifiedName

printfn ""


// Basic types and lists can be extended

type System.String with
    member this.zap = printfn "Hello string"

type System.Int32 with
    member this.zap = printfn "Hello int"

type Microsoft.FSharp.Collections.List<'T> with
    member this.zap = printfn "Hello F# list"
   

let s = "abcd"
s.zap

let n = 42
n.zap

let l = [1;2;3]
l.zap



// But not so much for tuples...

// Only this one seems to be accessible, and only the static version
type System.Tuple<'T> with
    static member zap tt = printfn "Hello tuple 1 zap: %A" tt
    member this.zip = printfn "Hello tuple 1 zip: %A" this

type System.Tuple<'T, 'U> with
    static member zap = printfn "Hello tuple 2"

type System.Tuple with
    static member zap = printfn "Hello tuple 0"


let t1:System.Tuple<Int32,Int32> = (1,2)
let t2 = (1,2)
printfn "t1=t2? %b" (t1=t2)


System.Tuple<_>.zap t2
System.Tuple<_>.zap (1,2,3,4)

System.Tuple<System.Int32>.zap ("a", "b")
System.Tuple<System.Int32>.zap ("a", "b")   // Work, depite the fact that it's not a Tupe<Int32>...


