(* 002 Modules
** Learning F#, using modules
**
** 2024-06-17   PV
*)

let p = FS002.Powers.p3 4.0
printfn "p = %A" p

let add1Float x = FS002.MathStuff.FloatLib.add x 1.0
let q = add1Float p
printfn "q = %A" q
