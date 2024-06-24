(* 01 SumTri
** Learning F#, recursive function to compute sum of integers from 1 to n
**
** 2024-06-17   PV
** 2024-06-20   PV      Added "just for fun"
*)

let sumNumbersUpTo max =

    // recursive helper function with accumulator
    let rec recursiveSum n sumSoFar =
        match n with
        | 0 -> sumSoFar
        | _ -> recursiveSum (n - 1) (n + sumSoFar)

    // call helper function with initial values
    recursiveSum max 0

// test
let res = sumNumbersUpTo 1000
printfn "SumTri 1000=%d" res


// Just for fun
let PI = 4.0 * atan (1.0)
let DTOR = PI / 180.0

let sind x = sin (x * DTOR)
let cosd x = cos (x * DTOR)
let tand x = tan (x * DTOR)
let asind x = asin (x) / DTOR
let acosd x = acos (x) / DTOR
let atand x = atan (x) / DTOR

let test = sind >> cosd >> tand >> atand >> acosd >> asind

printfn "res=%A" (sind 45.0)
printfn "res=%A" (45.0 |> sind |> asind)
printfn "res=%A" ((9.0 |> test) - 9.0)
