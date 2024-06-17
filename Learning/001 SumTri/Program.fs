(* 001 SumTri
** Learning F#, recursive function to compute sum of integers from 1 to n
**
** 2024-06-17   PV
*)

let sumNumbersUpTo max =

    // recursive helper function with accumulator
    let rec recursiveSum n sumSoFar =
        match n with
        | 0 -> sumSoFar
        | _ -> recursiveSum (n-1) (n+sumSoFar)

    // call helper function with initial values
    recursiveSum max 0

// test
let res = sumNumbersUpTo 1000
printfn "SumTri 1000=%d" res
