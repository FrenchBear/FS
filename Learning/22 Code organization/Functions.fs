// 22 Code organization
// Learning F#, Cheatsheet, Code organization
//
// 2024-07-28   PV


module Functions        // No = at top of the file

let sumOfSquares1 n = seq {1..n} |> Seq.map (fun x -> x*x) |> Seq.reduce (+)
let sumOfSquares2 n = seq {1..n} |> Seq.fold (fun sumSoFar x -> sumSoFar+x*x) 0
let sumOfSquares3 n = seq {1..n} |> Seq.sumBy (fun x -> x*x)
