// Algorithme de Stern-Brocot pour convertir une suite décimale péridique en fraction
//
// 2024-07-14    PV     First version, epsilon is hardcoded

open System


// Returns a tuple (n, d) where n/d is the matching fraction with |absolute error| < eplison
let doubleToFraction (f: float) =
    let epsilon = 1e-6

    // Classic copySign on integers
    let copySign (x: int) (y: int) = int (Math.CopySign(x, y))

    // Special case
    match f = 0.0 with
    | true -> (0, 1) // By convention, 0.0 = 0/1
    | _ ->
        let (sign, f) =
            match f with
            | x when x < 0 -> (-1, -f)
            | _ -> (1, f)

        let off = int (Math.Floor(f))
        let f = f - (float off)

        let rec internalLoop (infNum: int) (infDen: int) (supNum: int) (supDen: int) =
            let rNum = infNum + supNum
            let rDen = infDen + supDen
            let r = (float rNum) / (float rDen)

            match r - f with
            | x when Math.Abs(x) < epsilon -> (copySign (rNum + off * rDen) sign, rDen)
            | x when x < 0 -> internalLoop rNum rDen supNum supDen // r<f
            | _ -> internalLoop infNum infDen rNum rDen // r>f

        match f < epsilon with
        | true -> (copySign off sign, 1)
        | false ->internalLoop 0 1 1 0


printfn "Stern-Brocot algorithm to transform a periodic decimal suite into a fraction (F#)"

let mutable f = 0.1415926535
let mutable ans = doubleToFraction f
printfn "%10f = %d/%d" f (fst ans) (snd ans)


f <- 3.1415926535
ans <- doubleToFraction f
printfn "%10f = %d/%d" f (fst ans) (snd ans)

f <- -0.1415926535
ans <- doubleToFraction f
printfn "%10f = %d/%d" f (fst ans) (snd ans)

f <- -3.1415926535
ans <- doubleToFraction f
printfn "%10f = %d/%d" f (fst ans) (snd ans)



// Helper
let rec gcd a b =
    match (a, b) with
    | (0, 0) -> failwith "gcd 0 0 is not defined!"
    | (0, _) -> b
    | (_, 0) -> a
    | _ -> gcd b (a % b)


printfn "\nTesting 1 million fractions with n,d in [1..1000]"

for i in [ 1..1000 ] do
    for j in [ 1..1000 ] do
        let fr = doubleToFraction (float i / float j)
        let pgdc = gcd i j

        if i <> (fst fr) * pgdc || j <> (snd fr) * pgdc then
            failwith "Check failed"

printfn "Test Ok!"
