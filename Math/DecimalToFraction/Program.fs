// Stern-Brocot algorithm to convert a float number to a faction with error<ε
// Testing absolute error to keep things simple
//
// 2024-07-14    PV     First version, epsilon is hardcoded

open System

// Returns a tuple (n, d) where n/d is matching f with |f - n/d| < ε
let doubleToFraction (f: float) =
    let ε = 1e-6

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
            | x when Math.Abs(x) < ε -> (copySign (rNum + off * rDen) sign, rDen)
            | x when x < 0 -> internalLoop rNum rDen supNum supDen // r<f
            | _ -> internalLoop infNum infDen rNum rDen // r>f

        match f < ε with
        | true -> (copySign off sign, 1)
        | false -> internalLoop 0 1 1 0



printfn "Decimal to fraction in F#"
printfn "Stern-Brocot algorithm to transform a periodic decimal suite into a fraction\n"

let test f expectedNum expectedDen =
    let ans = doubleToFraction f

    if ans = (expectedNum, expectedDen) then
        printfn "Ok: %A = %d/%d" f (fst ans) (snd ans)
    else
        printfn "KO: %A expected %d/%d, found %d/%d" f expectedNum expectedDen (fst ans) (snd ans)


test 0.1415926535 16 113
test 3.1415926535 355 113
test -0.1415926535 -16 113
test -3.1415926535 -355 113


printfn "\nTesting 1M fractions with n,d in [1..1000]"

let rec gcd a b =
    match (a, b) with
    | (0, 0) -> failwith "gcd 0 0 is not defined!"
    | (0, _) -> b
    | (_, 0) -> a
    | _ -> gcd b (a % b)

// If n and d <= 1000, exact fraction will be found. Beyond this, it's not true anymore, another fraction with error<ε can be returned.
for i in [ 1..1_000 ] do
    for j in [ 1..1_000 ] do
        let fr = float i / float j
        let ans = doubleToFraction fr
        let (ansNum, ansDen) = ans
        let pgdc = gcd i j

        if i <> ansNum * pgdc || j <> ansDen * pgdc then
            failwith (sprintf "Check failed with %d/%d, found %d/%d" i j ansNum ansDen)

printfn "Test Ok!"
