// FracDev
// Fraction Development in F#
//
// 2024-07-13   PV      First version, recursive

let develop n d =
    let rec developFraction (s1: string) (dic: System.Collections.Generic.Dictionary<int, int>) n1 d1 =
        dic.Add(n1, s1.Length) // Store current remainder and associated position in s1, including 1st call

        let n2 = 10 * n1
        let digit = n2 / d1
        let rem = n2 % d1
        let s2 = sprintf "%s%d" s1 digit
        match rem with
        | 0 -> s2 // Remainder 0, dividion is done, no period.
        | _ ->
            let (found, firstViewPos) = dic.TryGetValue(rem)
            match found with
            | true -> sprintf "%s[%s]" (s2.Substring(0, firstViewPos)) (s2.Substring(firstViewPos)) // Period found
            | false -> developFraction s2 dic rem d1

    // First manage /0, sign and divisions with no fractional part
    match (n, d) with
    | (0, 0) -> "0/0 undefined"
    | (_, 0) -> "/0 error"
    | _ ->
        let sign = if n * d < 0 then "-" else ""
        let na = abs (n)
        let da = abs (d)
        let s = sprintf "%s%d" sign (na / da)
        match na % da with
        | 0 -> s // No fractional part
        | _ -> // General case
            let dic = new System.Collections.Generic.Dictionary<int, int>()
            developFraction (s + ".") dic (na % da) da

let test n d expected =
    let res = develop n d

    match res = expected with
    | true -> printfn "Ok: %d/%d = %s" n d res
    | false -> printfn "KO: %d/%d found %s expected %s" n d res expected



printfn "Fraction development in F#\n"


test 100 250 "0.4"
test 100 4 "25"
test 8 2 "4"
test 1 3 "0.[3]"
test -1 3 "-0.[3]"
test 1 -3 "-0.[3]"
test -1 -3 "0.[3]"
test 1 5 "0.2"
test 1 7 "0.[142857]"
test 100 23 "4.[3478260869565217391304]"
test 679 550 "1.23[45]"
test 1 9801 "0.[000102030405060708091011121314151617181920212223242526272829303132333435363738394041424344454647484950515253545556575859606162636465666768697071727374757677787980818283848586878889909192939495969799]" // 0.[00 01 02 03... 96 97 99] (no 98)
test 0 5 "0"
test 5 0 "/0 error"
test 0 0 "0/0 undefined"


// Performance
let mutable sw = System.Diagnostics.Stopwatch.StartNew()
for i in [1..100_000] do
    develop 100 23 |> ignore
printfn "\n100K developments of 100/23: Elapsed: %A" sw.Elapsed

sw <- System.Diagnostics.Stopwatch.StartNew()
for n in [1..1000] do
    for d in [1..1000] do
        develop n d |> ignore
printfn "1M developments: Elapsed: %A" sw.Elapsed
