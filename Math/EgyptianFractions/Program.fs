// Calculs de fractions égyptiennes, convertit une fraction en somme de franctions "1/k"
// For now, my code is ugly because I only know how to prepend an element to a list...
// Julia version is *much* more elegant and cleaner...
//
// 2024-07-15    PV


let rec gcd a b =
    match (a, b) with
    | (0, 0) -> failwith "gcd 0 0 is not defined!"
    | (0, _) -> b
    | (_, 0) -> a
    | _ -> gcd b (a % b)

let addFraction n1 d1 n2 d2 =
    let num = n1 * d2 + n2 * d1
    let den = d1 * d2
    let pgdc = gcd num den
    (num / pgdc, den / pgdc)

// Transform a list of pairs [(n1,d1); (n2,d2); ... (nk,dk)] into a string "nk/dk + ... + d2/d2 + n1/d1"
let prettyPrint l =
    let reverseList list =
        let rec rev list acc =
            match list with
            | [] -> acc
            | [x] -> x::acc
            | head::tail -> rev tail (head::acc)
        rev list []

    reverseList l |> List.map (fun p -> $"{fst p}/{snd p}") |> List.reduce (fun a b -> $"{a} + {b}")

// Recursive descent: start with the largest egyption fraction that fits in f, and decompose the remanining difference
// Note that the list is built in "reverse order", the biggest fractions at the end
let decompose num den =
    let rec decomposeToList num den (l: (int*int) list) =
        match num with
        | 1 ->
            let f = (num, den)
            f::l
        | _ ->
            let d1 = den / num + 1
            let (f2n, f2d) = addFraction num den -1 d1
            decomposeToList f2n f2d ((1, d1)::l)

    decomposeToList num den []



printfn "Fractions Égyptiennes en F#\n"

let max=13
for num in 2..max do
    for den in num+1..max do
        if (gcd num den)=1 then
            let l = decompose num den
            printfn "%d/%d = %s" num den (prettyPrint l)
    printfn ""