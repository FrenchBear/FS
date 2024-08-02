// 20 Active patterns
// Learning F#, Cheatsheet, Play with active patterns
//
// 2024-07-25   PV


open System.Text.RegularExpressions
// Note that (| is not an atomic symbol
let ( | FractionToDouble | ) s =
    let ma = Regex.Match(s, @"^(\d+)/(\d+)$")
    if ma.Success 
    then System.Double.Parse(ma.Groups[1].ToString()) / System.Double.Parse(ma.Groups[2].ToString())
    else System.Double.NaN

let (FractionToDouble fr) = "100/23"
printfn "100/23 = %A" fr

let (|FractionToNumDen|) s =
    let ma = Regex.Match(s, @"(\d+)/(\d+)$")
    if ma.Success 
    then (System.Int32.Parse(ma.Groups[1].ToString()), System.Int32.Parse(ma.Groups[2].ToString()))
    else (0, 0)

let rec gcd a b =
    if b=0 then a else gcd b (a%b)

// printfn "gcd 18 12 = %A\n" (gcd 18 12)
// printfn "gcd 100 23 = %A\n" (gcd 100 23)
// printfn "gcd 12 18 = %A\n" (gcd 12 18)
// printfn "gcd 23 100 = %A\n" (gcd 23 100)

(*
let rec gcd a b =
    match (a, b) with
    | (0, 0) -> failwith "gcd 0 0 is not defined!"
    | (0, _) -> b
    | (_, 0) -> a
    | _ -> gcd b (a % b)
*)

let addStringFractions (FractionToNumDen (num1, den1)) (FractionToNumDen (num2, den2)) =
    let num = num1*den2+num2*den1
    let den = den1*den2
    let g = gcd num den
    $"{num/g}/{den/g}"
let sfr = addStringFractions "1/2" "3/4"
printfn @"""1/2"" + ""3/4"" = %A" sfr
printfn ""


let (|Zap|) a b = 3
let (Zap "aze" trois) = 3.1416
printfn "trois = %A\n" trois



let (|CountLetters|) s =
    s.ToString().Length

let name = "Pierre Violent"
let (CountLetters n) = name

printfn "Letters: %d" n


match name with
| CountLetters c when c<5 -> printfn "Very small word"
| CountLetters c when c<10 -> printfn "Common word"
| _ -> printfn "Long word"

let PrintWordLength (CountLetters n) =
    printfn "Longueur: %d" n

PrintWordLength name





let (|RealPart|) (x:System.Numerics.Complex) = x.Real
let (|ImaginaryPart|) (x:System.Numerics.Complex) = x.Imaginary
let (|RealImaginary|) (x:System.Numerics.Complex) = (x.Real, x.Imaginary)

let z = new System.Numerics.Complex(4.0, 5.0)
match z with
| RealImaginary (r, i) -> printfn "z=%A+%Ai" r i



// For paremeterized Active Pattern, the parameter is always just after the name

let (|DefVal|) def va =
    match va with
    | Some v -> v
    | None -> def

let (DefVal 12 nn) = Some 42            // Some 42 is va, nn is the result of DefVal pattern matching



let (|Decomp|) defVal defSign value =
    let tp = System.Int32.TryParse(value:string)
    if (fst tp) then
        (abs (snd tp), sign (snd tp))
    else
        (defVal, defSign)

let (Decomp 1 1 di) = "-43"
printfn "Decomp: %A" di

let (Decomp 1 1 di2) = "Hello"
printfn "Decomp: %A\n" di2



// Multi-case active pattern
// Can only be used in a match construction

let rnd = new System.Random()
let (|Pile|Face|) x =
    if rnd.Next(2)=0 then Pile else Face

for i in [1..10] do
    match i with
    | Pile -> printf "P "
    | Face -> printf "F "
printfn ""


// This one returns something like
// type Result =
// | Entier of int
// | Negatif of int*iny
// | Reel of float
// | Autre
// But it can only be used in a match deconstructor

let (|Entier|Reel|Negatif|Autre|) (str:string) =
    let tup = System.Int32.TryParse(str)
    if (fst tup)
    then 
        let i=snd tup
        if i>0 then Entier i else Negatif(i,i)
    else
        let tupd = System.Double.TryParse(str)
        if (fst tupd)
        then Reel (snd tupd)
        else Autre

match "3,1416" with                 // Note  that Double.TryParse expects a comma...
| Entier i -> printfn "Int %d" i
| Negatif(n1,n2) -> printfn "Negatif(%d,%d)" n1 n2
| Reel f -> printfn "Float %A" f    // while printf use a dot in its output!
| Autre -> printfn "Not a number"


// Partial active pattern, use |_|
// Must return an Option<'T>

let (|DivisibleBy|_|) by n =
    if n%by=0
    then Some DivisibleBy
    else None

let fizzBuzz = function
    | DivisibleBy 3 & DivisibleBy 5 -> "FizzBuzz"
    | DivisibleBy 3 -> "Fizz"
    | DivisibleBy 5 -> "Buzz"
    | i -> string i
