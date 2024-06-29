// 09 Roman
// Learning F#, Roman numerals, manual rebuild of example code
//
// 2024-06-26   PV

module RomanNumeralsPerso

type RomanDigit = I | V | X | L | C | D | M

type CheckedRomanDigit = {
    digit: RomanDigit
    isValid: bool
}
type RomanDigitsList = RomanDigit list

let romanDigitToInt =
    function
    | I -> 1
    | V -> 5
    | X -> 10
    | L -> 50
    | C -> 100
    | D -> 500
    | M -> 1000

let romanDigitsListToInt (l:RomanDigitsList) =
    let rec rdlti (l:RomanDigitsList) (sumsofar:int) =
        match l with
        | [] -> sumsofar
        | head::next::rest when romanDigitToInt(head)<romanDigitToInt(next) -> rdlti rest (sumsofar+romanDigitToInt(next)-romanDigitToInt(head))
        | head::rest -> rdlti rest (sumsofar+romanDigitToInt(head))
    rdlti l 0

let assertEquals x y =
    assert (x=y)

let test1() =
    romanDigitsListToInt [I;I;I] |> assertEquals 3
    romanDigitsListToInt [I;V] |> assertEquals 4
    romanDigitsListToInt [C;L;X;I;X] |> assertEquals 169

let charToRomanCheckedDigit c =
    match c with
    | 'I' -> { digit=I; isValid=true}
    | 'V' -> { digit=V; isValid=true}
    | 'X' -> { digit=X; isValid=true}
    | 'L' -> { digit=L; isValid=true}
    | 'C' -> { digit=C; isValid=true}
    | 'D' -> { digit=D; isValid=true}
    | 'M' -> { digit=M; isValid=true}
    | c -> printfn "Invalid character %c" c; { digit=I; isValid=false}

let stringToRomanDigitsList (s:string) =
    let rec strdl (lc:char list) (listSoFar: RomanDigit list) =
        match lc with
        | [] -> listSoFar
        | c::rest -> 
            let cd = charToRomanCheckedDigit c
            match cd with
            | {digit=d; isValid=true} -> strdl rest (listSoFar @ [d])
            | _ -> []   // Returns an empty list of RomanDigit in case it contains an invalid char

    let lc = s.ToCharArray() |> Array.toList
    strdl lc []

let test2() =
    let res = stringToRomanDigitsList "II"
    stringToRomanDigitsList "II" |> assertEquals [I;I]
    stringToRomanDigitsList "VI" |> assertEquals [V;I]
    stringToRomanDigitsList "CLXIX" |> assertEquals [C;L;X;I;X]

let stringToInt (s:string) :int =
    let dl = stringToRomanDigitsList s
    let res = romanDigitsListToInt dl
    res

let test3() = 
    "IIII"  |> stringToInt |> assertEquals 4
    "IV" |> stringToInt |> assertEquals 4
    "VI"  |> stringToInt |> assertEquals 6
    "IX"  |> stringToInt |> assertEquals 9
    "MCMLXXIX"  |> stringToInt  |> assertEquals 1979
    "MCMXLIV" |> stringToInt  |> assertEquals 1944
    "VI?"  |> stringToInt |> assertEquals 0
