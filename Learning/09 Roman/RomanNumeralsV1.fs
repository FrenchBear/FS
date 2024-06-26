// 09 Roman
// Learning F#, Roman numerals, first version
//
// 2024-06-26   PV

module RomanNumeralsV1

// ==========================================
// Types
// ==========================================

type RomanDigit = I | V | X | L | C | D | M
type RomanNumeral = RomanNumeral of RomanDigit list

// ==========================================
// Output logic
// ==========================================

/// Converts a single RomanDigit to an integer
let digitToInt =
    function
    | I -> 1
    | V -> 5
    | X -> 10
    | L -> 50
    | C -> 100
    | D -> 500
    | M -> 1000

/// converts a list of digits to an integer
let rec digitsToInt =
    function

    // empty is 0
    | [] -> 0

    // special case when a smaller comes before larger
    // convert both digits and add the difference to the sum
    // Example: "IV" and "CM"
    | smaller::larger::ns when smaller < larger ->
        (digitToInt larger - digitToInt smaller)  + digitsToInt ns

    // otherwise convert the digit and add to the sum
    | digit::ns ->
        digitToInt digit + digitsToInt ns

/// converts a RomanNumeral to an integer
let toInt (RomanNumeral digits) = digitsToInt digits

// ==========================================
// Input logic
// ==========================================

type ParsedChar =
    | Digit of RomanDigit
    | BadChar of char

let charToRomanDigit =
    function
    | 'I' -> Digit I
    | 'V' -> Digit V
    | 'X' -> Digit X
    | 'L' -> Digit L
    | 'C' -> Digit C
    | 'D' -> Digit D
    | 'M' -> Digit M
    | ch -> BadChar ch

let toRomanDigitList (s:string) =
    s.ToCharArray()
    |> List.ofArray
    |> List.map charToRomanDigit

/// Convert a string to a RomanNumeral
/// Does not validate the input.E.g. "IVIV" would be valid
let toRomanNumeral s =
    toRomanDigitList s
    |> List.choose (
        function
        | Digit digit ->
            Some digit
        | BadChar ch ->
            eprintfn "%c is not a valid character" ch
            None
        )
    |> RomanNumeral

// ==========================================
// Validation logic
// ==========================================

let runsAllowed =
    function
    | I | X | C | M -> true
    | V | L | D -> false

let noRunsAllowed  = runsAllowed >> not

// check for validity
let rec isValidDigitList digitList =
    match digitList with

    // empty list is valid
    | [] -> true

    // A run of 5 or more anything is invalid
    // Example:  XXXXX
    | d1::d2::d3::d4::d5::_
        when d1=d2 && d1=d3 && d1=d4 && d1=d5 ->
            false

    // 2 or more non-runnable digits is invalid
    // Example:  VV
    | d1::d2::_
        when d1=d2 && noRunsAllowed d1 ->
            false

    // runs of 2,3,4 in the middle are invalid if next digit is higher
    // Example:  IIIX
    | d1::d2::d3::d4::higher::ds
        when d1=d2 && d1=d3 && d1=d4
        && runsAllowed d1 // not really needed because of the order of matching
        && higher > d1 ->
            false

    | d1::d2::d3::higher::ds
        when d1=d2 && d1=d3
        && runsAllowed d1
        && higher > d1 ->
            false

    | d1::d2::higher::ds
        when d1=d2
        && runsAllowed d1
        && higher > d1 ->
            false

    // three ascending numbers in a row is invalid
    // Example:  IVX
    | d1::d2::d3::_  when d1<d2 && d2<= d3 ->
        false

    // A single digit with no runs is always allowed
    | _::ds ->
        // check the remainder of the list
        isValidDigitList ds

// top level check for validity
let isValid (RomanNumeral digitList) =
    isValidDigitList digitList


// ===================================================================
let tests() =
    printfn "\nTests V1"

    let assertTrue b = assert b
    let assertFalse b = assert not b
    let assertEquals x y = assert (x=y)

    // Unit tests
    [I;I;I;I]  |> digitsToInt |> assertEquals 4
    [I;V]  |> digitsToInt |> assertEquals 4
    [V;I]  |> digitsToInt |> assertEquals 6
    [I;X]  |> digitsToInt |> assertEquals 9
    [M;C;M;L;X;X;I;X]  |> digitsToInt  |> assertEquals 1979
    [M;C;M;X;L;I;V] |> digitsToInt  |> assertEquals 1944

    RomanNumeral [I;I;I;I] |> toInt |> assertEquals 4
    RomanNumeral [M;C;M;L;X;X;I;X] |> toInt |> assertEquals 1979

    "IIII"  |> toRomanNumeral |> assertEquals (RomanNumeral [I;I;I;I])
    "MCMLXXIX"  |> toRomanNumeral |> assertEquals (RomanNumeral [M;C;M;L;X;X;I;X])
    ""  |> toRomanNumeral |> assertEquals (RomanNumeral [])
    
    // test good cases
    "IIII"  |> toRomanNumeral |> isValid |> assertTrue
    "IV"  |> toRomanNumeral |> isValid |> assertTrue
    "" |> toRomanNumeral |> isValid |> assertTrue

    // validation
    // test valid
    let validList = [
        [I;I;I;I]
        [I;V]
        [I;X]
        [I;X;V]
        [V;X]
        [X;I;V]
        [X;I;X]
        [X;X;I;I]
        ]
    validList |> List.map isValidDigitList |> List.map assertTrue |> ignore

    let invalidList = [
        // Five in a row of any digit is not allowed
        [I;I;I;I;I]
        // Two in a row for V,L, D is not allowed
        [V;V]
        [L;L]
        [D;D]
        // runs of 2,3,4 in the middle are invalid if next digit is higher
        [I;I;V]
        [X;X;X;M]
        [C;C;C;C;D]
        // three ascending numbers in a row is invalid
        [I;V;X]
        [X;L;D]
    ]
    invalidList |> List.map isValidDigitList |> List.map assertFalse |> ignore


    // error cases
    "IIXX" |> toRomanNumeral |> isValid |> assertFalse
    "VV" |> toRomanNumeral |> isValid |> assertFalse

    // grand finale
    [ "IIII"; "XIV"; "MMDXC"; "IIXX"; "VV"; ]
    |> List.map toRomanNumeral
    |> List.iter (function
        | n when isValid n ->
            printfn "%A is valid and its integer value is %i" n (toInt n)
        | n ->
            printfn "%A is not valid" n
        )
