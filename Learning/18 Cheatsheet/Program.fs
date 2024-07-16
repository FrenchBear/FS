// 18 Cheatsheet
// Examples from F# 8.0 cheatsheet
// https://fsprojects.github.io/fsharp-cheatsheet/
//
// 2024-07-16   PV



// strings
// F# string type is an alias for System.String type.

let hello = "Hello" + " World"      // concatenation

// Use verbatim strings preceded by @ symbol to avoid escaping control characters (except escaping " by "").
let verbatimXml = @"<book title=""Paradise Lost"">"

// We don't even have to escape " with triple-quoted strings.
let tripleXml = """<book title="Paradise Lost">"""

// Backslash strings indent string contents by stripping leading spaces.
let poem =
    "The lesser world was daubed\n\
     By a colorist of modest skill\n\
     A master limned you in the finest inks\n\
     And with a fresh-cut quill."

// String Slicing is supported by using [start..end] syntax.
let str = "Hello World"
let firstWord = str[0..4] // "Hello"
let lastWord = str[6..] // "World"

// String Interpolation is supported by prefixing the string with $ symbol. All of these will output "Hello" \ World!:
let expr = "Hello"
printfn " \"%s\" \\ World!" expr
printfn $" \"{expr}\" \\ World!"
printfn $" \"%s{expr}\" \\ World!" // using a format specifier
printfn $@" ""{expr}"" \ World!"
printfn $@" ""%s{expr}"" \ World!"
printf  $@" ""%s{expr}"" \ World!"  // no newline

printfn "\n"


// Simple use of let to define values
let myStringValue = "my string"
let myCharValue = 'A'
let myIntValue = 10
let myExplicitlyTypedIntValue: int = 10
let mutable myMutableInt = 10
myMutableInt <- 11  // use <- arrow to assign a new value


// Basic types and litterals
let ( sbyte, byte   )  = ( 55y, 55uy )  // 8-bit integer
let ( short, ushort )  = ( 50s, 50us )  // 16-bit integer
let ( int,   uint   )  = ( 50,  50u  )  // 32-bit integer
let ( long,  ulong  )  = ( 50L, 50uL )  // 64-bit integer
let bigInt             = 9999999999999I // System.Numerics.BigInteger

let float              = 50.0f          // signed 32-bit float
let double             = 50.0           // signed 64-bit float
let scientific         = 2.3E+32        // signed 64-bit float
let decimal            = 50.0m          // signed 128-bit decimal

let mybyte             = 'a'B           // ascii character; 97uy
let byteArray          = "text"B        // ascii string; [|116uy; 101uy; 120uy; 116uy|]


// Fonctions composition
let add n1 n2 = n1 + n2
let subtract n1 n2 = n1 - n2
let negate num = -1 * num
let print num = printfn $"The number is: {num}"

let addTwoSubtractTwoNegateAndPrint num = num |> add 2 |> subtract 2 |> negate |> print
addTwoSubtractTwoNegateAndPrint 10

let addTwoSubtractTwoNegateAndPrint' = add 2 >> subtract 2 >> negate >> print
addTwoSubtractTwoNegateAndPrint' 10


// inline functions
let inline ajt x y = x + y
let integerAdd = ajt 1 2
let floatAdd = ajt 1.0f 2.0f // without `inline` on `add` function, this would cause a type error

// Statically resolved parameters
type RequestA = { Id: string; StringValue: string }
type RequestB = { Id: string; IntValue: int }

let requestA: RequestA = { Id = "A"; StringValue = "Value" }
let requestB: RequestB = { Id = "B"; IntValue = 42 }

let inline getId<'T when 'T : (member Id: string)> (x: 'T) = x.Id
let idA = getId requestA  // "A"
let idB = getId requestB  // "B"


// Collections

// functions that take unit as arguments and return different Collection types
let getList(): int list = [1;2;3;4]
let hetArray: int[] = [| 1;2;3 |]
let getSeq (): int seq = new System.Collections.Generic.List<int>()



