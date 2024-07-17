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
let floatAdd = ajt 1.0f 2.0f // without inline on add function, this would cause a type error

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


// Lists
// A list is an immutable collection of elements of the same type. Implemented internally as a linked list.

// Create
let list1 = [ "a"; "b" ]
let list2 =
    [ 1
      2 
    ]
let list3 = "c" :: list1    // prepending; [ "c"; "a"; "b" ]
let list4 = list1 @ list3   // concat; [ "a"; "b"; "c"; "a"; "b" ]
let list5 = [ 1..2..9 ]     // start..increment..last; [ 1; 3; 5; 7; 9 ]

// Slicing is inclusive
let firstTwo = list5[0..1]  // [ 1; 3 ]

// Indexed access
printfn "list5[3] = %A" list5[3]

// Pattern matching
match list2 with
| [] -> 0               // empty list
| [ 3 ] -> 1            // a single item, which is '3'
| [ _; 4 ] -> 2         // two items, second item is '4'
| head :: tail -> 3     // cons pattern; matches non-empty. head is the first item, tail is the rest
|> ignore

// Tail-recursion with a list, using cons pattern
let rec sumEachItem (myList:int list) =
    match myList with
    | [] -> 0
    | head :: tail -> head + sumEachItem tail


// Arrays
// Arrays are fixed-size, zero-based, collections of consecutive data elements maintained as one block of memory. 
// They are mutable; individual elements can be changed.

// Create
let array1 = [| "a"; "b"; "c" |]
let array2 =
    [| 1
       2 |]
let array3 = [| 1..2..9 |]  // start..increment..last; [| 1; 3; 5; 7; 9 |]

// Indexed access
let first = array1[0]   // "a"

// Slicing is inclusive; [| "a"; "b" |]
let firstTwoArray = array1[0..1]

// Assignment using <-
array1[1] <- "d"        // [| "a"; "d"; "c" |]

// Pattern matching
match array2 with
| [||] -> 0             // match an empty array
| [| 3 |] -> 1          // match array with single 3 item
| [| _; 4 |] -> 2       // match array with 2 items, second item = 4
| _ -> 3                // Catch-all to avoid a warning
|> ignore


// Sequences
// A sequence is a logical series of elements of the same type. seq<'t> is an alias for System.Collections.Generic.IEnumerable<'t>.

// Create
let seq1 = seq { 1; 2 }
let seq2 = seq {
      1
      2 }
let seq3 = seq { 1..2..9 }  // start..increment..last; 1,3,5,7,9



// Collection comprehensions

// Computed expressions with ->. Results in 1, 3, 5, 7, 9
let listComp = [ for i in 0..4 -> 2 * i + 1 ]
let arrayComp = [| for i in 0..4 -> 2 * i + 1 |]
let seqComp = seq { for i in 0..4 -> 2 * i + 1 }

// Lists with yield (same as [ 1; 3; 5; 7; 9 ] actually)
let listWithYield = [ yield 1; yield 3; yield 5; yield 7; yield 9 ]

//  Using computed expressions with yield and yield!. (yield is optional in a do, but is being used explicitly here)
let comprehendedList = [  // [ 1;3;5;7;9 ]
    for i in 0..4 do
        yield 2 * i + 1
    ]

let comprehendedArray = [|  // [| 1;3;5;7;9;1;3;5;7;9 |]
    for i in 0..4 do
        yield 2 * i + 1
    yield! listWithYield
    |]
let comprehendedSequence = seq {  // seq { 1;3;5;7;9;1;3;5;7;9;.... }
    while true do
        yield! listWithYield
    }


// ----------------------------------------------------------------------------------------
// Pause, extra stuff...

let s1 = "Hello"
let s2 = "< \"  \\  \x41  \066  \u2222  \U0001D11E  \'  \n >"

System.Console.OutputEncoding <- System.Text.Encoding.Unicode   // So the Clef of Sol 𝄞 is shown property
printfn "%s" s2


let ``strange `ident` in F#`` = 2

let s3=" \065\065 "

printfn "%s" s3

let sb = 12uy

// BEWARE, contrary to many languages, - in front of a numeric constant is merged with the constant (with few extra
// rules related to post-filtering of adjacent prefix tokens...)
// As a consequence, -3.5**2 is positive, whereas in Python, Excel, calculators, ... - has lower priority than ** or ^
// and the result is negative
let res = -3.5**2
printfn "-3.5**2 = %A" res

// The sequence int.. is parsed as two tokens, a int and symbolic-keyword ..
// Without this rule, the longest-match wule would consider this as a floating point followed by a .
let s4 = [1..5]

// Symbolic operators and some symbolic keywords have a compiled name (see F# spec)
let r4 = op_Addition 2 3
printfn "r4 = %d" r4

// Can define new operators
let (++++) a b = 2*(a+b)
let r5 = 2 ++++ 3
printfn "r5 = %d" r5

// And they also have a compiled name, see F# spec for naming
let r6 = op_PlusPlusPlusPlus 2 3
printfn "r6 = %d" r6

module Zap =
    let (%%%) x = -x
    let (+) a b = a-b

// Some long identifiers (with .) can terminate with an operator
let fn = Zap.(+)
let r7 = fn 3 2
printfn "r7 = %d" r7

let divRemTuple t = ((fst t)/(snd t), (fst t)%(snd t))
let divRem2Args a b = (a/b, a%b)

let inline sum t = Array.reduce (+) t

(100,23) |> divRemTuple |> printf "%A"
(100,23) ||> divRem2Args |> printf "%A"     // ||> splits a 2-args tuple into 2 separate values (and |||> for a 3-args tuple)

let sf = sum [| 1.0; 2.0; 3.0 |]
let si = sum [| 1; 2; 3 |]          // Without inline in sum definition, this would work but return a float...

// End of pause
// ----------------------------------------------------------------------------------------


// Data Types
