// 18 Cheatsheet
// Examples from F# 8.0 cheatsheet
// https://fsprojects.github.io/fsharp-cheatsheet/
//
// 2024-07-16   PV


// ----------------------------------------------------------------------------------------
// Comments

// Line comment
/// XMLDoc comment
(* block comment *)

// Next block comment is an error, content of comment is parsed, here it contains an unfinished triple-quote string, *) is skipped
// (* """ *)    


// ----------------------------------------------------------------------------------------
// Strings
// F# string type is an alias for System.String type.

let hello = "Hello" + "\r\n World"          // concatenation
let hello2 = "Hello
 World"                                     // \r, \n can be directly included in regular string
printfn "hello=hello2: %b" (hello = hello2) // true

// Use verbatim strings preceded by @ symbol to avoid escaping control characters (except escaping " by "").
let verbatimXml = @"<book title=""Paradise Lost"">"
let verbatim1 = @"Hello
World"
printfn "hello=verbatim1: %b" (hello = hello2) // true

let ch1 = "Hello \\ \"World\" "
let ch2 = @"Hello \ ""World"" "
let ch3 = """Hello \ "World" """    // Space before final """ is required here, otherwise """" is interpreted as closing triple-quote string followed by "
printfn "ch1=ch2: %b" (ch1=ch2)
printfn "ch1=ch3: %b" (ch1=ch3)

// Note that @"""...""" means $"<verbatim ""...""> which means "...", this is not a "verbatim triple-quote string :-)

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


// --------------------------------------------
// String interpolation

// String Interpolation is supported by prefixing the string with $ symbol. All of these will output "Hello" \ World!:
let expr = "Hello"
printfn " \"%s\" \\ World!" expr
printfn $" \"{expr}\" \\ World!"        // Simple interpolated string. 
printfn $" \"%s{expr}\" \\ World!"      // using a format specifier
printfn $@" ""{expr}"" \ World!"        // With $@, use "" for quote, simple \ for backslash
printfn $@" ""%s{expr}"" \ World!"
printfn $""" "{expr}" \ World!"""
printfn $""" "%s{expr}" \ World!"""     // using a format specifier
printfn $""" "{expr}" \ World!"""       // With $""", use simple " for quote, simple \ for backslash
printfn $""" "{"He"+"llo"}" \ World!""" // With $""", " are ok in expression

printfn "\n"

printfn $"{{{expr}}}"                   // In interpolated string, Use {{ and }} for simple { }, this prints {Hello}
printfn $@"{{{expr}}}"                  // same
printfn $"""{{{expr}}}"""               // same
printfn $$"""{-{{expr}}-}"""            // Extended syntax (see below) , $$=>{{ expression }}, { }=>just text, this prints {-Hello-}


printfn $"A beautiful \": {'"'}"        // In simple interpolated strings, direct use of " is forbidden (use $"""), but at sublevel, it's Ok
printfn $"1+2 = {1+2 (* trois!*) }"     // Ok for comments
printfn $"{let mutable a='{'
           let b='}'
           let concatChars x y = x.ToString()+y.ToString()
           concatChars a b}"            // Ok for multiline, {}
printfn $"{let mutable a='{'
#if DEBUG
           let n='1'
#else
           let n='2'
#endif
           let b='}'
           let anonRecord1 = {| Lettre='}'; Age = 999 |}
           let concat2 x y = x.ToString()+y.ToString()
           let concat3 x y z = concat2 (concat2 x y) z
           concat3 a n b
           }"                           // Ok for multiline including preprocessor directives, {2}

printfn
       $"""{let a="{"

#if DEBUG
            let n='\"'
#else
            let n='\''
#endif
            let b="}"

            let getCircleStats (radius: float) =
                {| Radius = radius
                   Diameter = radius * 2.0
                   Area = System.Math.PI * (radius ** 2.0)
                   Circumference = 2.0 * System.Math.PI * radius |}

            // Signature
            let printCircleStats (circle: {| Radius: float; Area: float; Circumference: float; Diameter: float |}) =
                    printfn $"Circle with R=%f{circle.Radius}; D=%f{circle.Diameter}; A=%f{circle.Area}; C=%f{circle.Circumference}"

            let cc = getCircleStats 2.0

            let anonRecord1 = {| 
                Lettre = '}'
                Chaine = "{[("; 
                SubRecord =  {| Nom="Violent"; Prenom="Pierre"; AnneeDeNaissance=1965; t=(1,"Hello",'√',0xCAFE) |}
                Age = (999,44) |}

            let anonRecord2 = {| anonRecord1 with SubRecord = {| Nom="Violent"; Prenom="Jacques"; AnneeDeNaissance=1969 |} |}

            a+n.ToString()+b
            }"""                       // Ok for multiline with ", {"}

printfn "\n"


// --------
// Format specifiers

// Format specifiers can either be printf-style or .NET-style. Printf-style specifiers are those covered in plaintext
// formatting, placed before the braces. For example:
let pi = $"%0.3f{System.Math.PI}"       // "3.142"
let code = $"0x%08x{43962}"             // "0x0000abba"

// The format specifier %A is particularly useful for producing diagnostic output of structured F# data.
let data = [0..4]
let output = $"The data is %A{data}"    // "The data is [0; 1; 2; 3; 4]"

// .NET-style specifiers are those usable with String.Format, placed after a : within the braces. For example:
let pi2 = $"{System.Math.PI:N4}"         // "3.1416"
let now = $"{System.DateTime.UtcNow:yyyyMMdd}" // e.g. "20240717"

// If a .NET-style specifier contains an unusual character, then it can be escaped using double-backticks:
let nowDashes = $"{System.DateTime.UtcNow:``yyyy-MM-dd``}" // e.g. "2024-07-17"

// Aligning expressions
printfn $"""|{"Left",-7}|{"Right",7}|"""        // |Left   |  Right|
printfn $"""|{12,-7:D04}|{3.14159265,7:F4}|"""  // |0012   | 3,1416|


// --------
// Extended syntax
// Triple quote string literals can start with multiple $ characters, which changes how many braces are required to open
// and close interpolation. In these string literals, { and } characters don't need to be escaped:
let str6 = $$"""A string containing some {curly braces} and an {{"F#" + " " + "expression"}}."""
// "A string containing some {curly braces} and an F# expression."
let another = $$$"""A string with pairs of {{ and }} characters and {{{ "an F# expression" }}}."""
// "A string with pairs of {{ and }} characters and an F# expression."""

// The number of % characters needed for format specifiers is affected in the same way:
let percent = $$"""50% of 20 is %%.1f{{20m * 0.5m}}"""
// "50% of 20 is 10.0"

// ----------------------------------------------------------------------------------------
// Basic types and litterals

// Simple use of let to define values
let myStringValue = "my string"
let myCharValue = 'A'
let myIntValue = 10
let myExplicitlyTypedIntValue: int = 10
let mutable myMutableInt = 10
myMutableInt <- 11  // use <- arrow to assign a new value

// Integer Prefixes for hexadecimal, octal, or binary:
let numbers = (0xcafe, 0x9F, 0o77, 0b1010)  // (51966, 159, 63, 10)
let numbers2 = (0x1101lf, 0o77lf, 0x3Blf)
let numbers3 = (0x1101LF, 0x3BLF)           // 0o771LF is not allowed while 0o771lf is allowed...

// Integer suffixes
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

let az01:sbyte = 55y
let az02:byte = 55uy

// ----------------------------------------------------------------------------------------
// Functions

// Use let to define named functions
let add n1 n2 = n1 + n2
let subtract n1 n2 = n1 - n2
let negate num = -1 * num
let print num = printfn $"The number is: {num}"
let maintenant () = System.DateTime.Now             // unit type (), absence of specific value

// Pipe and composition operators
let addTwoSubtractTwoNegateAndPrint num = num |> add 2 |> subtract 2 |> negate |> print
addTwoSubtractTwoNegateAndPrint 10

let addTwoSubtractTwoNegateAndPrint' = add 2 >> subtract 2 >> negate >> print
addTwoSubtractTwoNegateAndPrint' 10

// Signatures and Explicit Typing
// Function signatures are useful for quickly learning the input and output of functions. The last type is the return
// type and all preceding types are the input types.
type T1 = int -> string                       // this defines a function that receives an integer; returns a string
type T2 = int -> int -> string                // two integer inputs; returns a string
type T3 = unit -> string                      // unit; returns a string
type T4 = string -> unit                      // accepts a string; no return
type T5 = (int * string) -> string -> string  // a tuple of int and string, and a string inputs; returns a string

// inline functions
let inline ajt x y = x + y
let integerAdd = ajt 1 2
let floatAdd = ajt 1.0f 2.0f // without inline on add function, this would cause a type error

// recursive functions
// The rec keyword is used together with the let keyword to define a recursive function:

[<TailCall>]                // Will cause a compilation warning since
let rec fact x =            // it does not support tail call optimization
    if x < 1 then 1
    else x * fact (x - 1)

// Mutually recursive functions (those functions which call each other) are indicated by and keyword:
let rec even x =
    if x = 0 then true
    else odd (x - 1)
and odd x =
    if x = 0 then false
    else even (x - 1)

// Statically resolved parameters
type RequestA = { Id: string; StringValue: string }
type RequestB = { Id: string; IntValue: int }

let requestA: RequestA = { Id = "A"; StringValue = "Value" }
let requestB: RequestB = { Id = "B"; IntValue = 42 }

// In generic info between <...>, generic types can start with ' such as 'T or 'a...
let inline getId<'T when 'T : (member Id: string)> (x: 'T) = x.Id
let idA = getId requestA  // "A"
let idB = getId requestB  // "B"

type C<'T> = 'T * 'T


// ----------------------------------------------------------------------------------------
// Collections

// functions that take unit as arguments and return different Collection types
let getList(): int list = [1;2;3;4]
let getArray(): int array = [| 1;2;3 |]
let getSeq(): int seq = new System.Collections.Generic.List<int>()


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
printfn "list5.[3] = %A" list5.[3]

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
// Pause, extra stuff while considering parsing of F#...

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

// ' is valid in identifier name (but cannot start with ')
let a = sin
let a' = cos
let a'' = sin >> ((*) -1.0)

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

let dq1 = ``sf``

// End of pause
// ----------------------------------------------------------------------------------------


// Data Types

// Tuples
// A tuple is a grouping of unnamed but ordered values, possibly of different types:

// Construction
let numberAndWord = (1, "Hello")
let numberAndWordAndNow = (1, "Hello", System.DateTime.Now)
let structTuple = struct (1, "Hello")       // Struct tuple is equivalent to C# tuple
let structTuple2 = struct (1.025f, 1.5f)

// Deconstruction
let (numberZ, wordZ) = numberAndWord
let (_, _, now2) = numberAndWordAndNow

// fst and snd functions for two-item tuples:
let number = fst numberAndWord
let word = snd numberAndWord

// Pattern matching
let printNumberAndWord numberAndWord =
    match numberAndWord with
    | (1, word) -> printfn $"One: %s{word}"
    | (2, word) -> printfn $"Two: %s{word}"
    | (_, word) -> printfn $"Number: %s{word}"

// Function parameter deconstruction
let printNumberAndWord' (number, word) = printfn $"%d{number}: %s{word}"

// In C#, if a method has an out parameter (e.g. DateTime.TryParse) the out result will be part of a tuple.
let (success, outParsedDateTime) = System.DateTime.TryParse("2001/02/06")


// -------------------------
// Records
// Records represent aggregates of named values. They are sealed classes with extra toppings: default immutability,
// structural equality, and pattern matching support.
// Record fields can be mutable.

// Declare
type Person = { Name: string; Age: int }
type Car =
    { Make: string
      Model: string
      Year: int }

// Create
let paul = { Name = "Paul"; Age = 28 }

// Deconstruct, dot notation
let paulName = paul.Name

// Deconstruct, pattern-matching
let {Name=zzname; Age=zzage} = paul

// Copy and Update
let paulsTwin = { paul with Name = "Jim" }

// Built-in equality
let evilPaul = { Name = "Paul"; Age = 28 }
paul = evilPaul |> ignore // true

// Pattern matching
let isPaul person =
    match person with
    | { Name = "Paul" } -> true
    | _ -> false



// Anonymous Records {|   |}
// Anonymous Records represent aggregates of named values, but do not need declaring before use.
// Anonymous records do not support pattern matching, unlike named records (https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/anonymous-records)
// It is not currently possible to define an anonymous record with mutable data.

// Create
let anonRecord1 = {| Name = "Don Syme"; Language = "F#"; Age = 999 |}
let anonStructRecord = struct {| Name = "Don Syme"; Language = "F#"; Age = 999 |}

// Copy and Update
let anonRecord2 = {| anonRecord1 with Name = "Mads Torgersen"; Language = "C#" |}

let getAnonymousRecord(): {| Name:string; FirstName:string |}  = {| Name="Violent"; FirstName="Pierre" |}

let getCircleStats radius =
    {| Radius = radius
       Diameter = radius * 2.0
       Area = System.Math.PI * (radius ** 2.0)
       Circumference = 2.0 * System.Math.PI * radius |}

// Signature
let printCircleStats (circle: {| Radius: float; Area: float; Circumference: float; Diameter: float |}) =
    printfn $"Circle with R=%f{circle.Radius}; D=%f{circle.Diameter}; A=%f{circle.Area}; C=%f{circle.Circumference}"

let cc = getCircleStats 2.0
printCircleStats cc

// Deconstruct, dot notation
let zzzName = anonRecord1.Name

// Deconstruct, pattern-matching doesn't work: Anonymous records do not support pattern matching
// Only parameters can do a kind of pattern-maching deconstruct:
let circleDeconstructor (circle: {| Radius: float; Area: float; Circumference: float; Diameter: float |}) =
    (circle.Radius, circle.Area, circle.Circumference, circle.Diameter)
let (wwwRadius, wwwArea, wwwCircumference, wwwDiameter) = circleDeconstructor cc

// -------------------------
// Discriminated Unions
// Discriminated unions (DU) provide support for values that can be one of a number of named cases, each possibly with
// different values and types.

// Declaration
type Interaction =
    | Keyboard of char
    | KeyboardWithModifier of letter: char * modifier: System.ConsoleModifiers  // Tuples accept names here...
    | MouseClick of countOfClicks: int

// Create
let interaction1 = MouseClick 1
let interaction2 = MouseClick (countOfClicks = 2)
let interaction3 = KeyboardWithModifier ('c', System.ConsoleModifiers.Control)
let interaction4 = KeyboardWithModifier ('c', modifier=System.ConsoleModifiers.Control)

// Pattern matching
match interaction3 with
|   Keyboard chr -> $"Character: {chr}"
|   KeyboardWithModifier (chr, modifier) -> $"Character: {modifier}+{chr}"
|   MouseClick (countOfClicks = 1) -> "Click"
|   MouseClick count when count=2 -> "Double-click"
|   MouseClick (countOfClicks = x) -> $"Clicked: {x}"
|> ignore

// Can have named tuples using discriminated union, out of discriminated union named tuples don't exist...
type NamedTuple = NamedTuple of nom:string * age:int
let pierre = NamedTuple(nom="Pierre", age=58)

// Deconstruct a single-case discriminated union (but see next comment)
let namedTupleDeconstructor nt =
    match nt with
    | NamedTuple (n, a) -> (n,a)
let (myName, myAge) = namedTupleDeconstructor pierre

// Actually, desconstruction of a single-case using pattern-matching IS possible, and direct:
let (NamedTuple (fgA, fgB)) = pierre
assert(fgA="Pierre")
assert(fgB=58)

// Example from cheatsheet:
// Single-case discriminated unions are often used to create type-safe abstractions with pattern matching support:
type OrderId = Order of string
let orderId = Order "12"
let (Order id) = orderId  // id = "12"


// Can use anonymous directly record in discriminated union...
type NamedRecord = NamedRecord of {| nom: string; age:int |}    
let reco = NamedRecord {| nom="Pierre"; age=58 |}

let namedRecordDeconstructor nt =
    match nt with
    | NamedRecord namr -> (namr.nom, namr.age)
let (ymName, ymAge) = namedRecordDeconstructor reco

// But it doesn't work for records, they must be defined first to be used in a discriminated union
type PerRec = { nom: string; age:int }
type PerRecRec = PerRecRec of PerRec
let recrec = PerRecRec { nom="Pierre"; age=58 }


// Deconstruction of a more complex discriminated union
type ComplexDU = 
    | Character of char
    | PairOrIntegers of Item1:int * Item2:int
    | NamedTuple of NamedTuple


// Combining match and defaultArg we can extract a simple char, or '?' if it's not a "Character"
let deconstructComplexDUCharacter nt =
    defaultArg (match nt with
                | Character ch -> Some ch
                | _ -> None) '?'

let ccdu = Character 'ω'
let ch = deconstructComplexDUCharacter ccdu     // Get a char



// -------------------------
// Generics
type Tree<'T> =
    | Node of Tree<'T> * 'T * Tree<'T>
    | Leaf

let rec depth node =
    match node with
    | Node (l, _, r) -> 1 + max (depth l) (depth r)
    | Leaf -> 0


// -------------------------
// F# Core has built-in discriminated unions for error handling, e.g., option and Result.
let optionPatternMatch input =
    match input with
    | Some value -> printfn $"input is %d{value}"
    | None -> printfn "input is missing"

let resultPatternMatch input =
    match input with
    | Ok value -> $"Input: %d{value}"
    | Error value -> $"Error: %d{value}"


let res1 = Ok 42                    // Result<int, 'a>
let res2 = Error "Y'a un bug!"      // Result<'a, string>

let randomResult =                  // Result<float, byte>
    let rnd = new System.Random()
    match rnd.Next(maxValue=2) with
    | 0 -> Ok (rnd.NextDouble())
    | _ -> Error 44uy



// ----------------------------------------------------------------------------------------
// Pattern Matching

// Patterns are a core concept that makes the F# language and other MLs very powerful. They are found in let bindings,
// match expressions, lambda expressions, and exceptions.
// The matches are evaluated top-to-bottom, left-to-right; and the first one to match is selected.
// Examples of pattern matching in Collections and Data Types can be found in their corresponding sections. Here are
// some additional patterns:

let intValue = 42
let tupleValue = (2, 3)

match intValue with
| 0 -> "Zero"                  // constant pattern
| 1 | 2 -> "One or Two"        // OR pattern with constants
| x -> $"Something else: {x}"  // variable pattern; assign value to x
|> ignore

match tupleValue with
| (_ ,3) & (x, y) -> $"{x}, 3"  // AND pattern with a constant and variable; matches 3 and assign 3 to x
| _ -> "Wildcard"               // underscore matches anything
|> ignore

// when Guard clauses
// In order to match sophisticated inputs, one can use when to create filters, or guards, on patterns:
match intValue with
| 0 -> 0
| x when x < 0 -> -1
| x -> 1
|> ignore

// Pattern matching function
// The let..match..with statement can be simplified using just the function statement:
let filterNumbers num =
    match num with
        | 1 | 2 | 3 -> printfn "Found 1, 2, or 3!"
        | a -> printfn "%d" a

let filterNumbers' =  // the paramater and `match num with` are combined
    function | 1 | 2 | 3 -> printfn "Found 1, 2, or 3!"
             | a -> printfn "%d" a


// ----------------------------------------------------------------------------------------
// Exceptions

// Try..With
// An illustrative example with: custom F# exception creation, all exception aliases, raise() usage, and an exhaustive
// demonstration of the exception handler patterns:

open System

exception MyException of int * string           // (1)
let guard = true

try
    failwith   "Message"                        // throws a System.Exception (aka exn)
    nullArg    "ArgumentName"                   // throws a System.ArgumentNullException
    invalidArg "ArgumentName" "Message"         // throws a System.ArgumentException
    invalidOp  "Message"                        // throws a System.InvalidOperation

    raise(NotImplementedException("Message"))   // throws a .NET exception (2)
    raise(MyException(0, "Message"))            // throws an F# exception (2)

    true // (3)
with
| :? ArgumentNullException                      -> printfn "NullException"; false // (3)
| :? ArgumentException as ex                    -> printfn $"{ex.Message}"; false // (4)
| :? InvalidOperationException as ex when guard -> printfn $"{ex.Message}"; reraise() // (5,6)
| MyException(num, str) when guard              -> printfn $"{num}, {str}"; false // (5)
| MyException(num, str)                         -> printfn $"{num}, {str}"; reraise() // (6)
| ex when guard                                 -> printfn $"{ex.Message}"; false
| ex                                            -> printfn $"{ex.Message}"; false
|> ignore

// (1)	define your own F# exception types with exception, a new type that will inherit from System.Exception;
// (2)	use raise() to throw an F# or .NET exception;
// (3)	the entire try..with expression must evaluate to the same type, in this example: bool;
// (4)	ArgumentNullException inherits from ArgumentException, so ArgumentException must follow after;
// (5)	support for when guards;
// (6)	use reraise() to re-throw an exception; works with both .NET and F# exceptions



// Try..Finally
// The try..finally expression enables you to execute clean-up code even if a block of code throws an exception. Here's
// an example that also defines custom exceptions.

exception InnerError of string
exception OuterError of string

let handleErrors x y =
    try
        try
            if x = y then raise (InnerError("inner"))
            else raise (OuterError("outer"))
        with
        | InnerError str -> printfn "Error1 %s" str
    finally
        printfn "Always print this."

// Note that finally does not follow with. try..with and try..finally are separate expressions.




// ----------------------------------------------------------------------------------------
// Simple quotes

let simple_char = 'A'
let escape_chars = [ '\n'; '\t'; '\r'; '\b'; '\a'; '\f'; '\v'; '\''; '\\']
let hex2 = '\x1F'
let tri = '\033'
let uni4 = '\u0041'
let uni8 = '\U00000041'

let byte_simple = 'A'B
let byte_escape = '\n'B
let byte_tri = '\033'B
let byte_uni4 = '\u0041'B

let f x = x**3.0
let f' x = 3.0*x**2.0
let f'' x = 6.0*x
let f''' x = 6.0

type C1<'T> = 'T * 'T
type C2<'T,'U> = 'T * 'U



// ----------------------------------------------------------------------------------------
// Classes and inheritance

    