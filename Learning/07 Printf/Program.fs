// 07 Printf
// Learning F#, Play with printf
//
// 2024-06-24   PV


// Composite formatting technique is available in all .NET languages, but rarely used in F#
System.Console.WriteLine("A string: {0}. An int: {1}. A float: {2}. A bool: {3}","hello",42,3.14,true)

// The printf technique, on the other hand, is based on the C-style format strings (statically type checked, supports partial application
printfn "A string: %s. An int: %i. A float: %f. A bool: %b" "hello" 42 3.14 true

// partial application - explicit parameters
let printStringAndInt s i =  printfn "A string: %s. An int: %i" s i
let printHelloAndInt i = printStringAndInt "Hello" i
printHelloAndInt 42

// partial application - point free style
let printInt =  printfn "An int: %i"
printInt 42


// printf can be used for function parameters anywhere a standard function can be used.
let doSomething printerFn x y =
    let result = x + y
    printerFn "result is" result

let callback = printfn "%s %i"
doSomething callback 3 4

// This also includes the higher order functions for lists, etc.:
[1..5] |> List.map (printf "i=%i ") |> ignore
printfn "\n"


// printf supports native F# types
// For non-primitive types, the .NET formatting functions only support using ToString(), but printf supports native F#
// types using the %A specifier:

// tuple printing
let t = (1,2)
System.Console.WriteLine("A tuple: {0}", t)
printfn "A tuple: %A" t

// record printing
type Person = {First:string; Last:string}
let johnDoe = {First="John"; Last="Doe"}
System.Console.WriteLine("A record: {0}", johnDoe )
printfn "A record: %A" johnDoe

// union types printing
type Temperature = F of int | C of int
let freezing = F 32
System.Console.WriteLine("A union: {0}", freezing )
printfn "A union: %A" freezing
