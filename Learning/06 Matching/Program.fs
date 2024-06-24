// 06 Matching
// Learning F#, Play with matching
//
// 2024-06-24   PV


// Order is important, wildcard is last!
let x1 =
    match 1 with
    | 1 -> "a"
    | 2 -> "b"
    | _ -> "z"

// if we changed the order to put the wildcard first, it would be tried first and always win immediately
// let x2 =
//     match 1 with
//     | _ -> "z"
//     | 1 -> "a"      // Warning FS0026: This rule will never be matched
//     | 2 -> "b"      // Warning FS0026: This rule will never be matched


// match..with is an expression --> all branches must return the same type

//let x =
//    match 1 with
//    | 1 -> 42
//    | 2 -> true    // error wrong type
//    | _ -> "hello" // error wrong type

// nested match..withs are ok
let f aValue =
    match aValue with
    | x ->
        match x with
        | _ -> "something"


// match expression embedded in a lambda
let ls = [2..10] |> List.map (fun i ->
        match i with
        | 2 | 3 | 5 | 7 -> sprintf "%i is prime" i
        | _ -> sprintf "%i is not prime" i
        )

// matching must be exhaustive
// Followint causes runtime exception Microsoft.FSharp.Core.MatchFailureException
//let x =
//    match 42 with
//    | 1 -> "a"
//    | 2 -> "b"

// wildcard is not always mandatory
type Choices = A | B | C
let x3 =
    match A with
    | A -> "a"
    | B -> "b"
    | C -> "c"
    //NO default match

// assert an runtime error for the wildcard case
let x4 =
    match 1 with
    | 1 -> "a"
    | 2 -> "b"
    | i when i >= 0 && i<=100 -> "ok"
    // the last case will always match
    | x -> failwithf "%i is out of range" x
    

// Types of patterns

// Binding to values
let y1 =
    match (1,0) with
    // binding to a named value
    | (1,x) -> printfn "x=%A" x
    | _ -> printfn "Other case"

// AND, OR

let y2 =
    match (1,2) with
    // OR  -- same as multiple cases on one line
    | (2,x) | (3,x) | (4,x) -> printfn "x=%A" x

    // AND  -- must match both patterns at once. Only a single "&" is used
    | (1,x) & (_,1) -> printfn "x=%A" x
    | _ -> printfn "Other case"


// The OR logic is particularly common when matching a large number of union cases:
type Choices2 = A | B | C | D
let x =
    match A with
    | A | B | C -> "a or b or c"
    | D -> "d"


// Matching on lists
let y3 =
    match [1;2;3] with
    // binding to explicit positions. Square brackets used!
    | [1;x;y] -> printfn "x=%A y=%A" x y
    // binding to head::tail. No square brackets used!
    | 1::tail -> printfn "tail=%A" tail
    // empty list
    | [] -> printfn "empty"
    | _ -> printfn "Other case"

// NO matching on sequences (IEnumerables)


// Looping and recursion

// loop through a list and print the values
let rec loopAndPrint aList =
    match aList with
    // empty list means we're done.
    | [] -> printfn "empty"
    // binding to head::tail.
    | x::xs ->
        printfn "element=%A," x
        // do all over again with the rest of the list
        loopAndPrint xs
// test
loopAndPrint [1..5]

// loop through a list and sum the values
let rec loopAndSum aList sumSoFar =
    match aList with
    // empty list means we're done.
    | [] -> sumSoFar
    // binding to head::tail.
    | x::xs ->
        let newSumSoFar = sumSoFar + x
        // do all over again with the rest of the list and the new sum
        loopAndSum xs newSumSoFar
// test
let y4 = loopAndSum [1..5] 0
printfn "sum=%A" y4



// Matching on tuples, records and unions
// -----------------------
// Tuple pattern matching
let aTuple = (1,2)
match aTuple with
| (1,_) -> printfn "first part is 1"
| (_,2) -> printfn "second part is 2"
| _ -> printfn "Other case"

// -----------------------
// Record pattern matching
type Person = {First:string; Last:string}
let person = {First="john"; Last="doe"}
match person with
| {First="john"}  -> printfn "Matched John"
| _  -> printfn "Not John"

// -----------------------
// Union pattern matching
type IntOrBool= I of int | B of bool
let intOrBool = I 42
match intOrBool with
| I i  -> printfn "Int=%i" i
| B b  -> printfn "Bool=%b" b


// Matching the whole and the part with the “as” keyword
// Sometimes you want to match the individual components of the value and also the whole thing. You can use the as keyword for this.
let y =
    match (1,0) with
    // binding to three values
    | (x,y) as t ->
        printfn "x=%A and y=%A" x y
        printfn "The whole tuple is %A" t



// Matching on subtypes
// You can match on subtypes, using the :? operator, which gives you a crude polymorphism:
let x5 = new System.Object()
let y5 =
    match x5 with
    | :? System.Int32 ->
        printfn "matched an int"
    | :? System.DateTime ->
        printfn "matched a datetime"
    | _ ->
        printfn "another type"


// Note that in some cases, you may need to “box” the value.

let detectTypeBoxed v =
    match box v with      // used "box v"
        | :? int -> printfn "this is an int"
        | _ -> printfn "something else"
// test
detectTypeBoxed 1
detectTypeBoxed 3.14


// Matching on multiple values
// All the patterns we’ve looked at so far do pattern matching on a single value. How can you do it for two or more?
// The short answer is: you can’t. Matches are only allowed on single values.
// But wait a minute – could we combine two values into a single tuple on the fly and match on that? Yes, we can!
let matchOnTwoParameters x y =
    match (x,y) with
    | (1,y) ->
        printfn "x=1 and y=%A" y
    | (x,1) ->
        printfn "x=%A and y=1" x
    | _ -> printfn "Other"

// And indeed, this trick will work whenever you want to match on a set of values – just group them all into a single tuple.
let matchOnTwoTuples x y =
    match (x,y) with
    | (1,_),(1,_) -> "both start with 1"
    | (_,2),(_,2) -> "both end with 2"
    | _ -> "something else"
// test
matchOnTwoTuples (1,3) (1,2) |> printfn "%A"
matchOnTwoTuples (3,2) (1,2) |> printfn "%A"



// Guards, or the “when” clause
let elementsAreEqual aTuple =
    match aTuple with
    | (x,y) when x=y ->
        printfn "both parts are the same"
    | _ ->
        printfn "both parts are different"

// --------------------------------
// comparing values in a when clause
let makeOrdered aTuple =
    match aTuple with
    // swap if x is bigger than y
    | (x,y) when x > y -> (y,x)
    // otherwise leave alone
    | _ -> aTuple

// test
makeOrdered (1,2) |> ignore
makeOrdered (2,1) |> ignore

// --------------------------------
// testing properties in a when clause
let isAM aDate =
    match aDate:System.DateTime with
    | x when x.Hour <= 12->
        printfn "AM"
    // otherwise leave alone
    | _ ->
        printfn "PM"

// test
isAM System.DateTime.Now

// --------------------------------
// pattern matching using regular expressions
open System.Text.RegularExpressions

let classifyString aString =
    match aString with
    | x when Regex.Match(x,@".+@.+").Success->
        printfn "%s is an email" aString

    // otherwise leave alone
    | _ ->
        printfn "%s is something else" aString


// test
classifyString "alice@example.com"
classifyString "google.com"

// --------------------------------
// pattern matching using arbitrary conditionals
let fizzBuzz x =
    match x with
    | i when i % 15 = 0 ->
        printfn "fizzbuzz"
    | i when i % 3 = 0 ->
        printfn "fizz"
    | i when i % 5 = 0 ->
        printfn "buzz"
    | i  ->
        printfn "%i" i

// test
[1..30] |> List.iter fizzBuzz


// Using active patterns instead of guards
// Guards are great for one-off matches. But if there are certain guards that you use over and over, consider using active patterns instead.
// For example, the email example above could be rewritten as follows:
open System.Text.RegularExpressions
// create an active pattern to match an email address
let (|EmailAddress|_|) input =
   let m = Regex.Match(input,@".+@.+")
   if (m.Success) then Some input else None

// use the active pattern in the match
let classifyString2 aString =
    match aString with
    | EmailAddress x ->
        printfn "%s is an email" x
    // otherwise leave alone
    | _ ->
        printfn "%s is something else" aString

// test
classifyString2 "alice@example.com"
classifyString2 "google.com"


// The “function” keyword
// In the examples so far, we’ve seen a lot of this:
let f1 aValue =
    match aValue with
    | _ -> "something"

// In the special case of function definitions we can simplify this dramatically by using the function keyword.
let f2 =
    function
    | _ -> "something"
// As you can see, the aValue parameter has completely disappeared, along with the match..with.
// This keyword is not the same as the fun keyword for standard lambdas, rather it combines fun and match..with in a single step.


// The function keyword works anywhere a function definition or lambda can be used, such as nested matches:
// using match..with
let f3 aValue =
    match aValue with
    | x ->
        match x with
        | _ -> "something"

// using function keyword
let f4 =
    function
    | x ->
        function
        | _ -> "something"

// or lambdas passed to a higher order function:
// using match..with
[2..10] |> List.map (fun i ->
        match i with
        | 2 | 3 | 5 | 7 -> sprintf "%i is prime" i
        | _ -> sprintf "%i is not prime" i
        ) 
        |> ignore

// using function keyword
[2..10] |> List.map (function
        | 2 | 3 | 5 | 7 -> sprintf "prime"
        | _ -> sprintf "not prime"
        )
        |> ignore

// It's just a shortcut...
let nf =
    function
    | 1 -> "Un"
    | 2 -> "Deux"
    | 3 -> "Trois"
    | _ -> "Très grand"

[1..4] |> List.map nf |> printfn "%A"       // ["Un"; "Deux"; "Trois"; "Très grand"]


// Exception handling with try..with
// The try..with expression implements pattern matching in the same way as match..with.

(
try
    failwith "fail"
with
    | Failure msg -> "caught: " + msg
    | :? System.InvalidOperationException as ex -> "unexpected"
) |> ignore

// we can also use guards as well, if needed to add extra conditional logic:

// let debugMode = false
// try
//     failwith "fail"
// with
//     | Failure msg when debugMode  ->
//         reraise()
//     | Failure msg when not debugMode ->
//         printfn "silently logged in production: %s" msg


// match expressions doesn’t compose very well. That is, it is hard to chain match..with expressions and build simple
// ones into complex ones.
// The best way of avoiding this is to wrap match..with expressions into functions, which can then be composed nicely.

let isAnswerToEverything x =
    match x with
    | 42 -> (x,true)
    | _ -> (x,false)

// the function can be used for chaining or composition
let times6 x = x * 6
[1..10] |> List.map (times6 >> isAnswerToEverything) |> printfn "%A"


// Library functions to replace explicit matching
// Most built-in F# types have such functions already available.
// For example, instead of using recursion to loop through lists, you should try to use the functions in the List
// module, which will do almost everything you need.
// In particular, the function we wrote earlier:
let rec loopAndSum0 aList sumSoFar =
    match aList with
    | [] -> sumSoFar
    | x::xs ->
        let newSumSoFar = sumSoFar + x
        loopAndSum xs newSumSoFar

// can be rewritten using the List module in at least three different ways!

// simplest
let loopAndSum1 aList = List.sum aList
[1..10] |> loopAndSum1 |> ignore

// reduce is very powerful
let loopAndSum2 aList = List.reduce (+) aList
[1..10] |> loopAndSum2 |> ignore

// fold is most powerful of all
let loopAndSum3 aList = List.fold (fun sum i -> sum+i) 0 aList
[1..10] |> loopAndSum3 |> ignore


// a function that does a match on Some vs None can be replaced with Option.map
// unnecessary to implement this explicitly
let addOneIfValid optionalInt =
    match optionalInt with
    | Some i -> Some (i + 1)
    | None -> None

Some 42 |> addOneIfValid |> ignore

// much easier to use the built in function
let addOneIfValid2 optionalInt =
    optionalInt |> Option.map (fun i->i+1)

Some 42 |> addOneIfValid2 |> ignore


// Creating “fold” functions to hide matching logic

// Finally, if you create your own types which need to be frequently matched, it is good practice to create a
// corresponding generic “fold” function that wraps it nicely.
// For example, here is a type for defining temperature.
type TemperatureType  = F of float | C of float

// Chances are, we will matching these cases a lot, so let’s create a generic function that will do the matching for us.
module Temperature =
    let fold fahrenheitFunction celsiusFunction aTemp =
        match aTemp with
        | F f -> fahrenheitFunction f
        | C c -> celsiusFunction c

// All fold functions follow this same general pattern:
// • there is one function for each case in the union structure (or clause in the match pattern)
// • finally, the actual value to match on comes last. (Why? See the post on “designing functions for partial application”)

// Now we have our fold function, we can use it in different contexts. Let’s start by testing for a fever. We need a
// function for testing degrees F for fever and another one for testing degrees C for fever. And then we combine them
// both using the fold function.

let fFever tempF =
    if tempF > 100.0 then "Fever!" else "OK"
let cFever tempC =
    if tempC > 38.0 then "Fever!" else "OK"

// combine using the fold
let isFever aTemp = Temperature.fold fFever cFever aTemp

// And now we can test.
let normalTemp = C 37.0
let result1 = isFever normalTemp
let highTemp = F 103.1
let result2 = isFever highTemp


// For a completely different use, let’s write a temperature conversion utility. Again we start by writing the functions
// for each case, and then combine them.

let fConversion tempF =
    let convertedValue = (tempF - 32.0) / 1.8
    TemperatureType.C convertedValue    //wrapped in type

let cConversion tempC =
    let convertedValue = (tempC * 1.8) + 32.0
    TemperatureType.F convertedValue    //wrapped in type
// combine using the fold

let convert aTemp = Temperature.fold fConversion cConversion aTemp

// Note that the conversion functions wrap the converted values in a new TemperatureType, so the convert function has the signature:
// val convert : TemperatureType -> TemperatureType

let c20 = C 20.0
let resultInF = convert c20
let f75 = F 75.0
let resultInC = convert f75

// We can even call convert twice in a row, and we should get back the same temperature that we started with!

let resultInC2 = C 20.0 |> convert |> convert
