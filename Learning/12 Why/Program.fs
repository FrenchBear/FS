// 12 Why
// Learning F#, Exercises based on "Why using F#?"
//
// 2024-07-05   PV

type Point = Coords of X:double * Y:double

type Shape =
| Point of P:Point
| Line of P1:Point * P2:Point
| Rectangle of P1:Point * P2:Point
| Triangle of P1:Point * P2:Point * P3: Point
| Circle of Center:Point * Radius:double

let p1 = Coords (2.0, 3.0)
let p2 = Coords (4.0, -1.0)
let p3 = Coords (-1.0, 2.0)

let P = Point p1
let L = Line (p1, p2)
let R = Rectangle (p1, p2)
let T = Triangle (p1, p2, p3)
let C = Circle (p1, 3.14)

let printShape =
    function
    | Point p -> printfn "Point %A" p
    | Line (p1, p2) -> printfn "Line (%A, %A)" p1 p2
    | Rectangle (p1, p2) -> printfn "Rectangle (%A, %A)" p1 p2
    | Triangle (p1, p2, p3) -> printfn "Triangle (%A, %A, %A)" p1 p2 p3
    | Circle (c, r) -> printfn "Circle center %A, radius %g)" c r

[P;L;R;T;C] |> List.map printShape |> ignore
printfn ""


// --------------------------------
// Example of date format supporting stuff such as Absence or partial date (just month and year for instance)

type YMDate = {Year:int; Month:int}

type ExtendedDate =
| Empty                             // Nothing printed on the report
| Absence                           // "Absence" printed on the report
| YMDDate of System.DateOnly        // "Normal" D/M/Y date
| YMDate of YMDate                  // Just year and month
| YDate of int                      // Just year

let d1 = Empty
let d2 = Absence
let d3 = YMDDate (System.DateOnly(2024,07,14))
let d4 = YMDate {Year=2024; Month=7}
let d5 = YDate 2024

let printExtendedDate d =
    let extendedDateToString =
        function
        | Empty -> ""
        | Absence -> "Absence"
        | YMDDate fullDate -> fullDate.ToString("dd/MM/yyyy")
        | YMDate {Year=y;Month=m} ->
            let dt = System.DateOnly(y,m,1)
            dt.ToString("MMMM yyyy")            // juillet 2024
        | YDate y when y<100 -> (y+2000).ToString()
        | YDate y -> y.ToString()               // Using 'when y>=100' filter causes incomplete matches warning

    "Date: "+(extendedDateToString(d))

[d1;d2;d3;d4;d5] |> List.map (printExtendedDate >> printfn "%s") |> ignore
printfn ""


// --------------------------------
// List folfding: List.fold
// Signature: List.fold : ('State -> 'T -> 'State) -> 'State -> 'T list -> 'State
// Usage: List.fold folder state list
// state can be a simple value (ex: an int or a float for a sum) or a more complex state, class instance or tuple

// Fold pattern, remember it (if list is empty, just return initial state
let action p x = p*x
let initialState = 1
let liste = [1;2;3;4]
List.fold action initialState liste |> printfn "%A"


let product n =
    let initialValue = 1
    let action productSoFar x = productSoFar * x
    [1..n] |> List.fold action initialValue
product 10 |> printfn "10! = %d"

let sumOfOdds n =
    let initialValue = 0
    let action sumSoFar x = if x%2=0 then sumSoFar else sumSoFar+x
    [1..n] |> List.fold action initialValue
sumOfOdds 10 |> printfn "sumOfOdds(10) = %d"

let alternatingSum n =
    let initialValue = (true,0)
    let action (isNeg,sumSoFar) x = if isNeg then (false,sumSoFar-x)
                                             else (true ,sumSoFar+x)
    [1..n] |> List.fold action initialValue |> snd
alternatingSum 100 |> printfn "alternatingSum(100) = %d"


(* "Modern" C# Version 

    static void Main(string[] args)
    {
        Console.WriteLine($"10! = {Product(10)}");
        Console.WriteLine($"SumOfOdds(10) = {SumOfOdds(10)}");
        Console.WriteLine($"AlternatingSum(100) = {AlternatingSum2(100)}");
    }

    public static int Product(int n) => Enumerable.Range(1, n).Aggregate(1, (prd, x) => prd * x);

    // With a generic method for multiplication (only consider T*T->T to simplify)
    static T Mult<T>(T a, T b) where T:IMultiplyOperators<T,T,T> => a * b;
    public static int Product2(int n) => Enumerable.Range(1, n).Aggregate(1, Mult);


    public static int SumOfOdds(int n) => Enumerable.Range(1, n).Where(x => x % 2 != 0).Aggregate(0, (sum, x) => sum + x);

    // First version, where neg is an external variable captured by the lambda
    public static int AlternatingSum1(int n)
    {
        var neg = true;
        return Enumerable.Range(1, n).Aggregate(0, (sum, x) =>
        {
            sum += neg ? -x : x;
            neg = !neg;
            return sum;
        });
    }

    // Second version, neg is included in "current state" which is a tuple
    public static int AlternatingSum2(int n)
        => Enumerable.Range(1, n).Aggregate((0,true), (sum, x) => (sum.Item1 + (sum.Item2 ? -x : x), !sum.Item2)).Item1;

    // Variation
    public static int AlternatingSum3(int n)
        => Enumerable.Range(1, n).Aggregate((0, true), (sum, x) => sum.Item2 ? (sum.Item1 - x, false) : (sum.Item1 + 1, true)).Item1;

*)

// F# can be shorter
let product2 n = [1..n] |> List.fold (fun x y -> x*y) 1
product2 10 |> printfn "10! = %d"

printfn ""


// Example of π calculation using development of atan(1.0) = π/4 = 1-1/3+1/5-1/7+1/9-...
type Totaux = {SumPlus:double; SumMinus:double}
let actionPlusMinus currentState n = 
    let sp = currentState.SumMinus + 1.0/double(n)
    let sm = sp - 1.0/double(n+2)
    {SumPlus=sp; SumMinus=sm}
let startTotaux = {SumPlus=0.0; SumMinus=0.0}
let endTotaux = [1..4..1000] |> List.fold actionPlusMinus startTotaux
let pp = 4.0*endTotaux.SumPlus
let pm = 4.0*endTotaux.SumMinus
let ph = (pp+pm)/2.0
printfn "π+ = %f\nπ½ = %f\nπ- = %f\n" pp ph pm


// --------------------------------
// MaxBy

type NameAndSize = {Name:string; Size:int}

let maxNameAndSize list =
    let innerMaxAndSize initialvalue rest =
        let action maxSoFar x = if maxSoFar.Size<x.Size then x else maxSoFar
        rest |> List.fold action initialvalue

    match list with
    | [] -> None
    | first::rest ->
        let max = innerMaxAndSize first rest
        Some max

let list = [
    {Name="Alice"; Size=10}
    {Name="Bob"; Size=1}
    {Name="Carol"; Size=12}
    {Name="David"; Size=5}
]
maxNameAndSize list |> printfn "Max1 = %A"


//let Max2 list =
//    let innerMax initialValue rest =
//        let action maxSoFar x = if x.Size>maxSoFar.Size then x else maxSoFar
//        List.fold action initialValue rest

//    match list with
//    | [] -> None
//    | head::tail ->
//        let max = innerMax head tail
//        Some max

// MaxBy is integrated in F#, but doesn't handle empty lists well
// [] |> List.maxBy (fun item -> item.Size) |> ignore
// System.ArgumentException: 'The input sequence was empty.

// Signature: List.maxBy : ('T -> 'U) -> 'T list -> 'T (requires comparison)
// Usage: List.maxBy projection list

let maxNameAndSize2 liste =
    match list with
    | [] -> None
    | _ -> liste |> List.maxBy (fun item -> item.Size) |> Some
  
maxNameAndSize2 list |> printfn "Max2 = %A"
printfn ""


// --------------------------------
// Functions as building blocks

let add2 x = x + 2
let mult3 x = x * 3
let square x = x * x

let logMsg msg x = printf "%s%i" msg x; x     // without linefeed
let logMsgN msg x = printfn "%s%i" msg x; x   // with linefeed

// new composed function with new improved logging!
let mult3ThenSquareLogged =
   logMsg "before="
   >> mult3
   >> logMsg " after mult3="
   >> square
   >> logMsgN " result="

mult3ThenSquareLogged 5 |> ignore



// Example of composition operator to collapse a list of cunctions into a single operation
let listOfFunctions = [
   mult3;
   square;
   add2;
   logMsgN "result=";
   ]

// compose all functions in the list into a single one
let allFunctions = List.reduce (>>) listOfFunctions

// List.reduce:
// Applies a function to each element of the collection, threading an accumulator argument through the computation. 
// This function first applies the function to the first two elements of the list. Then, it passes this result into the
// function along with the third element and so on. Finally, it returns the final result. If the input function is f and
// the elements are i0...iN, then it computes f (... (f i0 i1) i2 ...) iN.
// Signature: List.reduce : ('T -> 'T -> 'T) -> 'T list -> 'T
// Usage: List.reduce reduction list

// test
allFunctions 5 |> printfn "affFunctions 5 = %d"
printfn ""


// --------------------------------
// Compare List.fold and List.reduce

// List.fold takes an initial state and combine each value of the list with this state to produce next state.
// result is returned after final state

let values2 = [1;2;3;4;5]
let initialState2 = 1
let action2 currentState newValue = currentState*newValue
values2 |> List.fold action2 initialState2 |> printfn "List.fold -> %d"

// List.reduce applies a function to each element of the collection, on forst two elements... up to the last
values2 |> List.reduce (*) |> printfn "List.reduce -> %d"


let π = 4.0*atan(1.0)
let dToRConst = π/180.0
let rToDConst = 180.0/π

let dToR = (*) dToRConst
let rToD = (*) rToDConst

let sind = dToR >> sin
let cosd = dToR >> cos
let tand = dToR >> tan

let asind = asin >> rToD
let acosd = acos >> rToD
let atand = atan >> rToD

let trigTest = [sind;cosd;tand;atand;acosd;asind]

// Using List.reduce
let op = List.reduce (>>) trigTest
let res9 = op 9.0
let err9 = res9-9.0
printfn "Trig test List.reduce: res=%f err=%g" res9 err9

// Using List.fold
let foldAction currentState trigFn = currentState |> trigFn
let res9b = List.fold foldAction 9.0 trigTest
let err9b = res9b-9.0
printfn "Trig test List.fold: res=%f err=%g\n" res9b err9b



// --------------------------------
// Mini-language

// set up the vocabulary
type DateScale = Hour | Hours | Day | Days | Week | Weeks
type DateDirection = Ago | Hence

// define a function that matches on the vocabulary
let getDate interval scale direction =
    let absHours = match scale with
                   | Hour | Hours -> 1 * interval
                   | Day | Days -> 24 * interval
                   | Week | Weeks -> 24 * 7 * interval
    
    let signedHours = match direction with
                      | Ago -> -1 * absHours
                      | Hence ->  absHours
    
    System.DateTime.Now.AddHours(float signedHours)

// test some examples
getDate 5 Days Ago |> printfn "5 days ago: %A"
getDate 1 Hour Hence  |> printfn "1 hour hence: %A"
printfn ""


// create an underlying type
type FluentShape = {
    label : string;
    color : string;
    onClick : FluentShape->FluentShape } // a function type

// some basic functions:
let defaultShape = {label=""; color=""; onClick=fun shape->shape}
let click shape = shape.onClick shape
let display shape =
    printfn "My label=%s and my color=%s" shape.label shape.color
    shape   // return same shape

// some helper functions which we expose as the “mini-language”, and will be used as building blocks by the users of the language.
let setLabel label shape = {shape with FluentShape.label = label}
let setColor color shape = {shape with FluentShape.color = color}
// add a click action to what is already there: takes a function as a parameter and composes it with the existing click action
let appendClickAction action shape = {shape with FluentShape.onClick = shape.onClick >> action}


// Now as a user of this “mini-language”, I can compose the base helper functions into more complex functions of my own,
// creating my own function library. (In C# this kind of thing might be done using extension methods.)

// Compose two "base" functions to make a compound function.
let setRedBox = setColor "red" >> setLabel "box"

// Create another function by composing with previous function.
// It overrides the color value but leaves the label alone.
let setBlueBox = setRedBox >> setColor "blue"

// Make a special case of appendClickAction
let changeColorOnClick color = appendClickAction (setColor color)

// setup some test values
let redBox = defaultShape |> setRedBox
let blueBox = defaultShape |> setBlueBox

// create a shape that changes color when clicked
redBox
    |> display
    |> changeColorOnClick "green"
    |> click
    |> display  // new version after the click
    |> ignore
printfn ""

// create a shape that changes label and color when clicked
blueBox
    |> display
    |> appendClickAction (setLabel "box2" >> setColor "green")
    |> click
    |> display  // new version after the click
    |> ignore
printfn ""

// Here is a more complex example. We will create a function “showRainbow” that, for each color in the rainbow, sets the
// color and displays the shape.
let rainbow =
    ["red";"orange";"yellow";"green";"blue";"indigo";"violet"]

let showRainbow =
    let setColorAndDisplay color = setColor color >> display
    rainbow
    |> List.map setColorAndDisplay
    |> List.reduce (>>)

// test the showRainbow function
defaultShape |> showRainbow |> ignore
printfn ""



// --------------------------------
// Pattern-matching

// matching tuples directly
let firstPart, secondPart, _ =  (1,2,3) // underscore means ignore

//#nowarn "25"                          // Deactivating a warning is for the whole file...
// matching lists directly
let elem1::elem2::rest = [1..10]        // ignore the warning for now

// matching lists inside a match..with
let listMatcher aList =
    match aList with
    | [] -> printfn "the list is empty"
    | [firstElement] -> printfn "the list has one element %A " firstElement
    | [first; second] -> printfn "list is %A and %A" first second
    | _ -> printfn "the list has more than two elements"

listMatcher [1;2;3;4]
listMatcher [1;2]
listMatcher [1]
listMatcher []


// create some types and a customer
type Address = { Street: string; City: string; }
type Customer = { ID: int; Name: string; Address: Address }
let customer1 = { ID = 1; Name = "Bob"; Address = {Street="123 Main"; City="NY" }}

// extract name only
let { Name=name1a } = customer1
printfn "The customer is called %s" name1a

type Animal = {Name: string; Race: string}      // Test when we reuse field Name in a different struct
// extract name only, need to use qualified member name
let { Customer.Name=name1b } = customer1
printfn "The customer is called %s" name1b

// extract name and id, since ID is not in Animal, there's no ambiguity here
let { ID=id2; Name=name2; } =  customer1
printfn "The customer called %s has id %i" name2 id2

// extract name and address
let { Name=name3;  Address={Street=street3}  } =  customer1
printfn "The customer is called %s and lives on %s" name3 street3


// Matching simple type of a variable (ca&n't use a "int i" match case contrary to C#)
let typ v =
    match v with
    | i when i.GetType()=typeof<int> -> printfn "Integer %A" i
    | d when d.GetType()=typeof<double> -> printfn "Double %A" d
    | _ -> printfn "other"

typ 42
typ (box 42)
typ 42.5
typ true

// Doesn't work
// let Test (o: 'T) =
//     let res =
//         match o with
//         | :? int32 as i -> $"Integer {i}"
//         | :? int16 as s -> $"Short {s}"
//         | :? bool as b -> "xx"
//         | _ -> "Other"
//     printfn "%s" res
// 
// Test 42
// Test (box 45)
// Test 33s

printfn ""


// --------------------------------
// Active Patterns

let (|Int|_|) str =
   match System.Int32.TryParse(str:string) with
   | (true,int) -> Some(int)
   | _ -> None

let (|Bool|_|) str =
   match System.Boolean.TryParse(str:string) with
   | (true,bool) -> Some(bool)
   | _ -> None

// Once these patterns have been set up, they can be used as part of a normal “match..with” expression.
let testParse str =
    match str with
    | Int i -> printfn "The value is an int '%i'" i
    | Bool b -> printfn "The value is a bool '%b'" b
    | _ -> printfn "The value '%s' is something else" str

testParse "12"
testParse "true"
testParse "abc"


// create an active pattern
open System.Text.RegularExpressions
let (|FirstRegexGroup|_|) pattern input =
   let m = Regex.Match(input,pattern)
   if (m.Success) then Some m.Groups.[1].Value else None

let testRegex str =
    match str with
    | FirstRegexGroup "http://(.*?)/(.*)" host ->
           printfn "The value is a url and the host is %s" host
    | FirstRegexGroup ".*?@(.*)" host ->
           printfn "The value is an email and the host is %s" host
    | _ -> printfn "The value '%s' is something else" str

testRegex "http://google.com/test"
testRegex "alice@hotmail.com"


// Active pattern with no parameters
// FizzBuzz challenge written using active patterns
let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

let fizzBuzz i =
  match i with
  | MultOf3 & MultOf5 -> printf "FizzBuzz, "
  | MultOf3 -> printf "Fizz, "
  | MultOf5 -> printf "Buzz, "
  | _ -> printf "%i, " i

// test
[1..20] |> List.iter fizzBuzz
printfn "\n"


let (|Digit|Letter|Whitespace|Other|) ch =
   if System.Char.IsDigit(ch) then Digit
   else if System.Char.IsLetter(ch) then Letter
   else if System.Char.IsWhiteSpace(ch) then Whitespace
   else Other

let printChar ch =
  match ch with
  | Digit -> printfn "%c is a Digit" ch
  | Letter -> printfn "%c is a Letter" ch
  | Whitespace -> printfn "%c is a Whitespace" ch
  | _ -> printfn "%c is something else" ch

// print a list
['a';'b';'1';' ';'-';'c'] |> List.iter printChar
printfn ""


// --------------------------------
// Exhaustive pattern matching as an error handling technique

type Result<'a, 'b> = // define a "union" of two different alternatives
    | Success of 'a   // 'a means generic type. The actual type will be determined when it is used.
    | Failure of 'b   // generic failure type as well
//    | Indeterminate

// define all possible errors
type FileErrorReason =
    | FileNotFound of string
    | UnauthorizedAccess of string * System.Exception

// define a low level function in the bottom layer
let performActionOnFile action filePath =
   try
      // open file, do the action and return the result
      use sr = new System.IO.StreamReader(filePath:string)
      let result = action sr  // do the action to the reader
      //match System.IO.Path.GetExtension(filePath).ToLower() with
      //| ".txt" -> Indeterminate
      //| _ -> Success(result)
      Success (result)        // return a Success
   with      // catch some exceptions and convert them to errors
      | :? System.IO.FileNotFoundException as ex
          -> Failure (FileNotFound filePath)
      | :? System.Security.SecurityException as ex
          -> Failure (UnauthorizedAccess (filePath,ex))
      // other exceptions are unhandled

// The code demonstrates how performActionOnFile returns a Result object which has two alternatives: Success and
// Failure. The Failure alternative in turn has two alternatives as well: FileNotFound and UnauthorizedAccess.

// Now the intermediate layers can call each other, passing around the result type without worrying what its structure
// is, as long as they don’t access it:

// a function in the middle layer
let middleLayerDo action filePath =
    let fileResult = performActionOnFile action filePath
    // do some stuff
    fileResult // return

// a function in the top layer
let topLayerDo action filePath =
    let fileResult = middleLayerDo action filePath
    // do some stuff
    fileResult // return


// Because of type inference, the middle and top layers do not need to specify the exact types returned. If the lower
// layer changes the type definition at all, the intermediate layers will not be affected.

// Obviously at some point, a client of the top layer does want to access the result. And here is where the requirement
// to match all patterns is enforced. The client must handle the case with a Failure or else the compiler will complain.
// And furthermore, when handling the Failure branch, it must handle the possible reasons as well. In other words,
// special case handling of this sort can be enforced at compile time, not at runtime! And in addition the possible
// reasons are explicitly documented by examining the reason type.
// Here is an example of a client function that accesses the top layer:

// get the first line of the file
let printFirstLineOfFile filePath =
    let fileResult = topLayerDo (fun fs->fs.ReadLine()) filePath
    match fileResult with
    | Success result ->
        // note type-safe string printing with %s
        printfn "first line is: '%s'" result
    | Failure reason ->
       match reason with  // must match EVERY reason
       | FileNotFound file ->
           printfn "File not found: %s" file
       | UnauthorizedAccess (file,_) ->
           printfn "You do not have access to the file: %s" file

// You can see that this code must explicitly handle the Success and Failure cases, and then for the failure case, it
// explicitly handles the different reasons. If you want to see what happens if it does not handle one of the cases, try
// commenting out the line that handles UnauthorizedAccess and see what the compiler says.

// Now it is not required that you always handle all possible cases explicitly. In the example below, the function uses
// the underscore wildcard to treat all the failure reasons as one. This can be considered bad practice if we want to
// get the benefits of the strictness, but at least it is clearly done.

// get the length of the text in the file
let printLengthOfFile filePath =
   let fileResult =
     topLayerDo (fun fs->fs.ReadToEnd().Length) filePath
   match fileResult with
   | Success result ->
      // note type-safe int printing with %i
      printfn "length is: %i" result
   | Failure _ ->
      printfn "An error happened but I don't want to be specific"

// Now let’s see all this code work in practice with some interactive tests.
// First set up a good file and a bad file.
// write some text to a file
let writeSomeText filePath someText =
    use writer = new System.IO.StreamWriter(filePath:string)
    writer.WriteLine(someText:string)

let goodFileName = @"C:\Temp\good.txt"
let badFileName = @"C:\Temp\bad.txt"
writeSomeText goodFileName "hello"

// And now test interactively:
printFirstLineOfFile goodFileName
printLengthOfFile goodFileName

printFirstLineOfFile badFileName
printLengthOfFile badFileName

printfn ""



// --------------------------------
// Units of measure

// define some measures
[<Measure>]
type cm

[<Measure>]
type inches

[<Measure>]
type feet =
   // add a conversion function
   static member toInches(feet : float<feet>) : float<inches> = feet * 12.0<inches/feet>

// Add other conversion
type cm with
    static member toInches(cm: float<cm>): float<inches> = cm / 2.54<cm/inches>

printfn "186 cm = %g inches" (cm.toInches(186.0<cm>))

// define some values
let meter = 100.0<cm>
let yard = 3.0<feet>

// convert to different measure
let yardInInches = feet.toInches(yard)

// can't mix and match!
// yard + meter

// now define some currencies
[<Measure>]
type GBP

[<Measure>]
type USD

let gbp10 = 10.0<GBP>
let usd10 = 10.0<USD>
gbp10 + gbp10 |> ignore         // allowed: same currency
// gbp10 + usd10                // not allowed: different currency
// gbp10 + 1.0                  // not allowed: didn't specify a currency
gbp10 + 1.0<_> |> ignore        // allowed using wildcard
gbp10 + 1.0<GBP>  |> ignore     // Same unit, Ok!

printfn ""



// --------------------------------
// Stop a type being compared

// deny comparison
[<NoEquality; NoComparison>]
type CustomerAccount = {CustomerAccountId: int}

let x = {CustomerAccountId = 1}

// Only work for global type, not members
// x = x       // error!
x.CustomerAccountId = x.CustomerAccountId |> ignore // no error



// --------------------------------
// Creating objects from interface

// create a new object that implements IDisposable
let makeResource name =
    { new System.IDisposable with
        member this.Dispose() = printfn "%s disposed" name 
    }

let useAndDisposeResources =
    use r1 = makeResource "first resource"
    printfn "using first resource"
    for i in [1..3] do
        let resourceName = sprintf "inner resource %d" i
        use temp = makeResource resourceName
        printfn "    do something with %s" resourceName
    use r2 = makeResource "second resource"
    printfn "using second resource"
    printfn "done."
printfn ""



// --------------------------------
// Mixing .NET interfaces with pure F# types

// The ability to create instances of an interface on the fly means that it is easy to mix and match interfaces from
// existing APIs with pure F# types.
// For example, say that you have a preexisting API which uses the IAnimal interface, as shown below.
type IAnimal =
   abstract member MakeNoise : unit -> string

let showTheNoiseAnAnimalMakes (animal:IAnimal) =
   animal.MakeNoise() |> printfn "Making noise %s"

// But we want to have all the benefits of pattern matching, etc., so we have created pure F# types for cats and dogs
// instead of classes.
type Cat = Felix | Garfield
type Dog = Butch | Lassie

// But using this pure F# approach means that that we cannot pass the cats and dogs to the showTheNoiseAnAnimalMakes
// function directly.
// However, we don’t have to create new sets of concrete classes just to implement IAnimal. Instead, we can dynamically
// create the IAnimal interface by extending the pure F# types.

// now mixin the interface with the F# types
type Cat with
   member this.AsAnimal =
        { new IAnimal
          with member a.MakeNoise() = "Meow" }

type Dog with
   member this.AsAnimal =
        { new IAnimal
          with member a.MakeNoise() = "Woof" }

let dog = Lassie
showTheNoiseAnAnimalMakes (dog.AsAnimal)

let cat = Felix
showTheNoiseAnAnimalMakes (cat.AsAnimal)

printfn ""



// --------------------------------
// Using reflection to examine F# types

// F# gets the benefit of the .NET reflection system, which means that you can do all sorts of interesting things that
// are not directly available to you using the syntax of the language itself. The Microsoft.FSharp.Reflection namespace
// has a number of functions that are designed to help specifically with F# types.
// For example, here is a way to print out the fields in a record type, and the choices in a union type.

open System.Reflection
open Microsoft.FSharp.Reflection

// create a record type...
type Account = {Id: int; Name: string}

// ... and show the fields
let fields =
    FSharpType.GetRecordFields(typeof<Account>)
    |> Array.map (fun propInfo -> propInfo.Name, propInfo.PropertyType.Name)
printfn "%A" fields

// create a union type...
type public Choices = | A of int | B of string

// ... and show the choices
let choices =
    FSharpType.GetUnionCases(typeof<Choices>)
    |> Array.map (fun choiceInfo -> choiceInfo.Name)
printfn "%A" choices
