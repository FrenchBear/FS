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
