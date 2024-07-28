// 22 Code organization
// Learning F#, Cheatsheet, Code organization
//
// 2024-07-28   PV


module Money =
    type CardInfo =
        { number: string
          expiration: int * int }

    type Payment =
        | Card of CardInfo
        | Cash of int

    module Functions =
        let validCard (cardNumber: string) =
            cardNumber.Length = 16 && (cardNumber[0], ['3';'4';'5';'6']) ||> List.contains

// Use module prefixing with name.
let ci:Money.CardInfo = {number="1234567890123456"; expiration=(01,26)}
let pa = Money.Payment.Card ci
let ans1 = Money.Functions.validCard ci.number


// External modules as well
let ans2 = Functions.sumOfSquares1 10

// If there's a namespace, just prefix it with namespace.
let ans3 = MathSpace.Math3.cube 10
let ans4 = MathSpace.Advanced.Math4.fourth 10



// Open and AutoOpen

module Groceries =
    type Fruit = Apple|Banana

let fruit1 = Groceries.Banana

open Groceries
let fruit2 = Apple      // Now Groceries content is in scope

open System.Diagnostics
let sw = Stopwatch.StartNew

open type System.Text.RegularExpressions.Regex   // Can open a type with "open type", kind of using static in C#
let isHttp = IsMatch("^https?:", "C:AUTOEXEC.BAT")

open type System.Console
WriteLine("Hello, world!")


[<AutoOpen>]    // No need for open... But there's no warning for shadowing a former definition of a name
module Groceries2 =
    type Vegetable = Potato|Carrot|Spinach

let todayVeg = Spinach



// Accessibility modifiers
// With the exception of let bindings in a class type, everything defaults to public.

module internal MyModule =
    let half x = x>>>1

let private value = sqrt(2.0)

type internal MyRecord = { id: int }

type MyPrivateRecord = private { id: int }

type internal MyDiscUni = A | B

type MyDiscUni2 = private A | B

type internal MyClass() =
    member this.hello = printfn "Hello"

type MyClass2 private (s:string) =                  // Primary constructor is private, can't call it from ouside
    let mutable myInt1 = 10

    member this.world() = printfn "%s" s

    internal new() = MyClass2("defaultValue")       // Extra constructors must call the primary constructor

    public new(i:int) = MyClass2(i.ToString())

    member private _.privateMenber x = 2*x

    [<DefaultValue>] val mutable internal myInitializedInt:int 

// let mc2a = MyClass2("Pierre")    // No, default constructor is private
let mc2b = MyClass2()               // parameterless constructor is internal, Ok
let mc2c = MyClass2(123)            // constructor(int) is public, Ok


// Smart constructors
// Example of Single-case Discriminated Union with a private constructor that constrains a quantity between 0 and 100:

type UnitQuantity =
    private UnitQuantity of int

module UnitQuantity =  // common idiom: type companion module
    let tryCreate qty =
        if qty < 1 || qty > 100
        then None
        else Some (UnitQuantity qty)
    let value (UnitQuantity uQty) = uQty
    let zero = UnitQuantity 0

let unitQtyOpt = UnitQuantity.tryCreate 5
let validQty = unitQtyOpt |> Option.defaultValue UnitQuantity.zero


// defaultArg and Option.defaultValue have reversed arguments...
let bizOpt = Some 52
let biz = defaultArg bizOpt 0

let fibOpt = Some 52
let fib = Option.defaultValue 0 fibOpt



// Recursive Reference

// F#'s type inference and name resolution runs in file and line order. By default, any forward references are errors.
// However, in some cases forward referencing might be needed. To do this we have rec for module and namespace; and and
// for type and let (Recursive Functions) functions.

module rec CarModule =
    exception OutOfGasException of Car  // Car not defined yet; would be an error
    type Car =
        { make: string; model: string; hasGas: bool }
        member self.Drive destination =
            if not self.hasGas
            then raise (OutOfGasException self)
            else "Ok"

// Mutually referencing types must be declared together using and
type Personne =
    { Name: string; Address: Address }
and Address =
    { Line1: string; Line2: string; Occupant: Personne }
