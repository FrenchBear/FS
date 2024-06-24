// 03 Attaching members
// Learning F#, Attaching members and functions
//
// 2024-06-18   PV

module Person =
    type T = { First: string; Last: string } with

        // member defined with type declaration
        member this.FullName = this.First + " " + this.Last

    // constructor
    let create first last : T = { First = first; Last = last }

// test
let person = Person.create "John" "Doe"
let fullname = person.FullName


module Person2 =
    // If type def is indented, with keywork is not necessary...
    type T = 
        { First: string; Last: string }

        // member defined with type declaration
        member this.FullName = this.First + " " + this.Last

    // constructor
    let create first last = { First = first; Last = last }

    // another member added later
    type T with
        member this.SortableName = this.Last + ", " + this.First

// test
let person2 = Person2.create "John" "Doe"
let fullname2 = person2.FullName
let sortableName = person2.SortableName


// Add an UppercaseName extension to Person in a different module:
module PersonExtensions =
    type Person.T with
        member this.UppercaseName =
            this.FullName.ToUpper()

// bring the extension into scope first!
open PersonExtensions

let personExt = Person.create "John" "Doe"
let uppercaseName = personExt.UppercaseName



// ------------------------------------------------------------------
// Extending system types
type System.Int32 with
    member this.IsEven = this % 2 = 0

//test
let i = 20
if i.IsEven then printfn "'%i' is even" i



// ------------------------------------------------------------------
// Static members (add static, drop this.)
module Person3 =
    type T = {First:string; Last:string} with
        // member defined with type declaration
        member this.FullName = this.First + " " + this.Last

        // static constructor
        static member Create first last = {First=first; Last=last}

// test
let person3 = Person3.T.Create "John" "Doe"
let fullname3 = person3.FullName


// ------------------------------------------------------------------
// Create static members for system types
type System.Int32 with
    static member IsOdd x = x % 2 = 1

type System.Double with
    static member Pi = 3.141

//test
let result = System.Int32.IsOdd 20
let pi = System.Double.Pi


// ------------------------------------------------------------------
// Attaching existing functions

module Person4 =
    // type with no members initially
    type T = {First:string; Last:string}

    // constructor
    let create first last = {First=first; Last=last}

    // standalone function
    let fullName {First=first; Last=last} = first + " " + last

    // attach preexisting function as a member
    type T with
        member this.FullName = fullName this

// test
let person4 = Person4.create "John" "Doe"
let fullname4 = Person4.fullName person4    // functional style
let fullname4b = person4.FullName           // OO style, allower by final attach


// ------------------------------------------------------------------
// Attaching existing functions with multiple parameters

// you don’t have to respecify them all when doing the attachment, as long as the this parameter is first.
// In the example below, the hasSameFirstAndLastName function has three parameters. Yet when we attach it, we only need to specify one!

module Person5 =
    // type with no members initially
    type T = {First:string; Last:string}

    // constructor
    let create first last = {First=first; Last=last}

    // standalone function
    let hasSameFirstAndLastName (person:T) otherFirst otherLast = person.First = otherFirst && person.Last = otherLast

    // attach preexisting function as a member, only specify "this"
    type T with
        member this.HasSameFirstAndLastName = hasSameFirstAndLastName this

// test
let person5 = Person5.create "John" "Doe"
let result51 = Person5.hasSameFirstAndLastName person5 "bob" "smith"    // functional style
let result52 = person5.HasSameFirstAndLastName "bob" "smith"            // OO style


// ------------------------------------------------------------------
// Attaching members to tuple-form methods

type Product = {SKU:string; Price: float} with
    // curried style
    member this.CurriedTotal qty discount = (this.Price * float qty) - discount
    // tuple style
    member this.TupleTotal(qty, discount) = (this.Price * float qty) - discount

let productA = {SKU="ABC"; Price=2.0}
let total1A = productA.CurriedTotal 10 1.0
let total2A = productA.TupleTotal(10,1.0)
// No differences so far


// curried version can be partially applied
let totalFor10 = productA.CurriedTotal 10
let discounts = [1.0..5.0]
let totalForDifferentDiscounts
    = discounts |> List.map totalFor10

// But the tuple approach can do a few things that that the curried one can’t, namely:
// • Named parameters
// • Optional parameters
// • Overloading

// The tuple-style approach supports named parameters:
let productB = {SKU="ABC"; Price=2.0}
let total1B = productB.TupleTotal(qty=10, discount=1.0)
let total2B = productB.TupleTotal(discount=1.0, qty=10)       // when names are used, the parameter order can be changed.


// Optional parameters with tuple-style parameters
// For tuple-style methods, you can specify an optional parameter by prefixing the parameter name with a question mark.
// • If the parameter is set, it comes through as Some value
// • If the parameter is not set, it comes through as None

type Product2 = {SKU:string; Price: float} with
    // optional discount
    member this.TupleTotal2(qty,?discount) =
        let extPrice = this.Price * float qty
        match discount with
        | None -> extPrice
        | Some discount -> extPrice - discount

let productC = {SKU="ABC"; Price=2.0}
let total1C = productC.TupleTotal2(10)          // discount not specified
let total2C = productC.TupleTotal2(10, 1.0)     // discount specified


// There is a function defaultArg which takes the parameter as the first argument and a default for the second argument.
// If the parameter is set, the value is returned. And if not, the default value is returned.

type Product3 = {SKU:string; Price: float} with
    // optional discount
    member this.TupleTotal2(qty,?discount) =
        let extPrice = this.Price * float qty
        let discount = defaultArg discount 0.0
        //return
        extPrice - discount


// Method overloading
// F# does support method overloading, but only for methods (that is functions attached to types) and of these, only
// those using tuple-style parameter passing.

type Product4 = {SKU:string; Price: float} with
    // no discount
    member this.TupleTotal3(qty) =
        printfn "using non-discount method"
        this.Price * float qty
    // with discount
    member this.TupleTotal3(qty, discount) =
        printfn "using discount method"
        (this.Price * float qty) - discount

// Normally, the F# compiler would complain that there are two methods with the same name, but in this case, because
// they are tuple based and because their signatures are different, it is acceptable. (To make it obvious which one is
// being called, I have added a small debugging message.)

let productD = {SKU="ABC"; Price=2.0}
// discount not specified
let total1D = productD.TupleTotal3(10)
// discount specified
let total2D = productD.TupleTotal3(10, 1.0)
