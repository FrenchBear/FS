// 003 Attaching members
// Learning F#, Attaching members
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



// Extending system types
type System.Int32 with
    member this.IsEven = this % 2 = 0

//test
let i = 20
if i.IsEven then printfn "'%i' is even" i


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

// can create static members for system types as well:
type System.Int32 with
    static member IsOdd x = x % 2 = 1

type System.Double with
    static member Pi = 3.141

//test
let result = System.Int32.IsOdd 20
let pi = System.Double.Pi
