// 10 Types
// Learning F#, Play with types
//
// 2024-06-29   PV

// Helpers that allow partial application
let assertTrue b = assert b
let assertFalse b = assert not b
let assertEquals x y = assert (x=y)

// Type abbreviation (Alias)
type ProductCode = string
type ComplexNumber = float * float
type AdditionFunction = int->int->int
type ComplexAdditionFunction = ComplexNumber-> ComplexNumber -> ComplexNumber

// beware, no true encapsulation
type CustomerId = int
type OrderId = int
let a:CustomerId = 123
let b:OrderId = a       // No warning, no error


// ------------------------------------------------------
// Tuples

let t1 = (2,3)          // int*int
let t2 = -5,0           // int*int      The comma is important, not parentheses
let t3 = ("hello",42)   // string*int
let t4 = (42,true,"ah") // int*bool*string

// beware!
// a function that takes a single tuple parameter but looks like it takes two ints:
let addConfusingTuple (x,y) = x + y


// generic tuples
type gt<'a, 'b> = 'a*'b

// tuples of complex types
// define some types
type Person = {First:string; Last:string}
type Complex = float * float
type ComplexComparisonFunction = Complex -> Complex -> int

// define some tuples using them
type PersonAndBirthday = Person * System.DateTime
type ComplexPair = Complex * Complex
type ComplexListAndSortFunction = Complex list * ComplexComparisonFunction
type PairOfIntFunctions = (int->int) * (int->int)

// making and matching tuples
let x = (1,2)
let y = 1,2                     // it's the comma you need, not the parentheses!
let z = 1,true,"hello",3.14     // create arbitrary tuples as needed

// And as we have seen, to “deconstruct” a tuple, use the same syntax:
let z' = 1,true,"hello",3.14    // "construct"
let z1,z2,z3,z4 = z'            // "deconstruct"

// When pattern matching like this, you must have the same number of elements, otherwise you will get an error:
// let z1,z2 = z                // error FS0001: Type mismatch. The tuples have differing lengths

// If you don’t need some of the values, you can use the “don’t care” symbol (the underscore) as a placeholder.
let _,z5,_,z6 = z  // ignore 1st and 3rd elements

// A two element tuple is commonly called a “pair” and a three element tuple is called a “triple” and so on. In the
// special case of pairs, there are functions fst and snd which extract the first and second element.
let x2 = 1,2
fst x2 |> printfn "First=%d"
snd x2 |> printfn "Second=%d"

// They only work on pairs. Trying to use fst on a triple will give an error.
let x3 = 1,2,3
//fst x3

// Using tuples for returning multiple values
let tryParse intStr =
   try
      let i = System.Int32.Parse intStr
      (true,i)
   with _ -> (false,0)  // any exception

// Creating tuples from other tuples
// Tuples are immutables, you must always create a new one
let addOneToTuple aTuple =
   let (x,y,z) = aTuple
   (x+1,y+1,z+1)   // create a new one

addOneToTuple (1,2,3) |> printfn "Tuple+1=%A"

// equality
(1,2) = (1,2)  |> assertTrue                        // true
(1,2,3,"hello") = (1,2,3,"bye")  |> assertFalse     // false
(1,(2,3),4) = (1,(2,3),4)  |> assertTrue            // true

// Trying to compare tuples of different lengths is a type error:
// (1,2) = (1,2,3)                    // error FS0001: Type mismatch
// And the types in each slot must be the same as well:
// (1,2,3) = (1,2,"hello")   // element 3 was expected to have type int but here has type string
// (1,(2,3),4) = (1,2,(3,4)) // elements 2 & 3 have different types

// Tuples also have an automatically defined hash value based on the values in the tuple, so that tuples can be used as
// dictionary keys without problems.
(1,2,3).GetHashCode() |> printfn "Hashcode=%A"
printfn ""


// ------------------------------------------------------
// Records

// A record type is a tuple where each element is labeled.
type ComplexNumberR = { Real: float; Imaginary: float }
type GeoCoord = { Lat: float; Long: float }

// A record type has the standard preamble: type [typename] = followed by curly braces. Inside the curly braces is a
// list of label: type pairs, separated by semicolons (remember, all lists in F# use semicolon separators – commas are
// for tuples).
// Let’s compare the “type syntax” for a record type with a tuple type:
type ComplexNumberRecord = { Real: float; Imaginary: float }
type ComplexNumberTuple = float * float
// In the record type, there is no “multiplication”, just a list of labeled types.


// Making and matching records
// To create a record value, use a similar format to the type definition, but using equals signs after the labels. This
// is called a “record expression.”

type ComplexNumberRecord2 = { Real: float; Imaginary: float }
let myComplexNumber = { Real = 1.1; Imaginary = 2.2 } // use equals!

type GeoCoord2 = { Lat: float; Long: float }    // use colon in type
let myGeoCoord = { Lat = 1.1; Long = 2.2 }      // use equals in let

// And to “deconstruct” a record, use the same syntax:
let myGeoCoord1 = { Lat = 1.1; Long = 2.2 }     // "construct"
let { Lat=myLat; Long=myLong } = myGeoCoord1    // "deconstruct"

// As always, if you don’t need some of the values, you can use the underscore as a placeholder; or more cleanly, just
// leave off the unwanted label altogether.
let { Lat=_; Long=myLong2 } = myGeoCoord        // "deconstruct"
let { Long=myLong3 } = myGeoCoord               // "deconstruct", ok with partial list of labels

// If you just need a single property, you can use dot notation rather than pattern matching.
let x4 = myGeoCoord.Lat
let y4 = myGeoCoord.Long

// Note that you can leave a label off when deconstructing, but not when constructing:
// let myGeoCoord = { Lat = 1.1; }  // error FS0764: No assignment given for field 'Long'

// One of the most noticeable features of record types is use of curly braces. Unlike C-style languages, curly braces
// are rarely used in F# – only for records, sequences, computation expressions (of which sequences are a special case),
// and object expressions (creating implementations of interfaces on the fly). These other uses will be discussed later.


// Label order
// Unlike tuples, the order of the labels is not important. So the following two values are the same:
let myGeoCoordA = { Lat = 1.1; Long = 2.2 }
let myGeoCoordB = { Long = 2.2; Lat = 1.1 }     // same as above

// Naming conflicts
// In the examples above, we could construct a record by just using the label names “lat” and “long”. Magically, the
// compiler knew what record type to create. (Well, in truth, it was not really that magical, as only one record type
// had those exact labels.)
// But what happens if there are two record types with the same labels? How can the compiler know which one you mean?
// The answer is that it can’t – it will use the most recently defined type, and in some cases, issue a warning. Try
// evaluating the following:

type Person1 = {First:string; Last:string}
type Person2 = {First:string; Last:string}
let p = {First="Alice"; Last="Jones"} //
// What type is p? Answer: Person2, which was the last type defined with those labels.
// And if you try to deconstruct, you will get a warning about ambiguous field labels.
let {First=f; Last=l} = p
// warning FS0667: The labels of this record do not uniquely determine a corresponding record type

// How can you fix this? Simply by adding the type name as a qualifier to at least one of the labels.
let p2 = {Person1.First="Alice"; Last="Jones"}
//  ^Person1

// If needed, you can even add a fully qualified name (with namespace). Here’s an example using modules.
module Module1 =
  type Person = {First:string; Last:string}

module Module2 =
  type Person = {First:string; Last:string}

let p3 = {Module1.Person.First="Alice"; Module1.Person.Last="Jones"}

// Alternatively, you can add an explicit type annotation so that the compiler knows what type the record is:
let p4 : Module1.Person = {First="Alice"; Last="Jones"}

// Of course, if you can ensure there is only one version in the local namespace, you can avoid having to do this at all.
module Module3 =
  open Module1  // bring only one definition into scope
  let p5 = {First="Alice"; Last="Jones"} // will be Module1.Person

// The moral of the story is that when defining record types, you should try to use unique labels if possible, otherwise
// you will get ugly code at best, and unexpected behavior at worst.
// Note that in F#, unlike some other functional languages, two types with exactly the same structural definition are
// not the same type. This is called a “nominal” type system, where two types are only equal if they have the same name,
// as opposed to a “structural” type system, where definitions with identical structures will be the same type
// regardless of what they are called.


// Using records in practice

// Using records for function results
// Just like tuples, records are useful for passing back multiple values from a function. Let’s revisit the tuple
// examples described earlier, rewritten to use records instead:

// the tuple version of TryParse
let tryParseTuple intStr =
  try
    let i = System.Int32.Parse intStr
    (true,i)
  with _ -> (false,0)  // any exception

// for the record version, create a type to hold the return result
type TryParseResult = {Success:bool; Value:int}

// the record version of TryParse
let tryParseRecord intStr =
  try
    let i = System.Int32.Parse intStr
    {Success=true;Value=i}
  with _ -> {Success=false;Value=0}

tryParseTuple "99"      |> printfn "ParseTuple returned %A"
tryParseRecord "99"     |> printfn "ParseTuple returned %A"
tryParseTuple "abc"     |> printfn "ParseTuple returned %A"
tryParseRecord "abc"    |> printfn "ParseTuple returned %A"

// You can see that having explicit labels in the return value makes it much easier to understand (of course, in
// practice we would probably use an Option type, discussed in later post).


// And here’s the word and letter count example using records rather than tuples:

//define return type
type WordAndLetterCountResult = {WordCount:int; LetterCount:int}

let wordAndLetterCount (s:string) =
  let words = s.Split [|' '|]
  let letterCount = words |> Array.sumBy (fun word -> word.Length )
  {WordCount=words.Length; LetterCount=letterCount}

wordAndLetterCount "to be or not to be"    |> printfn "wordAndLetterCount returned %A"


// Creating records from other records
// Again, as with most F# values, records are immutable and the elements within them cannot be assigned to. So how do
// you change a record? Again the answer is that you can’t – you must always create a new one.

// Say that you need to write a function that, given a GeoCoord record, adds one to each element. Here it is:
let addOneToGeoCoord aGeoCoord =
  let {Lat=x; Long=y} = aGeoCoord
  {Lat = x + 1.0; Long = y + 1.0}   // create a new one

addOneToGeoCoord {Lat=1.1; Long=2.2}    |> printfn "addOneToGeoCoord returned %A"

// But again you can simplify by deconstructing directly in the parameters of a function, so that the function becomes a one liner:
let addOneToGeoCoord2 {Lat=x; Long=y} = {Lat=x+1.0; Long=y+1.0}

addOneToGeoCoord2 {Lat=1.0; Long=2.0} |> printfn "addOneToGeoCoord2 returned %A"

// or depending on your taste, you can also use dot notation to get the properties:
let addOneToGeoCoord3 aGeoCoord = {Lat=aGeoCoord.Lat + 1.0; Long= aGeoCoord.Long + 1.0}

// In many cases, you just need to tweak one or two fields and leave all the others alone. To make life easier, there is
// a special syntax for this common case, the “with” keyword. You start with the original value, followed by “with” and
// then the fields you want to change. Here are some examples:
let g1 = {Lat=1.1; Long=2.2}
let g2 = {g1 with Lat=99.9}   // create a new one

let p1 = {First="Alice"; Last="Jones"}
let p1m = {p1 with Last="Smith"}
// The technical term for “with” is a copy-and-update record expression.


// Record equality

// Like tuples, records have an automatically defined equality operation: two records are equal if they have the same
// type and the values in each slot are equal.
let p1b = {First="Alice"; Last="Jones"}
let p2b = {First="Alice"; Last="Jones"}
printfn "p1b=p2b is %b" (p1b=p2b)  // p1b=p2b is true

// And records also have an automatically defined hash value based on the values in the record, so that records can be
// used in a hashed collection without any problems.
let h1 = {First="Alice"; Last="Jones"}.GetHashCode()
let h2 = {First="Alice"; Last="Jones"}.GetHashCode()
printfn "h1=h2 is %b" (h1=h2)  // h1=h2 is true


// Record representation

// As noted in a previous post, records have a nice default string representation, and can be serialized easily. The
// default ToString() implementation uses this same representation.
let p6 = {First="Alice"; Last="Jones"}
printfn "%A" p6
// output:
//   { First = "Alice"
//     Last = "Jones" }

printfn "%O" p6   // same as above

// Sidebar: %A vs. %O in print format strings
// We just saw that print format specifiers %A and %O produce the same results. So why the difference?
// %A prints the value using the same pretty printer that is used for interactive output. But %O uses ToString(), which
// means that if the ToString method is not overridden, %O will give the default (sometimes unhelpful) output. So in
// general, you should try to use %A instead of %O for user-defined types unless you want to override ToString().
type Person3 = {First:string; Last:string}
  with
  override this.ToString() = sprintf "%s %s" this.First this.Last

printfn "%A" {First="Alice"; Last="Jones"}
// output:
//   { First = "Alice"
//     Last = "Jones" }
printfn "%O" {First="Alice"; Last="Jones"}
// output:
//   "Alice Jones"
// But note that the F# “class” types do not have a standard pretty printed format, so %A and %O are equally unhelpful
// unless you override ToString().

printfn ""


// ------------------------------------------------------
// X

