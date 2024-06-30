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

module Tuples =
    printfn "-------------------------------\nTuples\n"

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

module Records =
    printfn "-------------------------------\nRecords\n"

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
// Discriminated Unions

module DiscriminatedUnion =
    printfn "-------------------------------\nDiscriminated Unions\n"
    
    type IntOrBool1 =
      | I of int
      | B of bool
    // The “I” and the “B” are just arbitrary labels; we could have used any other labels that were meaningful.

    // For small types, we can put the definition on one line:
    type IntOrBool2 = I of int | B of bool

    //The component types can be any other type you like, including tuples, records, other union types, and so on.
    type Person1 = {first:string; last:string}  // define a record type
    type IntOrBool3 = I of int | B of bool
    type MixedType =
      | Tup of int * int  // a tuple
      | P of Person1      // use the record type defined above
      | L of int list     // a list of ints
      | U of IntOrBool1   // use the union type defined above

    // Key points about union types
    
    // - The vertical bar is optional before the first component, so that the following definitions are all equivalent,
    //   as you can see by examining the output of the interactive window:
    type IntOrBool4 = I of int | B of bool     // without initial bar
    type IntOrBool5 = | I of int | B of bool   // with initial bar
    type IntOrBool6 =
       | I of int
       | B of bool      // with initial bar on separate lines

    // -The tags or labels must start with an uppercase letter. So the following will give an error:
    // type IntOrBool = int of int| bool of bool
    // error FS0053: Discriminated union cases must be uppercase identifiers

    // Other named types (such as Person or IntOrBool) must be pre-defined outside the union type. You can’t define them
    // “inline” and write something like this:
    // type MixedType =
    //   | P of  {first:string; last:string}  // error
    // or
    // type MixedType =
    //   | U of (I of int | B of bool)  // error

    // The labels can be any identifier, including the names of the component type themselves, which can be quite
    // confusing if you are not expecting it. For example, if the Int32 and Boolean types (from the System namespace)
    // were used instead, and the labels were named the same, we would have this perfectly valid definition:
    open System
    type IntOrBool7 = Int32 of Int32 | Boolean of Boolean
    //This “duplicate naming” style is actually quite common, because it documents exactly what the component types are.


    // Constructing a value of a union type
    
    // To create a value of a union type, you use a “constructor” that refers to only one of the possible union cases.
    // The constructor then follows the form of the definition, using the case label as if it were a function. In the
    // IntOrBool example, you would write:
    type IntOrBool8 = I of int | B of bool
    let i  = I 99    // use the "I" constructor
    // val i : IntOrBool = I 99
    let b  = B true  // use the "B" constructor
    // val b : IntOrBool = B true
    
    // The resulting value is printed out with the label along with the component type:
    // val [value name] : [type]    = [label] [print of component type]
    // val i            : IntOrBool = I       99
    // val b            : IntOrBool = B       true
    
    // If the case constructor has more than one “parameter”, you construct it in the same way that you would call a function:
    type Person2 = {first:string; last:string}
    type MixedType2 =
      | Tup of int * int
      | P of Person2
    let myTup  = Tup (2,99)    // use the "Tup" constructor
    // val myTup : MixedType = Tup (2,99)
    let myP  = P {first="Al"; last="Jones"} // use the "P" constructor
    // val myP : MixedType = P {first = "Al";last = "Jones";}

    // The case constructors for union types are normal functions, so you can use them anywhere a function is expected.
    // For example, in List.map:
    type C = Circle of int | Rectangle of int * int
    [1..10] |> List.map Circle |> ignore
    [1..10] |> List.zip [21..30] |> List.map Rectangle |> ignore


    // Naming conflicts

    // If a particular case has a unique name, then the type to construct will be unambiguous. But what happens if you
    // have two types which have cases with the same labels?
    type IntOrBool1b = I of int | B of bool
    type IntOrBool2b = I of int | B of bool
    
    // In this case, the last one defined is generally used:
    let x = I 99                // val x : IntOrBool2b = I 99
    
    // But it is much better to explicitly qualify the type, as shown:
    let x1 = IntOrBool1.I 99    // val x1 : IntOrBool1 = I 99
    let x2 = IntOrBool2.B true  // val x2 : IntOrBool2 = B true
    
    // And if the types come from different modules, you can use the module name as well:
    module Module1 =
      type IntOrBool = I of int | B of bool
    module Module2 =
      type IntOrBool = I of int | B of bool
    module Module3 =
      let x = Module1.IntOrBool.I 99 // val x : Module1.IntOrBool = I 99

    
    // Matching on union types
    
    // For tuples and records, we have seen that “deconstructing” a value uses the same model as constructing it. 
    // This is also true for union types, but we have a complication: which case should we deconstruct? This is exactly
    // what the “match” expression is designed for. As you should now realize, the match expression syntax has parallels
    // to how a union type is defined.
    type Person3 = {First:string; Last:string}
    type MixedType3 =
      | Tup3 of int * int
      | P3 of Person3

    // "deconstruction" of union type
    let matcher x =
      match x with
      | Tup3 (x,y) -> printfn "Tuple matched with %i %i" x y
      | P3 {First=f; Last=l} -> printfn "Person2 matched with %s %s" f l

    let myTup3 = Tup3 (2,99)                 // use the "Tup" constructor
    matcher myTup3
    let myP3 = P3 {First="Al"; Last="Jones"} // use the "P" constructor
    matcher myP3

    // Let’s analyze what is going on here:
    // - Each “branch” of the overall match expression is a pattern expression that is designed to match the
    //   corresponding case of the union type.
    // - The pattern starts with the tag for the particular case, and then the rest of the pattern deconstructs the type
    //   for that case in the usual way.
    // - The pattern is followed by an arrow “->” and then the code to execute.


    // Empty cases
    // The label for a union case does not have to have to have any type after it. The following are all valid union types:
    type Directory =
      | Root                   // no need to name the root
      | Subdirectory of string // other directories need to be named
    type Result =
      | Success                // no string needed for success state
      | ErrorMessage of string // error message needed

    // If all the cases are empty, then we have an “enum style” union:
    type Size = Small | Medium | Large
    type Answer = Yes | No | Maybe

    // Note that this “enum style” union is not the same as a true C# enum type, discussed later.
    // To create an empty case, just use the label as a constructor without any parameters:
    let myDir1 = Root
    let myDir2 = Subdirectory "bin"
    let myResult1 = Success
    let myResult2 = ErrorMessage "not found"
    let mySize1 = Small
    let mySize2 = Medium
    
    
    // Single cases
    // Sometimes it is useful to create union types with only one case. This might be seem useless, because you don’t
    // seem to be adding value. But in fact, this a very useful practice that can enforce type safety.
    // For example, let’s say that we have customer ids and order ids which are both represented by integers, but that
    // they should never be assigned to each other. As we saw before, a type alias approach will not work, because an
    // alias is just a synonym and doesn’t create a distinct type. Here’s how you might try to do it with aliases:

    type CustomerId = int   // define a type alias
    type OrderId = int      // define another type alias
    let printOrderId (orderId:OrderId) = printfn "The orderId is %i" orderId
    let custId = 1          // create a customer id
    printOrderId custId     // Uh-oh! works!

    // But even though I explicitly annotated the orderId parameter to be of type OrderId, I can’t ensure that customer
    // ids are not accidentally passed in. On the other hand, if we create simple union types, we can easily enforce the
    // type distinctions.
    type CustomerId2 = CustomerId2 of int   // define a union type
    type OrderId2 = OrderId2 of int         // define another union type
    let printOrderId2 (OrderId2 orderId) =  // deconstruct in the param
       printfn "The orderId is %i" orderId

    let custId2 = CustomerId2 1             // create a customer id
    // printOrderId2 custId                   // Good! A compiler error now.

    // A convenient thing about single case union types is you can pattern match directly against a value without having
    // to use a full match-with expression.
    
    // deconstruct in the param
    let printCustomerId (CustomerId2 customerIdInt) =
       printfn "The CustomerId is %i" customerIdInt

    // or deconstruct explicitly through let statement
    let printCustomerId2 custId =
       let (CustomerId2 customerIdInt) = custId  // deconstruct here
       printfn "The CustomerId is %i" customerIdInt

    // try it
    let custId2b = CustomerId2 1             // create a customer id
    printCustomerId custId2b
    printCustomerId2 custId2b

    // But a common “gotcha” is that in some cases, the pattern match must have parens around it, otherwise the compiler
    // will think you are defining a function!
    let custId3 = CustomerId2 1
    let (CustomerId2 customerIdInt) = custId3  // Correct pattern matching
    let CustomerId2 customerIdInt = custId3    // Wrong! New function?

    // Similarly, if you ever do need to create an enum-style union type with a single case, you will have to start the
    // case with a vertical bar in the type definition; otherwise the compiler will think you are creating an alias.
    type TypeAlias = A     // type alias!
    type SingleCase = | A   // single case union type
    
    
    // Union equality
    
    // Like other core F# types, union types have an automatically defined equality operation: two unions are equal if
    // they have the same type and the same case and the values for that case is equal.
    type Contact = Email of string | Phone of int

    let email1 = Email "bob@example.com"
    let email2 = Email "bob@example.com"

    let areEqual = (email1=email2)
    
    
    // Union representation
    
    // Union types have a nice default string representation, and can be serialized easily. But unlike tuples, the
    // ToString() representation is unhelpful.
    type Contact2 = Email2 of string | Phone2 of int
    let email = Email2 "bob@example.com"
    printfn "%A" email    // nice
    printfn "%O" email    // ugly!

    printfn ""


// ------------------------------------------------------
// Options

module Option =
    printfn "-------------------------------\nOptions\n"
    
    // It is actually built into the language.
    // It is defined as union type with two cases: Some and None.
    // Here is a definition:    // COMMENTED OUT TO AVOID REDEFINITION!
    // type Option<'a> =        // use a generic definition
    //    | Some of 'a          // valid value
    //    | None                // missing

    // The option type is used in the same way as any union type in construction, by specifying one of the two cases,
    // the Some case or the None case:
    let validInt = Some 1
    let invalidInt = None
    
    // when pattern matching, as with any union type, you must always match all the cases:
    match validInt with
    | Some x -> printfn "the valid value is %A" x
    | None -> printfn "the value is None"

    // When defining a type that references the Option type, you must specify the generic type to use. You can do this
    // in an explicit way, with angle brackets, or use the built-in “option” keyword which comes after the type. The
    // following examples are identical:
    type SearchResult1 = Option<string>  // Explicit C#-style generics
    type SearchResult2 = string option   // built-in postfix keyword


    // Using the Option type
    
    // The option type is widely used in the F# libraries for values that might be missing or otherwise invalid.
    // For example, the List.tryFind function returns an option, with the None case used indicate that nothing matches
    // the search predicate.
    [1;2;3;4]  |> List.tryFind (fun x-> x = 3)  |> assertEquals (Some 3)
    [1;2;3;4]  |> List.tryFind (fun x-> x = 10) |> assertEquals None

    // Let’s revisit the same example we used for tuples and records, and see how options might be used instead.
    // NOTE: The tryParseOption code is just an example. A similar function tryParse is built into the .NET core
    // libraries and should be used instead.
    
    // the tuple version of TryParse
    let tryParseTuple intStr =
       try
          let i = System.Int32.Parse intStr
          (true,i)
       with _ -> (false,0)  // any exception

    // for the record version, create a type to hold the return result
    type TryParseResult = {success:bool; value:int}
    // the record version of TryParse
    let tryParseRecord intStr =
       try
          let i = System.Int32.Parse intStr
          {success=true;value=i}
       with _ -> {success=false;value=0}

    // the option version of TryParse
    let tryParseOption intStr =
       try
          let i = System.Int32.Parse intStr
          Some i
       with _ -> None

    tryParseTuple "99" |> assertEquals (true, 99)
    tryParseRecord "99" |> assertEquals {success=true;value=99}
    tryParseOption "99" |> assertEquals (Some 99)
    tryParseTuple "abc" |> assertEquals (false, 0)
    tryParseRecord "abc" |> assertEquals {success=false;value=0}
    tryParseOption "abc" |> assertEquals None

    // Of these three approaches, the “option” version is generally preferred; no new types need to be defined and for
    // simple cases, the meaning of None is obvious from the context.

    
    // Option equality

    // Like other union types, option types have an automatically defined equality operation
    let o1 = Some 42
    let o2 = Some 42
    let areEqual = (o1=o2)
    
    
    // Option representation
    
    // Option types have a nice default string representation, and unlike other union types, the ToString() representation is also nice.
    let o = Some 42
    printfn "%A" o   // nice: Some 42
    printfn "%O" o   // nice: Some(42)
    
    
    // Options are not just for primitive types
    
    // The F# option is a true first class type (it’s just a normal union type, after all). You can use it with any
    // type. For example, you can have an option of a complex type like Person, or a tuple type like int*int, or a
    // function type like int->bool, or even an option of an option type.
    type OptionalString = string option
    type OptionalPerson = TryParseResult option       // optional complex type
    type OptionalTuple = (int*int) option
    type OptionalFunc = (int -> bool) option  // optional function
    type NestedOptionalString = OptionalString option //nested options!
    type StrangeOption = string option option option


    // How the Option type should not be used
    
    // The option type has functions such as IsSome, IsNone and Value, which allow you to access the “wrapped” value
    // without doing pattern matching. Don’t use them! Not only it is not idiomatic, but it is dangerous and can cause
    // exceptions.
    // Here is how not to do it:
    let x = Some 99
    if x.IsSome then printfn "x is %i" x.Value   // ugly!!
    printfn "x is %i" x.Value   // no matching at all, ugly and dangerous!!
    
    // Here is how to do it properly:
    let x1 = Some 99
    match x1 with
    | Some i -> printfn "x is %i" i
    | None -> () // what to do here?

    // The pattern matching approach also forces you to think about and document what happens in the None case, which
    // you might easily overlook when using IsSome.
    
    
    // The Option module

    // If you are doing a lot of pattern matching on options, look into the Option module, as it has some useful helper
    // functions like map, bind, iter and so on.
    
    // For example, say that I want to multiply the value of an option by 2 if it is valid. Here’s the pattern matching way:
    let x2 = Some 99
    let result = match x2 with
                 | Some i -> Some(i * 2)
                 | None -> None

    // And here’s a more compact version written using Option.map:
    let x3 = Some 99
    x3 |> Option.map (fun v -> v * 2) |> assertEquals (Some 198)

    // Or perhaps I want to multiply the value of an option by 2 if it is valid but return 0 if it is None. Here’s the
    // pattern matching way:
    let x4 = Some 99
    let result4 = match x4 with
                  | Some i -> i * 2
                  | None -> 0

    // And here’s the same thing as a one-liner using Option.fold:
    let x5 = Some 99
    x5 |> Option.fold (fun _ v -> v * 2) 0 |> assertEquals 198      // Note that the result is 198, not Some 198

    // In simple cases like the one above, the defaultArg function can be used as well.
    let x6 = Some 99
    defaultArg x6 0 |> assertEquals 99


    // Option vs. Null vs. Nullable

    // The option type often causes confusion to people who are used to dealing with nulls and nullables in C# and other
    // languages. This section will try to clarify the differences.

    // Type safety of Option vs. null
    // In a language like C# or Java, “null” means a reference or pointer to an object that doesn’t exist. The “null”
    // has exactly the same type as the object, so you can’t tell from the type system that you have a null.
    
    // For example, in the C# code below we create two string variables, one with a valid string and one with a null string.
    // string s1 = "abc";
    // var len1 = s1.Length;
    // string s2 = null;
    // var len2 = s2.Length;
    
    // This compiles perfectly, of course. The compiler cannot tell the difference between the two variables. The null
    // is exactly the same type as the valid string, so all the System.String methods and properties can be used on it,
    // including the Length property.
    // Now, we know that this code will fail by just looking at it, but the compiler can’t help us. Instead, as we all
    // know, you have to tediously test for nulls constantly.

    // Now let’s look at the nearest F# equivalent of the C# example above. In F#, to indicate missing data, you would
    // use an option type and set it to None. (In this artificial example we have to use an ugly explicitly typed None –
    // normally this would not be necessary.)
    let s1 = "abc"
    let len1 = s1.Length

    // create a string option with value None
    let s2 = Option<string>.None
    // let len2 = s2.Length         // Raises a compile-time error!

    // In the F# version, we get a compile-time error immediately. The None is not a string, it’s a different type
    // altogether, so you can’t call Length on it directly. And to be clear, Some [string] is also not the same type as
    // string, so you can’t call Length on it either!

    // So if Option<string> is not a string, but you want to do something with the string it (might) contain, you are
    // forced to have to pattern match on it (assuming you don’t do bad things as described earlier).
    let s3 = Option<string>.None

    // which one is it?
    let len3 = match s3 with
               | Some s -> s.Length
               | None -> 0

    // You always have to pattern match, because given a value of type Option<string>, you can’t tell whether it is Some
    // or None. In just the same way Option<int> is not the same type as int, Option<bool> is not the same type as bool,
    // and so on.

    // To summarize the critical points:
    // - The type “string option” is not at all the same type as “string”. You cannot cast from string option to string,
    //   they do not have the same properties. A function that works with string will not work with string option, and
    //   vice versa. So the type system will prevent any errors.
    // - On the other hand, a “null string” in C# is exactly the same type as “string”. You cannot tell them apart at
    //   compile time, only at run time. A “null string” appears to have all the same properties and functions as a
    //   valid string, except that your code will blow up when you try to use it!
    
    
    // Nulls vs. missing data

    // A “null” as used in C# is completely different from the concept of “missing” data, which is a valid part of
    // modeling any system in any language. In a true functional language there can be a concept of missing data, but
    // there can be no such thing as “null”, because the concepts of “pointers” or “uninitialized variables” do not
    // exist in the functional way of thinking.

    // For example, consider a value bound to the result of an expression like this:
    let x7 = "hello world"

    // How can that value ever be uninitialized, or become null, or even become any other value at all?
    // Unfortunately, additional confusion has been caused because in some cases API designers have used null to
    // indicate the concept of “missing” data as well! For example, the .NET library method StreamReader.ReadLine
    // returns null to indicate that there is no more data in a file.
    
    
    // F# and null

    // F# is not a pure functional language, and has to interact with the .NET languages that do have the concept of
    // null. Therefore, F# does include a null keyword in its design, but makes it hard to use and treats it as an
    // abnormal value. As a general rule, nulls are never created in “pure” F#, but only by interacting with the .NET
    // libraries or other external systems.
    
    // Here are some examples:
    // pure F# type is not allowed to be null (in general)
    type Person = {first:string; last:string}
    // let p : Person = null                   // error!

    // type defined in CLR, so is allowed to be null
    let s : string = null                      // no error!
    // let line = streamReader.ReadLine()         // no error if null       // COMMENTED OUT, NOT A STATIC METHOD
    
    // In these cases, it is good practice to immediately check for nulls and convert them into an option type!
    // let line = match streamReader.ReadLine()  with
    //            | null -> None
    //            | line -> Some line

    // environment example
    let GetEnvVar var =
        match System.Environment.GetEnvironmentVariable(var) with
        | null -> None
        | value -> Some value

    // try it
    GetEnvVar "PATH" |> ignore
    GetEnvVar "TEST" |> ignore
    
    // And on occasion, you may need to pass a null to an external library. You can do this using the null keyword as
    // well.


    // Option vs. Nullable

    // In addition to null, C# has the concept of a Nullable type, such as Nullable<int>, which seems similar to the
    // option type. So what’s the difference? The basic idea is the same, but Nullable is much weaker. It only works on
    // value types such as Int and DateTime, not on reference types such as strings or classes or functions. You can’t
    // nest Nullables, and they don’t have much special behavior.
    // On the other hand, the F# option is a true first class type and can be used consistently across all types in the
    // same way. (See the examples above in the “Options are not just for primitive types” section.)
