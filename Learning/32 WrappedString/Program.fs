// 32 WrappedString
// Learning F# II, WrappedString example
//
// 2024-08-22   PV

module String100 =
    type T = String100 of string
    let create (s:string) =
        if s <> null && s.Length <= 100
        then Some (String100 s)
        else None
    let apply f (String100 s) = f s
    let value s = apply id s
    let value2 (String100 s) = s

module String50 =
    type T = String50 of string
    let create (s:string) =
        if s <> null && s.Length <= 50
        then Some (String50 s)
        else None
    let apply f (String50 s) = f s
    let value s = apply id s

module String2 =
    type T = String2 of string
    let create (s:string) =
        if s <> null && s.Length <= 2
        then Some (String2 s)
        else None
    let apply f (String2 s) = f s
    let value s = apply id s


let name = String100.create "Pierre"

match name with
| None -> printfn "Y'a un problème"
| Some n -> 
    printfn "name: %A" n
    String100.apply (fun x -> printfn "%s" x) n
    let v = String100.value n
    printfn "%s" v
    let l = String100.apply String.length n
    let l2 = String.length v
    printfn "%d" l
    let p = String100.value2 n
    printfn "%s" p


let s50 = String50.create "John"
let s100 = String100.create "Smith"
let s50' = s50.Value
let s100' = s100.Value
//let areEqual = (s50' = s100')  

let singleLineTrimmed s =
    System.Text.RegularExpressions.Regex.Replace(s,"\s+"," ").Trim()

let zz = singleLineTrimmed "  Pierre   Violent  "

printfn "<%s>" zz



module WrappedString =
    // An interface that all wrapped strings support
    type IWrappedString =
        abstract Value : string

    // Create a wrapped value option
    // 1) canonicalize the input first
    // 2) If the validation succeeds, return Some of the given constructor
    // 3) If the validation fails, return None
    // Null values are never valid.
    let create canonicalize isValid ctor (s:string) =
        if s = null
        then None
        else
            let s' = canonicalize s
            if isValid s'
            then Some (ctor s')
            else None

    // Apply the given function to the wrapped value
    let apply f (s:IWrappedString) =
        s.Value |> f

    // Get the wrapped value
    let value s = apply id s

    // Equality test
    let equals left right = (value left) = (value right)

    // Comparison
    let compareTo left right = (value left).CompareTo (value right)


    // Canonicalizes a string before construction
    // * converts all whitespace to a space char
    // * trims both ends
    let singleLineTrimmed s =
        System.Text.RegularExpressions.Regex.Replace(s,"\s+"," ").Trim()

    // A validation function based on length
    let lengthValidator len (s:string) =
        s.Length <= len

    // A string of length 100
    type String100 = String100 of string with
        interface IWrappedString with
            member this.Value = 
                let (String100 s) = this
                printfn "In IWrappedString.Value of String100"
                s

    // A constructor for strings of length 100
    let string100 = create singleLineTrimmed (lengthValidator 100) String100        // String100 is DU single case type constructor, not "type String100"!

    // Converts a wrapped string to a string of length 100
    let convertTo100 s = apply string100 s

    // A string of length 50
    type String50 = String50 of string with
        interface IWrappedString with
            member this.Value = let (String50 s) = this in s

    // A constructor for strings of length 50
    let string50 = create singleLineTrimmed (lengthValidator 50)  String50

    // Converts a wrapped string to a string of length 50
    let convertTo50 s = apply string50 s



    // Map helpers
    let mapAdd k v map =
        Map.add (value k) v map
    let mapContainsKey k map =
        Map.containsKey (value k) map
    let mapTryFind k map =
        Map.tryFind (value k) map



let ws100a = WrappedString.string100 "abc" |> Option.get
let ws100b = (WrappedString.string100 "abc").Value
printfn "ws100a is %A" ws100a
printfn "ws100b is %A" ws100b

let Exv (s:WrappedString.IWrappedString) = s.Value

printfn "Exv ws100a = %A" (Exv ws100a)
printfn "Exv ws100a = %A" (Exv ws100a)


let xs50 = WrappedString.string50 "abc" |> Option.get
printfn "xs50 is %A" xs50
let bad = WrappedString.string50 null
printfn "bad is %A" bad
let xs100 = WrappedString.string100 "abc" |> Option.get
printfn "xs100 is %A" xs100

// equality using module function is true
printfn "s50 is equal to s100 using module equals? %b" (WrappedString.equals xs50 xs100)
// equality using Object method is false
printfn "s50 is equal to s100 using Object.Equals? %b" (xs50.Equals xs100)
// direct equality does not compile
// printfn "s50 is equal to s100? %b" (s50 = s100) // compiler error


let abc = WrappedString.string50 "abc" |> Option.get
let def = WrappedString.string100 "def" |> Option.get
let map =
    Map.empty
    |> WrappedString.mapAdd abc "value for abc"
    |> WrappedString.mapAdd def "value for def"
printfn "Found abc in map? %A" (WrappedString.mapTryFind abc map)
let xyz = WrappedString.string100 "xyz" |> Option.get
printfn "Found xyz in map? %A" (WrappedString.mapTryFind xyz map)

