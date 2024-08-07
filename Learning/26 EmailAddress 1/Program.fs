// 26 EmailAddress 1
// Learning F# II, Variations over email address, part 1
//
// 2024-08-07   PV

// Single case unions

type EmailAddress = EmailAddress of string
type StateCode = StateCode of string

// This might seem confusing initially, but really they are in different scopes, so there is no naming collision. One is
// a type, and one is a constructor function with the same name.

// So if you see a function signature like this:
// val f: string -> EmailAddress
// this refers to things in the world of types, so EmailAddress refers to the type.

// On the other hand, if you see some code like this:
// let x = EmailAddress y
// this refers to things in the world of values, so EmailAddress refers to the constructor function.


// Constructing single case unions

// Here’s how we might extend the above module with some constructor functions:

// \s is space, a shorthand for [ \t\r\n\f]. \S is the negation of \s, [^ \t\r\n\f], that is, any non-space
let CreateEmailAddress (s:string) =
    if System.Text.RegularExpressions.Regex.IsMatch(s,@"^\S+@\S+\.\S+$")
        then Some (EmailAddress s)
        else None

let CreateStateCode (s:string) =
    let s' = s.ToUpper()
    let stateCodes = ["AZ";"CA";"NY"] // etc
    if stateCodes |> List.exists ((=) s')
        then Some (StateCode s')
        else None

// We can test the constructors now:
CreateStateCode "CA" |> printfn "CA -> %A"
CreateStateCode "XX" |> printfn "XX -> %A"

CreateEmailAddress "a@example.com" |> printfn "a@example.com -> %A"
CreateEmailAddress "example.com" |> printfn "example.com -> %A\n"


// Providing more info in case of invalid value (besides exceptions)

// The following example uses a CreationResult type to indicate the error in the failure case.
type CreationResult<'T> = Success of 'T | Error of string

let CreateEmailAddress2 (s:string) =
    if System.Text.RegularExpressions.Regex.IsMatch(s,@"^\S+@\S+\.\S+$")
        then Success (EmailAddress s)
        else Error "Email address must contain an @ sign"

// test
CreateEmailAddress2 "example.com" |> printfn "example.com -> %A\n"


// Using continuation fonctions
let CreateEmailAddressWithContinuations success failure (s:string) =
    if System.Text.RegularExpressions.Regex.IsMatch(s,@"^\S+@\S+\.\S+$")
        then success (EmailAddress s)
        else failure "Email address must contain an @ sign"

// The success function takes the email as a parameter and the error function takes a string. 
// Both functions must return the same type, but the type is up to you.

// Here is a simple example – both functions do a printf, and return nothing
printfn "\nUse continuations with printf"
let success0 (EmailAddress s) = printfn "success creating email %s" s
let failure0 msg = printfn "error creating email: %s" msg
CreateEmailAddressWithContinuations success0 failure0 "example.com"
CreateEmailAddressWithContinuations success0 failure0 "x@example.com"
printfn ""


// With continuations, you can easily reproduce any of the other approaches. Here’s the way to create options, for
// example. In this case both functions return an EmailAddress option.
printfn "\nUse continuations to return EmailAddress option"
let success1 e = Some e
let failure1 _  = None
CreateEmailAddressWithContinuations success1 failure1 "example.com" |> printfn "example.com -> %A"
CreateEmailAddressWithContinuations success1 failure1 "x@example.com" |> printfn "x@example.com -> %A"

// And here is the way to throw exceptions in the error case:
printfn "\nUse continuations to throw exceptions"
let success2 e = e
let failure2 _  = failwith "bad email address"
try
    CreateEmailAddressWithContinuations success2 failure2 "example.com" |> printfn "example.com -> %A"
with
| ex -> printfn "Error: %A" ex.Message
CreateEmailAddressWithContinuations success2 failure2 "x@example.com" |> printfn "x@example.com -> %A"

// And here is the way to throw exceptions in the error case:
printfn "\nUse continuations to return a CreationResult"
let success3 e = Success e
let failure3 m = Error m
CreateEmailAddressWithContinuations success3 failure3 "example.com" |> printfn "example.com -> %A"
CreateEmailAddressWithContinuations success3 failure3 "x@example.com" |> printfn "x@example.com -> %A"

// create a local partially applied function that you use instead of the long-winded one.
// setup a partially applied function
let success e = Some e
let failure _  = None
let createEmail = CreateEmailAddressWithContinuations success failure

// use the partially applied function
printfn "\nUsing a partially applied function"
createEmail "x@example.com" |> printfn "x@example.com -> %A"
createEmail "example.com" |> printfn "example.com -> %A"



// Simple wrapper types are starting to get more complicated now that we are adding validations, so it is probably a
// good idea to create a module for each wrapper type, and put the type and its associated functions there.

module EmailAddress =

    // Actually, _T is not very useful, in debug mode, we see a result of Some(EmailAddress "x@example.com")
    // Only the type of value is EmailAddress._T -> string, and create is string->EmailAddress._T option
    // The _ in front of T is a convention to indicate a private type, not to use directly
    type _T = EmailAddress of string

    // wrap
    let create (s:string) =
        if System.Text.RegularExpressions.Regex.IsMatch(s,@"^\S+@\S+\.\S+$")
            then Some (EmailAddress s)
            else None

    // unwrap
    let value (EmailAddress e) = e


// The users of the type would then use the module functions to create and unwrap the type. For example:
// create email addresses
let address1 = EmailAddress.create "x@example.com"
let address2 = EmailAddress.create "example.com"

// unwrap an email address (after unwrapping option)
match address1 with
| Some e -> EmailAddress.value e |> printfn "the value is %s"
| None -> ()

// Well, even without using _T, we can bypass the published constructor...
let address3 = EmailAddress.EmailAddress "Hello, world"




// Unwrapping with a continuation function

module EmailAddress2 =

    type _T = EmailAddress of string

    // create with continuation
    let createWithCont success failure (s:string) =
        if System.Text.RegularExpressions.Regex.IsMatch(s,@"^\S+@\S+\.\S+$")
            then success (EmailAddress s)
            else failure "Email address must contain an @ sign"

    // create directly
    let create s =
        let success e = Some e
        let failure _ = None
        createWithCont success failure s

    // Next two function are not strictly necessary, but added for the convenience of callers
    // unwrap with continuation
    let apply f (EmailAddress e) = f e

    // unwrap directly
    //let value e = apply id e
    let value (EmailAddress e) = e

