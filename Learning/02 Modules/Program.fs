// 02 Modules
// Learning F#, Using modules
// Main code
//
// 2024-06-17   PV

// open FS002.MathStuff.FloatLib        // Forbidden because of [<RequireQualifiedAccess>] attribute
open FS02.MathStuff.TrigDR

let p = FS02.Powers.p3 4.0
printfn "p = %A" p

let angleMode = FS02.MathStuff.Deg

let add1Float x = FS02.MathStuff.FloatLib.add x 1.0
let q = add1Float p
printfn "q = %A" q

degrees()
let r = sinDR 45.0
printfn "r = %A" r

radians()
let s = FS02.MathStuff.PI/6.0 |> sinDR
printfn "s = %A" s

// test module Person, type defined outside the module
let person = FS02.Person.create "john" "doe"
FS02.Person.fullName person |> printfn "Fullname=%s"

// test module Customer, type defined in the module
let customer = FS02.Customer.create 42 "bob"
FS02.Customer.isValid customer |> printfn "Is valid?=%b"
