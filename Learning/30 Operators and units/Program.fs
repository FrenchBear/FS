// 30 Operators and units
// Learning F#
//
// 2024-08-13   PV


// Creating New Operators
// You can overload all the standard operators, but you can also create new operators out of sequences of certain
// characters. Allowed operator characters are !, $, %, &, *, +, -, ., /, <, =, >, ?, @, ^, |, and ~. The ~ character
// has the special meaning of making an operator unary, and is not part of the operator character sequence. Not all
// operators can be made unary.
// Note that : can build operators : :: :> :? :?>
// [...]    constant list or index to an array or seq or Map or ...
// ()       unit type
// (...)    tuple, or .net call/class arguments or sub expression or ...
// [|...|]  array
// (|...|)  active pattern
// {|...|}  anonymous record
// [<...>]  attribute
// <@...@>  quoted expression
// ``...``  protect identifier

let s1 = [1;2;3]
let s2 = seq [1;2;3]
let s3 = Map [1,2;3,4]
let t1 = (1,2)
let q1 = <@ 1+2*3 @>

let (==) a b = a*b 
let (.*) a b = a*b 
let (+-*/) a b = a+b-2*3/4
let (--) a = -a
let (-->) a b = -a+b
let (->>) a b = a*b
let (+-) a b = a+10*b
let (~%) a = a+1        // Defines an unary operator without impacting existing binary % operator
//let (~!) z = z+1      // Invalid operator redefinition
let (!) z = not z
let (!=) a b = a <> b
let (^) a b = a ** b

let z1 = 2 +-*/ 3
let z2 = (+-*/) 2 3
let z3 = (--)3
let z4 = 2+ -3          // -1: normal + and -
let z5 = 2+-3           // 32: operator +-
let z6 = 2%3            // 2: Standard modulo
let z7 = %4             // 5: Redefined unary operator %
let z8 = !true
let z9 = 12!=24
let z10 = 2.0 ** 0.5    // foat power float

open Microsoft.FSharp.Linq
let n1 = new System.Nullable<int>(1)
let n2 = new System.Nullable<int>(2)
let r1 = n1 ?*? n2

let otn (x:'a option) = 
    match x with
    | Some v -> new System.Nullable<'a>(v)
    | None -> new System.Nullable<'a>()

let oi1 = Some 42
let oi2:int option = None

let ni1 = otn oi1
let ni2 = otn oi2

[<Measure>] type kg
[<Measure>] type m
[<Measure>] type s
[<Measure>] type N = kg m / s^2
[<Measure>] type Pa = N / m^2

[<Measure>] type cm
[<Measure>] type inch

let uq1 = 5.0<cm>
let uq2 = 25<kg>*2<m>/5<s^2>
let uq3 = 12<N>
let uq4 = uq2-uq3       // Same dimension


// Define conversion constants.
let cmPerMeter : float<cm/m> = 100.0<cm/m>
let cmPerInch : float<cm/inch> = 2.54<cm/inch>

// Define conversion functions.
let convertCentimetersToInches (x : float<cm>) = x / cmPerInch
let convertInchesToCentimeters (x : float<cm>) = x * cmPerInch


[<Measure>] type degC // temperature, Celsius/Centigrade
[<Measure>] type degF // temperature, Fahrenheit

let convertCtoF ( temp : float<degC> ) = 9.0<degF> / 5.0<degC> * temp + 32.0<degF>
let convertFtoC ( temp: float<degF> ) = 5.0<degC> / 9.0<degF> * ( temp - 32.0<degF>)

// Define conversion functions from dimensionless floating point values.
let degreesFahrenheit temp = temp * 1.0<degF>
let degreesCelsius temp = temp * 1.0<degC>


let genericSumUnits ( x : float<'u>) (y: float<'u>) = x + y

let v1 = 3.1<m/s>
let v2 = 2.7<m/s>
let x1 = 1.2<m>
let x2 = 1.0<s>

// OK: a function that has unit consistency checking.
let result1 = genericSumUnits v1 v2
// Error reported: mismatched units.
// let result2 = genericSumUnits v1 x1


// Define a vector together with a measure type parameter.
// Note the attribute applied to the type parameter.
type vector3D<[<Measure>] 'u> = { x : float<'u>; y : float<'u>; z : float<'u>}

// Create instances that have two different measures.
// Create a position vector.
let xvec : vector3D<m> = { x = 0.0<m>; y = 0.0<m>; z = 0.0<m> }
// Create a velocity vector.
let v1vec : vector3D<m/s> = { x = 1.0<m/s>; y = -1.0<m/s>; z = 0.0<m/s> }


// Conversion
let length = 12.0<cm>
let x = float length

