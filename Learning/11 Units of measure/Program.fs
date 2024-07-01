// 11 Units of measure
// Learning F#, Play with Units of measure
//
// 2024-07-02   PV

// Units of measure are used for static type checking. When floating point values are compiled, the units of measure are
// eliminated, so the units are lost at run time. Therefore, any attempt to implement functionality that depends on
// checking the units at run time is not possible. For example, implementing a ToString function to print out the units
// is not possible.

module FirstTests =
    [<Measure>] type m
    [<Measure>] type s
    [<Measure>] type kg

    // Combine with multiplication and division (space or * to muliply)
    let distance = 1.0<m>
    let time = 2.0<s>
    let speed = 2.0<m/s>
    let acceleration = 2.0<m/s^2>
    let g = 9.81<m/s^ 2>
    let force = 5.0<kg m/s^2>


    // Derived units of measure
    [<Measure>] type N = kg m/s^2

    let force1 = 5.0<kg m/s^2>
    let force2 = 5.0<N>


open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// https://fsharp.github.io/fsharp-core-docs/reference/fsharp-data-unitsystems-si-unitsymbols.html
// Type		Description
// A		A synonym for ampere, the SI unit of electric current
// Bq		A synonym for becquerel, the SI unit of activity referred to a radionuclide
// C		A synonym for coulomb, the SI unit of electric charge, amount of electricity
// cd		A synonym for candela, the SI unit of luminous intensity
// F		A synonym for farad, the SI unit of capacitance
// Gy		A synonym for gray, the SI unit of absorbed dose
// H		A synonym for henry, the SI unit of inductance
// Hz		A synonym for hertz, the SI unit of frequency
// J		A synonym for joule, the SI unit of energy, work, amount of heat
// K		A synonym for kelvin, the SI unit of thermodynamic temperature
// kat		A synonym for katal, the SI unit of catalytic activity
// kg		A synonym for kilogram, the SI unit of mass
// lm		A synonym for lumen, the SI unit of luminous flux
// lx		A synonym for lux, the SI unit of illuminance
// m		A synonym for Metre, the SI unit of length
// mol		A synonym for mole, the SI unit of amount of substance
// N		A synonym for newton, the SI unit of force
// ohm		A synonym for UnitNames.ohm, the SI unit of electric resistance.
// Pa		A synonym for pascal, the SI unit of pressure, stress
// S		A synonym for siemens, the SI unit of electric conductance
// s		A synonym for second, the SI unit of time
// Sv		A synonym for sievert, the SI unit of does equivalent
// T		A synonym for tesla, the SI unit of magnetic flux density
// V		A synonym for volt, the SI unit of electric potential difference, electromotive force
// W		A synonym for watt, the SI unit of power, radiant flux
// Wb		A synonym for weber, the SI unit of magnetic fl

[<Measure>] type Hz = /s
let frequence = 50<Hz>
assert (frequence*1<s> = 50)    // Dimensionless
assert (frequence*1<s> = 50<1>) // Dimensionless

let tension = 12<V>
let intensité = 2<A>
let puissance = tension*intensité
assert (puissance = 24<V A>)
assert (puissance = 24<W>)      // Ok, works!

let pwr = 1.2<V A> + 0.8<W>     // Impressive!

type Person = {Prénom:string; Nom:string}
let pierre = {Prénom="Pierre"; Nom="Violent"}


// Conversions
[<Measure>] type g
[<Measure>] type cm
[<Measure>] type inch
[<Measure>] type ml
[<Measure>] type L

// Define conversion constants.
let gramsPerKilogram : float<g kg^-1> = 1000.0<g/kg>
let cmPerMeter : float<cm/m> = 100.0<cm/m>
let cmPerInch : float<cm/inch> = 2.54<cm/inch>

let mlPerCubicCentimeter : float<ml/cm^3> = 1.0<ml/cm^3>
let mlPerLiter : float<ml/L> = 1000.0<ml/L>

// Define conversion functions.
let convertGramsToKilograms (x : float<g>) = x / gramsPerKilogram
let convertCentimetersToInches (x : float<cm>) = x / cmPerInch

let length:float<inch> = convertCentimetersToInches 30.0<cm>


// Types annotations
let untypedTimesThree (inc:float) = inc * 3.0
let inchTimesThree (inc:float<inch>) =  inc * 3.0


// Dimensionless values
let x1 = 42
let x2 = 42<1>


// More conversions
[<Measure>] type degC
[<Measure>] type degF
let convertDegCToF c = c * 1.8<degF/degC> + 32.0<degF>

let f = convertDegCToF 0.0<degC>

// Convert dimensionless to measure
let ten = 10.0
let tenInches = ten * 1.0<inch>


// Generic units of measure
// let square x = x * x
// let res = square 10<foot>    // error

let square (x:int<_>) = x*x     // <_> indicates any unit.  val square: x: int<'u> -> int<'u ^ 2>
let surface1 = square 10<cm>
assert (surface1 = 100<cm^2>)

let surface2 = square 10<inch>

// Can use underscores or letters for generic units
let speed1 (distance:float<_>) (time:float<_>) = distance / time
let speed2 (distance:float<'u>) (time:float<'v>) = distance / time

// Generic type for constant
let length2 = 10.0<cm> + 2.5<_>

// Works when passig constants to higher order functions
[<Measure>] type foot
let feet = [ for i in [1.0..10.0] -> i * 1.0<foot> ]        // Ranges of floats with default step 1.0...

feet |> List.sum |> printfn "sum = %g"        
//feet |> List.fold (+) 0.0 |> printfn "fold+ = %g"         // error
feet |> List.fold (+) 0.0<_> |> printfn "fold+ = %g"


// Issues with generic measures with functions

// There are some cases where type inference fails us. For example, let’s try to create a simple add1 function that uses
// units.

// try to define a generic function
// let add1 n = n + 1.0<_>
// warning FS0064: This construct causes code to be less generic than indicated by the type annotations. The
// unit-of-measure variable 'u has been constrained to be measure '1'.
// add1 10.0<foot>     // error FS0001: This expression was expected to have type float but here has type float<foot>

// The warning message has the clue. The input parameter n has no measure, so the measure for 1<_> will always be
// ignored. The add1 function does not have a unit of measure so when you try to call it with a value that does have a
// measure, you get an error.

// So maybe the solution is to explicitly annotate the measure type, like this:
// define a function with explicit type annotation
// let add1 (n:float<'u>) : float<'u> =  n + 1.0<_>
// But no, you get the same warning FS0064 again.

// Maybe we can replace the underscore with something more explicit such as 1.0<'u>?
// let add2 (n:float<'u>) : float<'u> = n + 1.0<'u>
// error FS0634: Non-zero constants cannot have generic units.
// But this time we get a compiler error!

// The answer is to use one of the helpful utility functions in the LanguagePrimitives module: FloatWithMeasure,
// Int32WithMeasure, etc.
let add3 n  = n + (LanguagePrimitives.FloatWithMeasure 1.0)
// test
add3 10.0<foot> |> ignore   // Yes!

// And for generic ints, you can use the same approach:
let add2Int n  = n + (LanguagePrimitives.Int32WithMeasure 2)
add2Int 10<foot> |> ignore  // OK



// Using generic measures with type definitions
// What about when we need to use a unit of measure in a type definition?
// Say we want to define a generic coordinate record that works with an unit of measure. Let’s start with a naive
// approach:
// type Coord = { X: float<'u>; Y: float<'u>; }
// // error FS0039: The type parameter 'u' is not defined

// That didn’t work, so what about adding the measure as a type parameter:
// type Coord<'u> = { X: float<'u>; Y: float<'u>; }
// // error FS0702: Expected unit-of-measure parameter, not type parameter.
// // Explicit unit-of-measure parameters must be marked with the [<Measure>] attribute.

// That didn’t work either, but the error message tells us what to do. Here is the final, correct version, using the
// Measure attribute:
type Coord<[<Measure>] 'u> = { X: float<'u>; Y: float<'u>; }

let coord = {X=10.0<foot>; Y=2.0<foot>}


// In some cases, you might need to define more than one measure. In the following example, the currency exchange rate
// is defined as the ratio of two currencies, and so needs two generic measures to be defined.
type CurrencyRate<[<Measure>]'u, [<Measure>]'v> = { Rate: float<'u/'v>; Date: System.DateTime}

// test
[<Measure>] type EUR
[<Measure>] type USD
[<Measure>] type GBP

let mar1 = System.DateTime(2012,3,1)
let eurToUsdOnMar1 = {Rate= 1.2<USD/EUR>; Date=mar1 }
let eurToGbpOnMar1 = {Rate= 0.8<GBP/EUR>; Date=mar1 }

let tenEur = 10.0<EUR>
let tenEurInUsd = eurToUsdOnMar1.Rate * tenEur

// And of course, you can mix regular generic types with unit of measure types.
// For example, a product price might consist of a generic product type, plus a price with a currency:
type ProductPrice<'product, [<Measure>] 'currency> = { Product: 'product; Price: float<'currency>; }


// Units of measure at runtime

// An issue that you may run into is that units of measure are not part of the .NET type system. F# does stores extra
// metadata about them in the assembly, but this metadata is only understood by F#. This means that there is no (easy)
// way at runtime to determine what unit of measure a value has, nor any way to dynamically assign a unit of measure at
// runtime. It also means that there is no way to expose units of measure as part of a public API to another .NET
// language (except other F# assemblies).
