// 12 Why
// Learning F#, Exercises based on "Why using F#?"
//
// 2024-07-05   PV

type Point = Coords of X:double * Y:double

type Shape =
| Point of P:Point
| Line of P1:Point * P2:Point
| Rectangle of P1:Point * P2:Point
| Triangle of P1:Point * P2:Point * P3: Point
| Circle of Center:Point * Radius:double

let p1 = Coords (2.0, 3.0)
let p2 = Coords (4.0, -1.0)
let p3 = Coords (-1.0, 2.0)

let P = Point p1
let L = Line (p1, p2)
let R = Rectangle (p1, p2)
let T = Triangle (p1, p2, p3)
let C = Circle (p1, 3.14)

let printShape =
    function
    | Point p -> printfn "Point %A" p
    | Line (p1, p2) -> printfn "Line (%A, %A)" p1 p2
    | Rectangle (p1, p2) -> printfn "Rectangle (%A, %A)" p1 p2
    | Triangle (p1, p2, p3) -> printfn "Triangle (%A, %A, %A)" p1 p2 p3
    | Circle (c, r) -> printfn "Circle center %A, radius %g)" c r

[P;L;R;T;C] |> List.map printShape |> ignore
printfn ""


// --------------------------------
// Example of date format supporting stuff such as Absence or partial date (just month and year for instance)

type YMDate = {Year:int; Month:int}

type ExtendedDate =
| Empty                             // Nothing printed on the report
| Absence                           // "Absence" printed on the report
| YMDDate of System.DateOnly        // "Normal" D/M/Y date
| YMDate of YMDate                  // Just year and month
| YDate of int                      // Just year

let d1 = Empty
let d2 = Absence
let d3 = YMDDate (System.DateOnly(2024,07,14))
let d4 = YMDate {Year=2024; Month=7}
let d5 = YDate 2024

let printExtendedDate d =
    let extendedDateToString =
        function
        | Empty -> ""
        | Absence -> "Absence"
        | YMDDate fullDate -> fullDate.ToString("dd/MM/yyyy")
        | YMDate {Year=y;Month=m} ->
            let dt = System.DateOnly(y,m,1)
            dt.ToString("MMMM yyyy")            // juillet 2024
        | YDate y when y<100 -> (y+2000).ToString()
        | YDate y -> y.ToString()               // Using 'when y>=100' filter causes incomplete matches warning

    "Date: "+(extendedDateToString(d))

[d1;d2;d3;d4;d5] |> List.map (printExtendedDate >> printfn "%s") |> ignore

