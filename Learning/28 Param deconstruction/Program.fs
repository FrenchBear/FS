// 28 Param deconstruction
// Rappel, deconstruction des paramètres d'une fonction
//
// 2024-08-11   PV

type EmailContactInfo =
    { EmailAddress: string
      IsEmailVerified: bool }


let eci =
    { EmailAddress = "168 allée de la Bâtie"
      IsEmailVerified = true }

let { EmailAddress = ad
      IsEmailVerified = ver } =
    eci

// Match/deconstruct record in function parameters
let printead
    { EmailAddress = ad
      IsEmailVerified = ver }
    =
    printfn "Address: %s" ad

printead eci


type Kolor = RGB of int * int * int

// Match/deconstruct parameter of single case discriminated union
let printKolor (RGB(r, g, b)) = printfn "red=%d, green=%d, b=%d" r g b

let red = RGB(255, 0, 0)


// Discriminated union with multiple cases
type AdvancedKolor =
    | RGB of int * int * int
    | GrayScale of int

let printAdvancedKolor (RGB(r, g, b)) = // Ok, warning...
    printfn "Advanced: red=%d, green=%d, b=%d" r g b

let ac = RGB(100, 85, 210)
printAdvancedKolor ac

let ak = GrayScale(64)
// printAdvancedKolor ak        // Fails at execution, since deconstruct of printAdvancedKolor doesn't support this case
