// 002 Module
// Learning F#, using a module
// A more complete module with various content, a submodule and initialization code
//
// 2024-06-17   PV

namespace FS002

module MathStuff =

    let add x y  = x + y
    let subtract x y  = x - y

    // nested module
    [<RequireQualifiedAccess>]                  // Avoid accidental shadowing, forbid the use of "open FloatLib"
    module FloatLib =
        let add x y :float = x + y
        let subtract x y :float  = x - y

    // type definitions
    type Complex = {r:float; i:float}
    type IntegerFunction = int -> int -> int
    type DegreesOrRadians = Deg | Rad

    // "constant"
    let PI = 4.0*atan(1.0)

    // "variable"
    let mutable private TrigType = Deg

    module TrigDR =
        let sinDR a =
            match TrigType with
            | Rad -> sin a
            | Deg -> a/180.0*PI |> sin

        let degrees() =
            TrigType <- Deg
            ()

        let radians() =
            TrigType <- Rad
            ()

    // initialization / static constructor
    do printfn "module initialized"             // It seems that this code is not executed...
