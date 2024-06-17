namespace FS002

module MathStuff =

    let add x y  = x + y
    let subtract x y  = x - y

    // nested module
    module FloatLib =

        let add x y :float = x + y
        let subtract x y :float  = x - y
