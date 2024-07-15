// Square root calculation without division
// See tangente #214, p. 17
//
// 2024-07-15    PV      First version

open System

// Maximum relative difference
let εr = 1e-14

// Usual method, quadratic convergence, but requres float division
let square_root_heron r =
    let rec heron r u =
        let u' = (u + r / u) / 2.0
        match abs(u-u')/u < εr with
        | true -> u'
        | false -> heron r u'

    heron r (0.5 * r)    // u₀ is Ok for reasonably small values of r

// Schönage method, no float division (relative error calculation here doesn't count) and v₀ can be simplified since
// we can choose u₀
let square_root_nodiv r =
    let rec sqrtnodiv (r:float) u v =
        let u' = u + (r - u * u) * v
        let v' = v + (1.0 - 2.0 * u * v) * v
        match abs(u' - u) / u < εr with
        | true -> u'
        | false -> sqrtnodiv r u' v'

    let u0 = 0.5 * r
    let v0 = 1.0 / (2.0 * u0)
    sqrtnodiv r u0 v0

let inverse_nodiv b =
    let rec inv b v =
        let v' = v * (2.0 - b * v)
        match abs(v' - v) / v < εr with
        | true -> v'
        | false -> inv b v'

    inv b 0.1       // Need a starting value <1/b, so here it works if b<10


printfn "Square root calculation without division in F#\n"

let mutable r = sqrt(2.0)
printfn "√2 math  = %.14f" r

r <- square_root_heron(2.0)
printfn "√2 héron = %.14f" r

r <- square_root_nodiv(2.0)
printfn "√2 nodiv = %.14f\n" r

r <- 1.0 / 7.0
printfn "1/7 math  = %.15f" r

r <- inverse_nodiv(7.0)
printfn "1/7 nodiv = %.15f" r
