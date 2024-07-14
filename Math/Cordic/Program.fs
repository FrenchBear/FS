// Cordic calculations
//
// 2024-07-14    PV      First version

open System
let π = Math.PI

let cordicCompute angle =
    // Note that angle is expressed in radians, but CORDIC algorithm does not care about it, just
    // change initial variable a to 45 to work in degrees for instance

    // Start at π/4, with both sin and cos = (√2)/2
    let a = π / 4.0
    let s = Math.Sqrt(2.0) / 2.0
    let c = s

    // Start with horizontal unitary vector for result
    let vcos = 1.0
    let vsin = 0.0

    let rec localCompute angle a vsin vcos s c =
        // If angle remaining to rotate is more than currently computed angle/s/c, we do the rotation
        let angle, vcos, vsin =
            if angle >= a then
                // Standard rotation matrix times vector (cos, sin)
                (angle - a, vcos * c - vsin * s, vcos * s + vsin * c)
            else
                (angle, vcos, vsin)

        // Compute sin and cos of half-angle for next step
        let a = a / 2.0

        match a < 1e-17 with
        | true -> (vsin, vcos)
        | false ->
            // Half-trig computation, sin(a/2) and cos(a/2)
            let c2 = c
            let c = Math.Sqrt((c + 1.0) / 2.0)
            let s = s / Math.Sqrt(2.0 * (1.0 + c2))
            localCompute angle a vsin vcos s c


    match angle with
    | 0.0 -> (vsin, vcos) // Simple time-saver
    | _ -> localCompute angle a vsin vcos s c


let sinCordic (angle: float) =
    // Get back into [0, π/2]
    let sign = Math.Sign(angle)
    let angle = Math.Abs(angle) % (2.0 * π) // Use modulo to be in [0,2π[

    // Now get in [0,π]
    let (angle, sign) =
        match angle with
        | x when x < π -> (angle, sign)
        | _ -> (angle - π, -sign)

    // Now get in [0, π/2]
    let angle =
        match angle with
        | x when x > π / 2.0 -> π - angle
        | _ -> angle

    let (vsin, _) = cordicCompute (angle)

    match sign with
    | 1 -> vsin
    | 0 -> 0.0
    | _ -> if vsin = 0.0 then vsin else -vsin // Don't want to return -0.0

// Simple version, but should probably have code ressembling to sinCordic 
let cosCordic angle = sinCordic (angle + π / 2.0)


// Tests, just use Float64
for z in [-9.75..0.25..9.9] do
    let s = sinCordic(z)
    printf "sin(%5.2f) = {%15.12f} " z s
    printfn " %*c" (int(20.0*(1.0+s)))  '*'

printfn ""

// Take a random angle (0..Pi/2)
let a0 = 1.1823614786
let (vsin, vcos) = cordicCompute (a0)

printfn "a=%f" a0
printfn "Math:   c=%.16f\t\t\ts=%.16f\t\t\t(Math.Cos and Math.Sin)" (Math.Cos(a0)) (Math.Sin(a0))
printfn "Cordic: c=%.16f\t\t\ts=%.16f\t\t\t(Cordic Cos and Sin)" vcos vsin
printfn "Maple:  c=0.378740326955891541643393287014\ts=0.925502979323861698653734026619\n"


// Just checking I'm not working at HP, sin(π) is zero! :-)
let ans = sinCordic (π)
printfn "Cordic sin(π):  %g" ans
