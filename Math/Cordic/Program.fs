// Cordic calculations
//
// 2024-07-14    PV      First version

open System
let π = Math.PI

let cordicCompute angle =
    // Note that angle is expressed in radians, but CORDIC algorithm does not care about it, just
    // change initial variable a to 45 to work in degrees for instance

    let rec localCompute angle a vsin vcos s c =
        // If angle remaining to rotate is more than currently computed angle/s/c, we do the rotation
        let angle, vcos, vsin =
            if angle >= a then
                // Standard rotation matrix times vector (cos, sin)
                (angle - a, vcos * c - vsin * s, vcos * s + vsin * c)
            else
                // Jusr keep current values
                (angle, vcos, vsin)

        // Continue with half-angle for next step
        let a = a / 2.0

        match a < 1e-17 with    // 
        | true -> (vsin, vcos)
        | false ->
            // Half-trig computation, sin(a/2) and cos(a/2)
            let c2 = c
            let c = Math.Sqrt((c + 1.0) / 2.0)
            let s = s / Math.Sqrt(2.0 * (1.0 + c2))
            localCompute angle a vsin vcos s c


    match angle with
    | x when x<=5e-8 -> (angle, 1.0) // Simple result for very small angles, including 0
    | _ -> 
        // Start at π/4, with both sin and cos = (√2)/2
        let a = π / 4.0
        let s = Math.Sqrt(2.0) / 2.0
        let c = s

        // Start with horizontal unitary vector for result (the one that will be rotated)
        let vcos = 1.0
        let vsin = 0.0

        localCompute angle a vsin vcos s c


let sinCordic (angle: float) =
    // Get back into [0, π/2]

    // First use modulo of Abs to be in [0,2π[
    let sign = Math.Sign(angle)
    let angle = Math.Abs(angle) % (2.0 * π) 

    // Now get in [0, π]: [π, 2π] = opposite of sin in [0, π]
    let (angle, sign) =
        match angle with
        | x when x < π -> (angle, sign)
        | _ -> (angle - π, -sign)

    // Now get in [0, π/2]: sin is symmtric around x=π/2
    let angle =
        match angle with
        | x when x > π / 2.0 -> π - angle
        | _ -> angle

    let (vsin, _) = cordicCompute angle

    match sign with
    | 1 -> vsin
    | 0 -> 0.0
    | _ -> if vsin = 0.0 then vsin else -vsin // Don't want to return -0.0

// Simple version, but should probably have code ressembling to sinCordic using cos period and symmetries
let cosCordic angle = sinCordic (angle + π / 2.0)


// Tests, plot a sine curve from -9.75 to +9.75 (keep formatting on 5 characters)
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
let ans1 = sinCordic π
printfn "Cordic sin(π):       %f" ans1
let ans2 = sinCordic 1e-12
printfn "Cordic sin(1e-12):   %.15e" ans2

// This is wrong at the 5th decimal, this is normal because sin(π-1e12) is computed as sin((π-1e12)-π),
// and because of float rounding value, the result of (π-1e12)-π is 1.000088900582341e-12 and not 1e-12
// so the result is 1.000088900582341e-12...
let ans3 = sinCordic (π-1e-12)
printfn "Cordic sin(π-1e-12): %.15e" ans3
