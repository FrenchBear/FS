// UniformIntRandom.cs - Random numbers generation, PV version, F#
// Implemented manually to be easily rewritten in other languages
// and produce same results
//
// 2017-09-02   PV
// 2018-08-21	PV      Used smaller constants because of JavaScript/TypeScript
// 2024-08-18	PV      F# version
//
// Values from https://en.wikipedia.org/wiki/Linear_congruential_generator

[<AutoOpen>]
module UniformUntRandom

[<Literal>]
let private M = 1u <<< 23

[<Literal>]
let private A = 65793u

[<Literal>]
let private C = 4282663u

type UniformUntRandom =
    { mutable V: uint32 }

    static member createNew seed =
        { UniformUntRandom.V = 
            if seed = 0
            then ((uint32 System.DateTime.Now.Millisecond) % M) 
            else uint(seed)
         }

    member this.maxValue = M

    member this.nextValue () =
        this.V <- (A * this.V + C) % M
        this.V >>> 8

    member this.nextDouble () =
        double (this.nextValue ()) / double M

    // % max is incorrect for uniform distribution, but impact is low if max << m (acceptable here)
    member this.nextInt max = int(this.nextValue () % uint(max))

    /// Return a random integer N such that a ≤ N ≤ b.
    member this.randInt a b = (this.nextInt (b + 1 - a)) + a

    static member test () =
        let r = UniformUntRandom.createNew 1
        let t = [ for _ in 1..20 -> r.randInt 1 6 ]
        printfn "%0A" t
        let u = r.nextDouble ()
        printfn "%A" u
        printfn "%A" (abs(u - 4.2319297790527344e-05) / u < 1e-12)
