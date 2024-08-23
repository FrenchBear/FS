// ParMillerRandom.fs
// Personal random generator, a linear congruential generator producing 31-bit random integers to use in languages comparisons
// F# version without tests
//
// 2024-08-18    PV
// https://en.wikipedia.org/wiki/Lehmer_random_number_generator#Parameters_in_common_use


[<AutoOpen>]
module ParkMillerRandom

type ParkMillerRandom =
    { mutable State: uint32 }

    static member createNew seed =
        { ParkMillerRandom.State =
            if seed = 0 then
                ((uint32 System.DateTime.Now.Millisecond) % 0x7fffffffu)
            else
                uint (seed) }

    static member maxValue = 0x7fffffff

    member this.nextValue() =
        this.State <- uint32 (uint64 (this.State) * 48271uL % 0x7fffffffuL)
        int (this.State)

    /// Return a random double number with linear distribution over [0..1[.
    member this.nextDouble() =
        double (this.nextValue ()) / double 0x80000000u

    // % max is incorrect for uniform distribution, but impact is low if max << m (acceptable here)
    /// Return a random integer with linear distribution over [0..max[.
    member this.nextInt max = int (this.nextValue ()) % max

    /// Return a random integer N such that a<=N<=b with linear distribution.
    member this.randInt a b = (this.nextInt (b + 1 - a)) + a
