// Hamming ditance calculations
// The Hamming distance between two strings or vectors of equal length is the number of positions at which the
// corresponding symbols are different
//
// 2024-07-15    PV      First version

open System

let hamming_distance (hash1:uint64) (hash2:uint64) =
    let mutable x = hash1 ^^^ hash2
    let m1 = 0x5555555555555555UL
    let m2 = 0x3333333333333333UL
    let h01 = 0x0101010101010101UL
    let m4 = 0x0f0f0f0f0f0f0f0fUL
    x <- x - ((x >>> 1) &&& m1)
    x <- (x &&& m2) + ((x >>> 2) &&& m2)
    x <- (x + (x >>> 4)) &&& m4
    int ((x * h01) >>> 56)

(*
// For 32-bit
// https://stackoverflow.com/questions/12171584/what-is-the-fastest-way-to-count-set-bits-in-uint32
    int NumberOfSetBits(int i)
    {
        i = i - ((i >> 1) & 0x55555555);
        i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
        return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
    }
*)

let hamming_distance_intrinsics (hash1:uint64) (hash2:uint64) =
        int (System.Runtime.Intrinsics.X86.Popcnt.X64.PopCount(hash1^^^hash2))


// Simple test
assert ((hamming_distance 871234UL 162332UL) = 12)

let bin64(n:uint64) = (Convert.ToString((int64 n), 2).PadLeft(64, '0'))

// Need an alias to use MaxValue
type longint = uint64

for i in [0..64] do
    let m = if i = 64 then longint.MaxValue else (1UL <<< i) - 1UL
    let n = 0UL
    let d = hamming_distance m n
    printfn "%2d %s %s" i (bin64 m) (bin64 n)
    assert (d=i)
    assert ((hamming_distance_intrinsics m n)=d)
