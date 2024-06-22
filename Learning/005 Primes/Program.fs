// 005 Primes
// Learning F#, Prime numbers sieve
//
// 2024-06-22   PV

// Readable version
let primesUpTo n =
    let rec sieve listOfNumbers =
        match listOfNumbers with
        | [] -> []
        | primeP::sivedNumbersBiggerThanP ->
            let sievedNumbersNotDivisibleByP = sivedNumbersBiggerThanP |> List.filter (fun i -> i%primeP<>0)
            let newPrimes = sieve sievedNumbersNotDivisibleByP      // recursive call
            primeP::newPrimes
    // Use the sieve
    let listOfNumbers = [2..n]
    sieve listOfNumbers

// Test
let res = primesUpTo 100
printfn "%A" res

// Idiomatic version
let primesUpTo2 n =
    let rec sieve l =
        match l with
        | [] -> []
        | p::xs -> p::sieve [for x in xs do if x%p<>0 then yield x]
    [2..n] |> sieve

// Test
let res2 = primesUpTo 100
printfn "%A" res2

// Generating and printing a sequence of random numbers
let printRamndomNumbersUntilMatched matchValue maxValue =
    let randomNumberGenerator = new System.Random()
    let sequenceGenerator _ = randomNumberGenerator.Next(maxValue)
    let isNotNatch = (<>) matchValue

    // Create a process the sequence of rands
    Seq.initInfinite sequenceGenerator
        |> Seq.takeWhile isNotNatch
        |> Seq.iter (printf "%d ")

    // done
    printfn "\nFound a %d!" matchValue

// test
printRamndomNumbersUntilMatched 10 20
