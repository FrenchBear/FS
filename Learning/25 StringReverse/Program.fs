// 25 StringReverse
// Learning F#, Variations over string reverse
//
// 2024-08-08   PV

open System.Text

let s = "Hello, world"

let tc = s.ToCharArray()

let rec swapLetters (tc: 'T[]) i j =
    match i>=j with
    | true -> tc
    | false ->
        let tmp = tc[i]
        tc[i] <- tc[j]
        tc[j] <- tmp
        swapLetters tc (i+1) (j-1)

let ra = swapLetters tc 0 (Array.length tc-1)

let st = Array.fold (fun state ch -> $"{state}{ch}") "" ra
printfn "st: %s" st

let s2 = System.String(ra)
printfn "s2: %s" s2


let stringReverse (s: string) = System.String(Array.rev (s.ToCharArray()))
stringReverse s |> printfn "sr: %s" 



let revList = List.rev (Seq.toList s)
//let revList = List.rev (List.ofSeq s)     // Same thing

// Now convert list of chars to string...

// Attention, string x means x.ToString() so we just get typename in some cases, while System.String(x) accepts overloads making "real" conversion,
// including for "char array"
let sr1 = revList |> List.toArray |> System.String
printfn "sr1: %s" sr1

let sr2 = System.String.Concat(Array.ofList(revList))
printfn "sr2: %s" sr2

//Using an array builder
let sr3 = new string [|for c in revList -> c|]
printfn "sr3: %s" sr3

//StringBuilder-Lisp-like approach
let sr4 = string (List.fold (fun (sb:StringBuilder) (c:char) -> sb.Append(c)) (new StringBuilder()) revList)
printfn "sr4: %s" sr4

let sr5 = List.fold (fun state ch -> $"{state}{ch}") "" revList
printfn "sr5: %s" sr5

let sr5b = List.fold (fun state ch -> state + string ch) "" revList
printfn "sr5b: %s" sr5b

let rec join5 = function  [] -> "" | h::t -> string h + join5 t
let sr5c = join5 revList
printfn "sr5c: %s" sr5c

let sr6 = List.map string revList |> String.concat ""
printfn "sr6: %s" sr6

//Continuation passing style
let sr7 =
    let rec aux L k =
        match L with
        | [] -> k ""
        | h::t -> aux t (fun rest -> k (string h + rest) )
    aux revList id
printfn "sr7: %s" sr7

let sr8 =
    let rec accum lst accSoFar =
        match lst with
        | [] -> accSoFar
        | head::tail -> accum tail (accSoFar+string head)
    accum revList ""
printfn "sr8: %s" sr8

let join (xs:char list) =
    List.map string xs |> List.reduce (+) 
let sr9 = join revList
printfn "sr9: %s" sr9

let implode (xs:char list) =
    let sb = System.Text.StringBuilder(xs.Length)
    xs |> List.iter (sb.Append >> ignore)
    //List.iter (sb.Append >> ignore) xs
    sb.ToString()
let sr10 = implode revList
printfn "sr10: %s" sr10

let sr11 = System.String.Join("", revList)
printfn "sr11: %s" sr11


/// With sequences
let revSeq = Seq.rev s

let sr12 = System.String.Join("", revSeq)
printfn "sr12: %s" sr12

// Recursive iteration of a sequence, Seq.toArray (beware, use System.String, not string...)
let sr13 = Seq.toArray revSeq |> System.String
printfn "sr13: %s" sr13

let sr14 = Seq.fold (fun acc ch -> acc + string ch) "" revSeq
printfn "sr14: %s" sr14

let sr15 =
    let mutable acc = ""
    Seq.iter (fun item -> acc <- acc + string item) revSeq
    acc
printfn "sr15: %s" sr15


//let explode (s:string) =
//    [for c in s -> c]



