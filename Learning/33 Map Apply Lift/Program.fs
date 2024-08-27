// 33 Map Apply Lift.fs
// Learning F# II, Map, apply, lift
//
// 2024-08-27   PV

// ============================================================
// Map
// (T -> U) -> E<T> -> E<U>

let si = Some 42
let sf = si |> Option.map (fun x -> 1.0 / double x)
printfn "%0A -> %0A\n" si sf


let add1 x = x + 1 // "normal world" function
let add1IfSomething = Option.map add1 // "elevated world" version

let lio = [ None; Some 2; Some 3; Some 5 ]
let lip = List.map add1IfSomething lio
printfn "%0A\n%0A\n" lio lip


type Triplet<'T> =
    { Item1: 'T
      Item2: 'T
      Item3: 'T }

    static member Map (fn: ('T -> 'U)) t =
        let u =
            { Item1 = fn t.Item1
              Item2 = fn t.Item2
              Item3 = fn t.Item3 }

        u

let ti: Triplet<int> = { Item1 = 1; Item2 = 2; Item3 = 3 }
let tf = ti |> Triplet<_>.Map(fun x -> 1.0 / double x)
printfn "%0A -> %0A\n" ti tf


let rec mapList fn lst =
    match lst with
    | [] -> []
    | head :: tail -> (fn head) :: (mapList fn tail)

let li = [ 1; 2; 3 ]
let lf = li |> mapList (fun x -> 1.0 / double x)
printfn "%0A -> %0A\n" li lf

printfn "---\n"


// ============================================================
// Return
// T -> E<T>

let returnOption<'T> (x: 'T) = Some x
printfn "%A" (returnOption 42)

let returnList x = [ x ]
printfn "%A\n" (returnList 42)

printfn "---\n"


// ============================================================
// Apply
// E<(T->U> -> E<T> -> E<U>

let applyOpt fOpt xOpt =
    match fOpt, xOpt with
    | Some f, Some x -> Some(f x)
    | _ -> None

let x1 = Some 3.0
let x2 = applyOpt (Some(fun x -> 1.0 / x)) x1
printfn "x1 = %A" x1
printfn "x2 = %A" x2

let x3 =
    let (<*>) = applyOpt
    (Some (+)) <*> (Some 2) <*> (Some 3)

printfn "x3 = %A\n" x3

let x4 =
    let (<!>) = Option.map
    let (<*>) = applyOpt
    (+) <!> (Some 2) <*> (Some 3)



// apply [f;g] [x;y] becomes [f x; f y; g x; g y]
let applyList (fList: ('a -> 'b) list) (xList: 'a list) =
    [ for f in fList do
          for x in xList do
              yield f x ]

let l1 = [ "Once"; "upon"; "a"; "time" ]
let l2 = applyList [ (fun s -> String.length s); (fun (s: string) -> 42) ] l1
printfn "l1 = %A" l1
printfn "l2 = %A" l2

let l3 =
    let (<*>) = applyList

    // For debug, to understand...
    let temp1 = [ (+) ] <*> [ 1; 2; 3 ]         // temp1: [(+) 1; (+) 2; (+) 3]
    let temp2 = temp1  <*> [ 10; 20; 30 ]       // cartesian product of (functions list) and [10;20;30]

    // Compact version
    [ (+) ] <*> [ 1; 2; 3 ] <*> [ 10; 20; 30 ]

printfn "l3 = %A\n" l3

let l4resultList =
    let (<!>) = List.map
    let (<*>) = applyList

    let temp1 = (+) <!> [1;2]
    let temp2 = temp1 <*> [10;20]

    // List.map: ('T -> 'U) -> 'T list -> 'U List
    // But since here (+) is int -> int -> int, the result of List.map is (int -> int) list
    let temp1b = List.map (+) [1;2]         // [(+) 1; (+) 2]
    let temp2b = applyList temp1b [10;20]

    // Compact version: first List.map (+) [1;2] that returns [(+) 1; (+) 2], and apply this result to [10;20] to get cartesian product
    (+) <!> [1;2] <*> [10;20]



// --------------------------
// Parenthesis on infix operators

let somme a b = a+b
let (<+>) = somme
let z = 2 <+> 3
printfn "z = %A" z


let fx f x = f x
let (<->) = fx
let res = (-) <-> 7 <-> 5
printfn "res = %A" res


printfn "---\n"


// ============================================================

