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
printfn "mapList: %0A -> %0A\n" li lf

printfn "---\n"


// ============================================================
// Return
// T -> E<T>

let returnOption<'T> (x: 'T) = Some x
printfn "%A" (returnOption 42)

let returnList x = [ x ]
printfn "returnList 42 = %A\n" (returnList 42)

printfn "---\n"


// ============================================================
// Apply
// E<(T->U> -> E<T> -> E<U>
// applyOpt fOpt xOpt -> Some(f x)

let applyOpt fOpt xOpt =
    match fOpt, xOpt with
    | Some f, Some x -> Some(f x)
    | _ -> None

let x1 = Some 3.0
let f1 = (Some(fun x -> 1.0 / x))
let x2a = applyOpt f1 x1
printfn "x1 = %A" x1
printfn "x2a = %A" x2a

let x2b =
    let (<*>) = applyOpt
    f1 <*> x1

printfn "x2b = %A\n" x2b

// With 2-arg function +
let x3 =
    let (<*>) = applyOpt
    (Some (+)) <*> (Some 2) <*> (Some 3)

printfn "x3 = %A\n" x3

// After defining <!> as Option.map
let x4 =
    let (<!>) = Option.map
    let (<*>) = applyOpt
    (+) <!> (Some 2) <*> (Some 3)



// apply [f] [x;y] becomes [f x; f y]
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
    let temp1 = [ (+) ] <*> [ 1; 2; 3 ] // temp1: [(+) 1; (+) 2; (+) 3]
    let temp2 = temp1 <*> [ 10; 20; 30 ] // cartesian product of (functions list) and [10;20;30]

    // Compact version
    [ (+) ] <*> [ 1; 2; 3 ] <*> [ 10; 20; 30 ]

printfn "l3 = %A\n" l3

let l4resultList =
    let (<!>) = List.map
    let (<*>) = applyList

    let temp1 = (+) <!> [ 1; 2 ]
    let temp2 = temp1 <*> [ 10; 20 ]

    // List.map: ('T -> 'U) -> 'T list -> 'U List
    // But since here (+) is int -> int -> int, the result of List.map is (int -> int) list
    let temp1b = List.map (+) [ 1; 2 ] // [(+) 1; (+) 2]
    let temp2b = applyList temp1b [ 10; 20 ]

    // Compact version: first List.map (+) [1;2] that returns [(+) 1; (+) 2], and apply this result to [10;20] to get cartesian product
    (+) <!> [ 1; 2 ] <*> [ 10; 20 ]



// --------------------------
// Parenthesis on infix operators

let somme a b = a + b
let (<+>) = somme
let z = 2 <+> 3
printfn "z = %A" z


let fx f x = f x
let (<->) = fx
let res = (-) <-> 7 <-> 5
printfn "res = %A" res


printfn "---\n"


// ============================================================
// Lift



// For options
// lift1 = map  Option.map     fn (Some x)                   -> Some (fn x)
// lift2        MyOption.lift2 fn (Some x) (Some y)          -> Some (fn x y)
// lift3        MyOption.lift3 fn (Some x) (Some y) (Some z) -> Some (fn x y y)
// applyOpt     applyOpt       (Some fn) (Some x) -> Some(fn x)

module MyOption =
    let (<!>) = Option.map
    let (<*>) = applyOpt

    let lift2 f2 xOpt yOpt = f2 <!> xOpt <*> yOpt

    let lift2b f2 xOpt yOpt =
        let tmp1 = Option.map f2 xOpt
        let res = applyOpt tmp1 yOpt
        res

    let lift3 f3 xOpt yOpt zOpt = f3 <!> xOpt <*> yOpt <*> zOpt

    let lift4 f4 x y z w = f4 <!> x <*> y <*> z <*> w

let zzfn = (+)
let zzx = Some 3
let zzy = Some 5
let zzres = MyOption.lift2 zzfn zzx zzy
printfn "Option.lift2 (+) (Some 3) (Some 5) = %0A" zzres


// define a two-parameter function to test with
let addPair x y = x + y

// lift a two-param function
let addPairOpt = MyOption.lift2 addPair

// call as normal
let wwres = addPairOpt (Some 1) (Some 2)
printfn "addPairOpt (Some 1) (Some 2) = %0A" wwres
printfn ""



// For lists
// lift1 = map  List.map       fn (List x1 x2 x3...)                    -> List (fn x1) (fn x2) (fn x3)...
// lift2        MyList.lift2   fn (List x1 x2 x3...) (List y1 y2 y3...) -> List (fn x1 y1) (fn x1 y2) (fn x1 y3)... (fn x2 y1) (fn x2 y2) (fn x2 y3)... (fn x3 y1) (fn x3 y2) (fn x3 y3)... ...
// applyList    applyList      (List fn) (List x1 x2 x3...)             -> List (fn x1) (fn x2) (fn x3)...
// applyList    applyList      (List fn1 fn2) (List x1 x2 x3...)        -> List (fn1 x1) (fn1 x2) (fn1 x3)... (fn2 x1) (fn2 x2) (fn2 x3)...

module MyList =
    let (<!>) = List.map
    let (<*>) = applyList       // applyList is cartesian product of lists items

    let lift2 f2 xLst yLst = f2 <!> xLst <*> yLst

let max2Lists l1 l2 =
    let rec max2ListsRec l1 l2 res =
        match l1, l2 with
        | [], [] -> res
        | (h1 :: tail1),(h2 :: tail2) -> max2ListsRec tail1 tail2 (max h1 h2 :: res)
        | _ -> failwith "Lists must have the same length"

    max2ListsRec l1 l2 []


let tl1 = [ 5; -2; 3 ]
let tl2 = [ 1; 3; 5 ]
printfn "tl1 = %0A" tl1
printfn "tl2 = %0A" tl2

let tlm = max2Lists tl1 tl2
printfn "tlm = %0A" tlm

let tlp = List.map (fun k -> k+100) tl1
printfn "tlp = %0A" tlp

let tll = MyList.lift2 (+) tl1 tl2
printfn "tll = %0A" tll

let al1 = applyList [(fun x -> -x); (fun x -> x+100)] tl1
printfn "al1 = %0A" al1


printfn "---\n"


// ============================================================
