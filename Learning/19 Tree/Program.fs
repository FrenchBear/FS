// 19 Tree
// Learning F#, Trees using tuples and records
//
// First version with immutable tuples (but a mutable root) 
// Second version with mutable records
//
// 2024-07-21   PV

// Performance tests on WOTAN on 2024-07-21 (Debug)
// TupleTree: 00:00:03.9301572
// RecordTree: 00:00:02.7287040
// Compare results: 00:00:01.1415606

// First version with Tuples: works, but since tuples are immutable, after each insertion,
// the whole parent chain up to root must be recreated: not really efficient...
type TupleTree<'T when 'T :> System.IComparable> =
    Node of Left: TupleTree<'T> option * Value: 'T * Right: TupleTree<'T> option

// Return the updated root after insertion
// let rec addItemToTupleTree<'T when 'T :> System.IComparable> (root: TupleTree<'T> option) (value: 'T) : TupleTree<'T> option =
let rec addItemToTupleTree root value =
    match root with
    | None -> Some(Node(None, value, None)) // Reached a null node, replace it by a new node
    | Some(Node(left, nodeValue, right)) ->
        match sign (nodeValue.CompareTo(value)) with
        | 1 ->
            let newLeft = addItemToTupleTree left value // nodeValue>value -> insert on the left
            Some(Node(newLeft, nodeValue, right))
        | -1 ->
            let newRight = addItemToTupleTree right value // nodeValue<value -> insert on the right
            Some(Node(left, nodeValue, newRight))
        | _ -> root // value already exists, ignore it


// Print values in order
let rec printTupleTree root =
    match root with
    | None -> ()
    | Some(Node(left, nodeValue, right)) ->
        printTupleTree left
        printf "%A " nodeValue
        printTupleTree right


// Return sorted 'T list of values
let rec getTupleTreeValues root =
    match root with
    | None -> []
    | Some(Node(left, nodeValue, right)) -> (getTupleTreeValues left) @ [ nodeValue ] @ (getTupleTreeValues right)


let mutable troot: TupleTree<int> option = None

troot <- addItemToTupleTree troot 7
troot <- addItemToTupleTree troot 2
troot <- addItemToTupleTree troot -2
troot <- addItemToTupleTree troot 8
troot <- addItemToTupleTree troot -1
troot <- addItemToTupleTree troot 5
troot <- addItemToTupleTree troot 3
troot <- addItemToTupleTree troot 6
troot <- addItemToTupleTree troot 4
troot <- addItemToTupleTree troot 9
troot <- addItemToTupleTree troot 0
troot <- addItemToTupleTree troot 1

// Print values in order
printfn "TupleTree sorted values"
printTupleTree troot
printfn ""
printfn "values: %A" (getTupleTreeValues troot)
printfn ""



// Second version, using record with mutable fields
// Code is actually more complex, sice at each step of the descent, we can either continue deeper
// if node is not None, or create a new record and update node.
// But code should be faster...
type RecordTree<'T when 'T :> System.IComparable> = 
    { mutable Left: RecordTree<'T> option
      mutable Value: 'T option
      mutable Right: RecordTree<'T> option }

// Does not return anything, root is not mutable, only its fields are
//let rec addItemToRecordTree<'T when 'T :> System.IComparable> (root: RecordTree<'T>) (value: 'T) =
let rec addItemToRecordTree root value =
    match root with
    | { Left = _; Value = None; Right = _ } -> root.Value <- Some value

    | { Left = left; Value = Some(nodeValue); Right = right } ->
        match sign (nodeValue.CompareTo(value)) with
        | 1 -> // nodeValue>value -> insert on the left
            match left with
            | None ->
                root.Left <-
                    Some { Left = None; Value = Some value; Right = None }
            | Some leftRecord -> addItemToRecordTree leftRecord value |> ignore

        | -1 -> // nodeValue<value -> insert on the right
            match right with
            | None ->
                root.Right <- Some { Left = None; Value = Some value; Right = None }
            | Some rightRecord -> addItemToRecordTree rightRecord value |> ignore

        | _ -> ()


// Print values in order
let printRecordTree root =
    let rec subPrintRecordTree (root: RecordTree<'T> option) =
        match root with
        | Some { Left = left; Value = Some(nodeValue); Right = right } ->
            subPrintRecordTree left
            printf "%A " nodeValue
            subPrintRecordTree right
        | _ -> () // Actually this should be a None match, but compiler is complaining not all cases are covered...

    match root with
    | { Left = _; Value = None; Right = _ } -> printfn "RecordTree is empty"
    | _ -> subPrintRecordTree (Some root)


// Return sorted 'T list of values
let rec getRecordTreeValues root =
    match root with
    | Some { Left = left; Value = Some(nodeValue); Right = right } -> 
        (getRecordTreeValues left) @ [ nodeValue ] @ (getRecordTreeValues right)
    | _ -> []


let rroot: RecordTree<int> = { Left = None; Value = None; Right = None }

addItemToRecordTree rroot 7
addItemToRecordTree rroot 2
addItemToRecordTree rroot -2
addItemToRecordTree rroot 8
addItemToRecordTree rroot -1
addItemToRecordTree rroot 5
addItemToRecordTree rroot 3
addItemToRecordTree rroot 6
addItemToRecordTree rroot 4
addItemToRecordTree rroot 9
addItemToRecordTree rroot 0
addItemToRecordTree rroot 1

printfn "RecordTree sorted values"
printRecordTree rroot
printfn ""
printfn "values: %A" (getRecordTreeValues (Some rroot))
printfn ""


// Check that both trees get the same result
let l1 = (getTupleTreeValues troot)
let l2 = (getRecordTreeValues (Some rroot))
(l1, l2) ||> List.map2 (fun a b -> assert (a = b)) |> ignore

// Performance tests
printfn "Performance tests, inserting 1M values"

// Array.rendomShuffle will soon be added...
// https://github.com/fsharp/fslang-suggestions/issues/508
// https://github.com/dotnet/fsharp/pull/17277
// https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1135-random-functions-for-collections.md

// Until then, let's do it manually
// From https://www.fssnip.net/search/shuffle
let rand = new System.Random()

let swap x y (a: 'a[]) =
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp

// shuffle an array (in-place), but orginial snippet was missing "a |>" so we hat do pass the array twice such as "shuffle a a"...
let shuffle a =
    a |> Array.iteri (fun i _ -> a |> swap i (rand.Next(i, Array.length a)))

// Use a sequence of 1M values randomized
let a = [| 1..1000000 |]
shuffle a

// Let's make two identical copies to be sure to compare identical tree structures
let a1 = Array.copy a
let a2 = Array.copy a

// Tuple tree
let sw = System.Diagnostics.Stopwatch.StartNew()
let mutable tt: TupleTree<int> option = None
a1 |> Array.map (fun i -> tt <- addItemToTupleTree tt i) |> ignore
printfn "TupleTree: %A" sw.Elapsed

// Record tree
sw.Restart()

let rt: RecordTree<int> = { Left = None; Value = None; Right = None }

a1 |> Array.map (fun i -> addItemToRecordTree rt i) |> ignore
printfn "RecordTree: %A" sw.Elapsed


// extract ordered sequence of values of tt and rt and check they're identical
sw.Restart()
let sl1 = (getTupleTreeValues tt)
let sl2 = (getRecordTreeValues (Some rt))
(sl1, sl2) ||> List.map2 (fun a b -> assert (a = b)) |> ignore
printfn "Compare results: %A" sw.Elapsed
