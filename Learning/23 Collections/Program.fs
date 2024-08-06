// 23 Collections
// Learning F#, Various F3 collections
//
// 2024-07-31   PV


// Sequences

let seq1 =
    0 // Initial state
    |> Seq.unfold (fun state ->
        if (state > 20) then
            None
        else
            Some(state*state, state + 1))

printfn "The sequence seq1 contains numbers from 0² to 20²"

for x in seq1 do
    printf "%d " x
printfn ""


// Returns an int list
let primesUpTo n =
   let rec sieve l  =
      match l with
      | [] -> []
      | p::xs -> p :: sieve [for x in xs do if (x % p) > 0 then yield x]
   [2..n] |> sieve



let (|SeqEmpty|SeqCons|) (xs: 'a seq) = 
  if Seq.isEmpty xs then SeqEmpty
  else SeqCons(Seq.head xs, Seq.skip 1 xs)

// Stupid example usage
let a = [1; 2; 3]

let f = function
  | SeqEmpty -> 0
  | SeqCons(x, rest) -> x
  
let result = f a
printfn "result: %A\n" result



// ----------------------------------------------------------------------------------
// Maps (dictionaries)

type Orientation = E|N|O|S|I
type Blocks = Hz|Hz1|Vt|Dr|Dl|Ur|Ul|Xx

// Map<Blocks,string>
let BoxesS = Map [Hz,"──"; Hz1,"─ "; Vt,"│ "; Dr,"┌─"; Dl,"┐ "; Ur,"└─"; Ul,"┘ "; Xx,"??"]

// Map of Maps: Map<Orientation,Map<Orientation,Blocks>>
let IOMap = Map [
        E, Map [E,Hz; N,Ul; O,Xx; S,Dl; I,Hz1]; 
        N, Map [E,Dr; N,Vt; O,Dl; S,Xx; I,Vt]; 
        O, Map [E,Xx; N,Ur; O,Hz; S,Dr; I,Hz1]; 
        S, Map [E,Ur; N,Xx; O,Ul; S,Vt; I,Vt]; 
        I, Map [E,Hz; N,Vt; O,Hz; S,Vt; I,Xx]; 
    ]
    
// 2D array initialized
let depth = 3
let side = 1<<<depth
let tc = Array2D.create side side Xx    


// ----------------------------------------------------------------------------------
// Array mutability

// Verify that an array is mutable, is passed "byref" to a function, and it remains mutable inside the function
let tsi = [| 1;2;3;4;5 |]
tsi[0] <- -1

let testMutArray (t: int array) = 
    t[1] <- -12
    3.14

printfn "Original array: %A" tsi
let _ = testMutArray tsi
printfn "Mutated array: %A\n" tsi


// ----------------------------------------------------------------------------------
// Iterations




// Recursive iteration over an array, need to use index (head::tail deconstruction isn't supported by arrays)
let productArray (ti: int array) =
    let rec aux i acc =
        match i with
        | -1 -> acc
        | _ -> aux (i-1) (acc*ti[i])
    aux (Array.length ti - 1) 1

let ti = [| 2;3;5;7;11;13 |]
let pti = productArray ti
printfn "pti: %A" pti


// Recursive iteration over a list using head::tail
let productList l =
    let rec aux lst acc = 
        match lst with
        | [] -> acc
        | head::tail -> aux tail (acc*head)
    aux l 1

let li = [2;3;5;7;11;13]
let pli = productList li
printfn "pli: %A" pli


// Recursive iteration of a sequence, Seq.reduce
let productSequence1 (s: int seq) =
    Seq.reduce (*) s
let si = seq {2;3;5;7;11;13}
let psi1 = productSequence1 si
printfn "psi1: %A" psi1

// Recursive iteration of a sequence, Seq.fold
let productSequence2 (s: int seq) =
    Seq.fold (fun acc item -> acc*item) 1 s
let psi2 = productSequence2 si
printfn "psi2: %A" psi2

// Recursive iteration of a sequence, Seq.iter
let productSequence3 (s: int seq) =
    let mutable acc = 1
    Seq.iter (fun item -> acc <- acc*item) s
    acc
let psi3 = productSequence3 si
printfn "psi3: %A" psi3

// Recursive iteration of a sequence, Seq.head, seq.tail
let productSequence4 (s: int seq) =
    let rec aux (s: int seq) acc =
        if Seq.isEmpty s
        then acc
        else 
            let head, tail = Seq.head s, Seq.tail s
            aux tail (head*acc)
    aux s 1
let psi4 = productSequence4 si
printfn "psi4: %A" psi4

