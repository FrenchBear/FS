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
printfn "result: %A" result



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
