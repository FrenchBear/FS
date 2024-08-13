// AllTypes.fs
// Play with type keyword, try all variants
//
// 2024-08-12   PV

module AllTypes

// Aliases
type Entier = int
type SB = System.Text.StringBuilder

// Arrays
type IntArray1 = int[]
type IntArray2 = array<int>
type IntArray3 = int array
type FloatArray2D = float[,]
type ComplexArray = array<int array*int[,]*array<int>>

// Tuples
type IntPair = int*int
type StrangeTuple = Entier*float[,,]*System.DateTime
type StrangerTuple = (((int array * int option) * (int list * int seq)) * int[,,] * int[,,]) * int

// Generics
type Generic1<'a> = 'a list
type Generic2<'T, 'U> = 'T[,] * System.Int32 list * list<'U>
type Dic<'key, 'value> = System.Collections.Generic.Dictionary<'key, 'value>

// Constructed types
type Constructed1 = int list
type Constructed2 = int option
type Constructed3 = int seq
type Constructed4 = int Set
type Constructed5 = int array
type Constructed6 = int ref

// Constructed generics
type ConsGen1a = List<int>
type ConsGen1b = list<int>
type ConsGen2a = Option<int>
type ConsGen2b = option<int>
type ConsGen3 = Set<int>
type ConsGen4 = array<int>
type ConsGen5 = Map<int,string>
type ConsGen6 = Result<int, string>
type ConsGen7 = ref<int>

// Functions signatures
type Sign1 = int -> double
type Sign2 = (int -> double) -> string
type Sign3 = bool array -> (int list -> double option) -> ((string Set -> byte ref) -> Result<uint64,sbyte>) -> unit
type Sign4<'a, 'b> = ('a -> 'b) -> 'a list -> 'b list

// Delegates
type Del1 = delegate of unit -> int
type Del2<'k, 'v> = delegate of 'k*'v -> Result<'v, 'k>
type WinEventDelegate = delegate of hWinEventHook:nativeint * eventType:uint32 * hWnd:nativeint * idObject:int * idChild:int * dwEventThread:uint32 * dwmsEventTime:uint32 -> unit

// Records
type Rec1 = { X: float; Y: float; Z: float }
type Rec2 =
    { First: string
      Last: string
      SSN: uint32
      AccountNumber: uint32 }
type Rec3<'a> =
    { Make: string
      Model: 'a
      mutable Odometer: int }
type Person = { Name: string; Age: int; Address: Address }
and Address = { Line1: string; Line2: string; PostCode: string; Occupant: Person }

// Discriminated unions
type DU1 = |Yes |No |Maybe
type DU1b = Oui|Non
type DU2 = Chaine of string
type DU2b = |Chaine of string
type DU3 = 
    |Color of int*int*int
    |GrayScale of int
    |NoCOlor
type DU4 =
    |Color of Red: int*Green: int*Blue: int
    |GrayScale of Luminosity: int
    |NoColor
type MyOption<'a> =
    | Some of 'a
    | None
type Tree =
    | Tip
    | Node of int * Tree * Tree

// Enums
type Color =
    | Red = 0
    | Green = 1
    | Blue = 2
type uColor = Red = 0u | Green = 1u | Blue = 2u

// Classes can't be defined without a body...
type MyType() = static member DoNothing() = ()
type public Class1<'T> internal(a: 'T) = 
    member this.printA() = printfn "%A" a

type MyClass2(dataIn) as self =
    let data = dataIn
    do
        self.PrintMessage()
    member this.PrintMessage() =
        printf "Creating MyClass2 with Data %d" data

type Folder(pathIn: string) =
    let path = pathIn
    let filenameArray: string array = System.IO.Directory.GetFiles(path)
    member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray
and File(filename: string, containingFolder: Folder) =
    member this.Name = filename
    member this.ContainingFolder = containingFolder

// Structs
type Point3D =
    struct
        val x: float
        val y: float
        val z: float
    end
type Point2D = struct val mutable x:float; val mutable y:float end

// Interfaces (declaration only)
type ISprintable =
    abstract member Print: format: string -> unit
    abstract member AsString: unit -> string
type INumericFSharp = abstract Add: x: int -> y: int -> int
type INumericDotNet = abstract Add: x: int * y: int -> int
type Interface3 =
    inherit INumericDotNet
    inherit INumericFSharp
    abstract member Method3: int -> int
type IA<'T> =
    abstract member Get : unit -> 'T


