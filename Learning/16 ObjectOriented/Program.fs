// 16 ObjectOriented code in F#
// Learning F#, Exercises based on "Why using F#?", Part 5, Anything C# can do
//
// 2024-07-15   PV

// ---------------------------------------------------------------------------
// Classes and interfaces

printfn "Classes and interfaces\n"

// First, here are some examples of an interface, an abstract class, and a concrete class that inherits from the abstract class.

// interface
type IEnumerator<'a> =
    abstract member Current : 'a
    abstract MoveNext : unit -> bool

// abstract base class with virtual methods
[<AbstractClass>]
type Shape() =
    // readonly properties
    abstract member Width : int with get
    abstract member Height : int with get

    // non-virtual method (actually, a property ?)
    member this.BoundingArea = this.Height * this.Width

    // virtual method with base implementation
    abstract member Print : unit -> unit
    default this.Print () = printfn "I'm a shape"

// concrete class that inherits from base class and overrides
type Rectangle(x:int, y:int) =      // Is this the equivalent of a primary constructor?
    inherit Shape()

    override this.Width = x
    override this.Height = y
    override this.Print ()  = printfn "I'm a Rectangle"

// test
let r = Rectangle(2,3)
printfn "The width is %i" r.Width
printfn "The area is %i" r.BoundingArea
r.Print()


// Classes can have multiple constructors, mutable properties, and so on.
type Circle(rad:int) =
    inherit Shape()

    // mutable field
    let mutable radius = rad        // Note that 'let' makes this field private

    // property overrides
    override this.Width = radius * 2
    override this.Height = radius * 2

    // alternate constructor with default radius
    new() = Circle(10)
    new(r1:int, r2:int) = Circle((r1+r2)/2)

    // property with get and set
    member this.Radius
         with get() = radius
         and set(value) = radius <- value

// test constructors
let c1 = Circle()   // parameterless ctor
printfn "The width is %i" c1.Width
let c2 = Circle(2)  // main ctor
printfn "The width is %i" c2.Width
// test mutable property
c2.Radius <- 3
printfn "The width is %i" c2.Width
printfn "The radius is now %i" c2.Radius
//printfn "The radius is %i" c2.radius    // Not accessible

// test alt constructor
let c3 = Circle(1,2)


// ---------------------------------------------------------------------------
// Generics

printfn "\n-------------------------------"
printfn "Generics\n"

// F# supports generics and all the associated constraints.

// standard generics
type KeyValuePair<'a,'b>(key:'a, value: 'b) =
    member this.Key = key
    member this.Value = value

    // Add a constructor to build a KeyValuePair from the "official" obe used in dictionaries
    new(kvp: System.Collections.Generic.KeyValuePair<'a,'b>) = KeyValuePair(kvp.Key, kvp.Value)

    // Ovveride ToString
    override this.ToString() =
        sprintf "KeyValuePair(%A, %A)" this.Key this.Value

// generics with constraints
type Container<'a,'b
    when 'a : equality
    and 'b :> System.Collections.ICollection>
    (name:'a, values:'b) =
    member this.Name = name
    member this.Values = values

let k1 = System.Collections.Generic.KeyValuePair(1, "One")
let k2 = KeyValuePair(k1)   // Conversion

printfn "k1 = %A" k1
printfn "k2 = %A" k2

// ---------------------------------------------------------------------------
// Structs

printfn "\n-------------------------------"
printfn "Structs\n"

// F# supports not just classes, but the .NET struct types as well, which can help to boost performance in certain cases.
type Point2D =
   struct
      val X: float
      val Y: float
      new(x: float, y: float) = { X = x; Y = y }
   end

   override this.ToString () =
        sprintf "Point2D(X=%A Y=%A)" this.X this.Y      // %A includes .0, %g does not, and %f includes .00000

// test
let p = Point2D()  // zero initialized
let p2 = Point2D(2.0,3.0)  // explicitly initialized

printfn "p = %A" p
printfn "p2 = %A" p2


// ---------------------------------------------------------------------------
// Exceptions

printfn "\n-------------------------------"
printfn "Exceptions\n"

// F# can create exception classes, raise them and catch them.

// create a new Exception class
exception MyError of string
try
    let e = MyError("Oops!")
    raise e
with
    | MyError msg ->
        printfn "The exception error was %s" msg
    | _ ->
        printfn "Some other exception"


(*
// ---------------------------------------------------------------------------
// Extension methods

printfn "\n-------------------------------"
printfn "Extension methods\n"

Just as in C#, F# can extend existing classes with extension methods.
type System.String with
    member this.StartsWithA = this.StartsWith "A"
let s = "Alice"
printfn "'%s' starts with an 'A' = %A" s s.StartsWithA
type System.Int32 with
    member this.IsEven = this % 2 = 0
let i = 20
if i.IsEven then printfn "'%i' is even" i
Parameter arrays
Just like C#’s variable length “params” keyword, this allows a variable length list of arguments to be converted to a single array parameter.
open System
type MyConsole() =
    member this.WriteLine([<ParamArray>] args: Object[]) =
        for arg in args do
            printfn "%A" arg
let cons = new MyConsole()
cons.WriteLine("abc", 42, 3.14, true)
Events
F# classes can have events, and the events can be triggered and responded to.
type MyButton() =
    let clickEvent = new Event<_>()
    [<CLIEvent>]
    member this.OnClick = clickEvent.Publish

    member this.TestEvent(arg) =
        clickEvent.Trigger(this, arg)

let myButton = new MyButton()
myButton.OnClick.Add(fun (sender, arg) ->
        printfn "Click event with arg=%O" arg)

myButton.TestEvent("Hello World!")
Delegates
F# can do delegates.
// delegates
type MyDelegate = delegate of int -> int
let f = MyDelegate (fun x -> x * x)
let result = f.Invoke(5)
Enums
F# supports CLI enums types, which look similar to the “union” types, but are actually different behind the scenes.
// enums
type Color = | Red=1 | Green=2 | Blue=3

let color1  = Color.Red    // simple assignment
let color2:Color = enum 2  // cast from int
// created from parsing a string
let color3 = System.Enum.Parse(typeof<Color>,"Green") :?> Color // :?> is a downcast

[<System.Flags>]
type FileAccess = | Read=1 | Write=2 | Execute=4
let fileaccess = FileAccess.Read ||| FileAccess.Write
Working with the standard user interface
Finally, F# can work with the WinForms and WPF user interface libraries, just like C#.
Here is a trivial example of opening a form and handling a click event.
open System.Windows.Forms

let form = new Form(Width= 400, Height = 300, Visible = true, Text = "Hello World")
form.TopMost <- true
form.Click.Add (fun args-> printfn "the form was clicked")
form.Show()
*)