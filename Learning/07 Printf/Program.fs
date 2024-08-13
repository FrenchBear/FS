// 07 Printf
// Learning F#, Play with printf
//
// 2024-06-24   PV

open System
open System.IO


// Composite formatting technique is available in all .NET languages, but rarely used in F#
Console.WriteLine("A string: {0}. An int: {1}. A float: {2}. A bool: {3}","hello",42,3.14,true)

// The printf technique, on the other hand, is based on the C-style format strings (statically type checked, supports partial application
printfn "A string: %s. An int: %i. A float: %f. A bool: %b" "hello" 42 3.14 true

// partial application - explicit parameters
let printStringAndInt s i =  printfn "A string: %s. An int: %i" s i
let printHelloAndInt i = printStringAndInt "Hello" i
printHelloAndInt 42

// partial application - point free style
let printInt =  printfn "An int: %i"
printInt 42


// printf can be used for function parameters anywhere a standard function can be used.
let doSomething printerFn x y =
    let result = x + y
    printerFn "result is" result

let callback = printfn "%s %i"
doSomething callback 3 4

// This also includes the higher order functions for lists, etc.:
[1..5] |> List.map (printf "i=%i ") |> ignore
printfn "\n"


// printf supports native F# types
// For non-primitive types, the .NET formatting functions only support using ToString(), but printf supports native F#
// types using the %A specifier:

// tuple printing
let t = (1,2)
Console.WriteLine("A tuple: {0}", t)
printfn "A tuple: %A" t

// record printing
type Person = {First:string; Last:string}
let johnDoe = {First="John"; Last="Doe"}
Console.WriteLine("A record: {0}", johnDoe )
printfn "A record: %A" johnDoe

// union types printing
type Temperature = F of int | C of int
let freezing = F 32
Console.WriteLine("A union: {0}", freezing )
printfn "A union: %A" freezing


// Beware, printf "format strings" are not strings!
// This works:
let netFormatString = "A string: {0}"
Console.WriteLine(netFormatString, "hello")
// But NOT this:
//let fsharpFormatString = "A string: %s"
//printfn fsharpFormatString  "Hello"

// For simple "inline" cases, use Print.TextWriterFormat
let format:Printf.TextWriterFormat<_> = "A string: %s"
printfn format "Hello"

// For really dynamic formats, use constructor with a type
let formatAString = "A string: %s"
let twFormat1  = Printf.TextWriterFormat<string->unit>(formatAString)
printfn twFormat1 "Hello"

let formatAStringAndInt = "A string: %s. An int: %i"
let twFormat2  = Printf.TextWriterFormat<string->int->unit>(formatAStringAndInt)
printfn twFormat2  "Hello" 42


// Format specifiers (use %% to represent % char)
// %[flags][width][.precision]specifier
// %s for strings, %b for bools, %i for ints, %f for floats, %A for pretty-printing tuples, records and union types,
// %O for other objects, using ToString()
// For floats: %f standard fmt, %e or %E for exponential format, %g or %G for most compact of f and e, %M for decimals

let rows = [ (1,"a"); (-22,"bb"); (333,"ccc"); (-4444,"dddd") ]
// no alignment
for (i,s) in rows do
    printfn "|%i|%s|" i s
// with alignment
for (i,s) in rows do
    printfn "|%5i|%5s|" i s
// with left alignment for column 2
for (i,s) in rows do
    printfn "|%5i|%-5s|" i s
// with dynamic column width=20 for column 1
for (i,s) in rows do
    printfn "|%*i|%-5s|" 20 i s
// with dynamic column width for column 1 and column 2
for (i,s) in rows do
    printfn "|%*i|%-*s|" 20 i 10 s

// Floats
let pi = 3.14
printfn "float: %f exponent: %e compact: %g" pi pi pi
let petabyte = pown 2.0 50
printfn "float: %f exponent: %e compact: %g" petabyte petabyte petabyte


printfn "2 digits precision: %.2f. 4 digits precision: %.4f." 123.456789 123.456789
// output => 2 digits precision: 123.46. 4 digits precision: 123.4568.
printfn "2 digits precision: %.2e. 4 digits precision: %.4e." 123.456789 123.456789
// output => 2 digits precision: 1.23e+002. 4 digits precision: 1.2346e+002.
printfn "2 digits precision: %.2g. 4 digits precision: %.4g." 123.456789 123.456789
// output => 2 digits precision: 1.2e+02. 4 digits precision: 123.5.

printfn "|%f|" pi     // normal
printfn "|%10f|" pi   // width
printfn "|%010f|" pi  // zero-pad
printfn "|%-10f|" pi  // left aligned
printfn "|%0-10f|" pi // left zero-pad


//Custom formatting functions
// There are two special format specifiers that allow to you pass in a function rather than just a simple value.
// %t expects a function that outputs some text with no input
// %a expects a function that outputs some text from a given input

let printHello (tw:TextWriter) = tw.Write("hello")
printfn "custom function: %t" printHello

//define the function using a closure
let printRand =
    let rand = new Random()
    // return the actual printing function
    fun (tw:TextWriter) -> tw.Write(rand.Next(1,100))
for i in 1..5 do
    printfn "rand = %t" printRand

// Here’s an example of custom formatting a tuple:

//define the callback function
//note that the data parameter comes after the TextWriter
let printLatLong (tw:TextWriter) (lat,long) =
    tw.Write("lat:{0} long:{1}", lat, long)
let latLongs = [ (1,2); (3,4); (5,6)]
for latLong  in latLongs  do
    // function and value both passed in to printfn
    printfn "latLong = %a" printLatLong latLong


// Date formatting
// There are no special format specifiers for dates in F#.
// If you want to format dates, you have a couple of options:
// - Use ToString to convert the date into a string, and then use the %s specifier
// - Use a custom callback function with the %a specifier as described above

// function to format a date
let yymmdd1 (date:DateTime) = date.ToString("yy.MM.dd")
// function to format a date onto a TextWriter
let yymmdd2 (tw:TextWriter) (date:DateTime) = tw.Write("{0:yy.MM.dd}", date)

for i in 1..5 do
    let date = DateTime.Now.AddDays(float i)
    // using %s
    printfn "using ToString = %s" (yymmdd1 date)
    // using %a
    printfn "using a callback = %a" yymmdd2 date

// F# function              C# equivalent	                                Comment
// printf and printfn	    Console.Write and Console.WriteLine             Functions starting with “print” write to standard out.
// eprintf and eprintfn	    Console.Error.Write and Console.Error.WriteLine	Functions starting with “eprint” write to standard error.
// fprintf and fprintfn	    TextWriter.Write and TextWriter.WriteLine	    Functions starting with “fprint” write to a TextWriter.
// sprintf                  String.Format	                                Functions starting with “sprint” return a string.
// bprintf	                StringBuilder.AppendFormat	                    Functions starting with “bprint” write to a StringBuilder.
// kprintf,  kfprintf, ksprintf and kbprintf	No equivalent	            Functions that accept a continuation.


// A particularly useful technique is to use partial application to “bake in” a TextWriter or StringBuilder.
// Here is an example using a StringBuilder:
let printToSb s i =
    let sb = new System.Text.StringBuilder()
    // use partial application to fix the StringBuilder
    let myPrint format = Printf.bprintf sb format
    do myPrint "A string: %s. " s
    do myPrint "An int: %i" i
    sb.ToString()     //get the result
printToSb "hello" 42 |> printfn "%s"

// And here is an example using a TextWriter:
let printToFile filename s i =
    let myDocsPath = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
    let fullPath = Path.Combine(myDocsPath, filename)
    use sw = new StreamWriter(path=fullPath)
    // use partial application to fix the TextWriter
    let myPrint format = fprintf sw format
    do myPrint "A string: %s. " s
    do myPrint "An int: %i" i
    sw.Close()     //get the result
printToFile "C:\\Temp\\myfile.txt" "hello" 42


// The four kXXX functions are similar to their cousins, except that they take an extra parameter – a continuation. That
// is, a function to be called immediately after the formatting has been done.

let doAfter s =
    printfn "Done <%s>" s
    s   // return the result

let result = Printf.ksprintf doAfter "%s" "Hello"

// Example of a logging library such as log4net or System.Diagnostics.Trace
type Logger(name) =
    let currentTime (tw:TextWriter) = tw.Write("{0:s}",DateTime.Now)
    let logEvent level msg = printfn "%t %s [%s] %s" currentTime level name msg
    member this.LogInfo msg = logEvent "INFO" msg
    member this.LogError msg = logEvent "ERROR" msg
    static member CreateLogger name = new Logger(name)

// Next in my application code, I do the following:
// - Create an instance of the logging framework. I’ve hard-coded the factory method here, but you could also use an IoC
//   container.
// - Create helper functions called logInfo and logError that call the logging framework, and in the case of logError,
//   show a popup message as well.

module MyApplication =
    let logger = Logger.CreateLogger("MyApp")
    // create a logInfo using the Logger class
    let logInfo format =
        let doAfter s = logger.LogInfo(s)
        Printf.ksprintf doAfter format
    
    // create a logError using the Logger class
    let logError format =
        let doAfter s = 
            logger.LogError(s)
            //System.Windows.Forms.MessageBox.Show(s) |> ignore
        Printf.ksprintf doAfter format
    
    // function to exercise the logging
    let test() =
        do logInfo "Message #%i" 1
        do logInfo "Message #%i" 2
        do logError "Oops! an error occurred in my app"

// when we run the test function, we should get the message written to the console, and also see the popup message:
MyApplication.test()
