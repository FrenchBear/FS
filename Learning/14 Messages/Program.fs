// 14 Messages
// Learning F#, Exercises based on "Why using F#?", Part 3, Messages and Agents
//
// 2024-07-12   PV


let pause duration =
    let sleepWorkflow = async { do! Async.Sleep (millisecondsDueTime=duration) }    // Use parameter name resolve overloads
    Async.RunSynchronously sleepWorkflow

// One-liner version
// let pause (ms:int) = async { do! Async.Sleep ms} |> Async.RunSynchronously


// A simple example

let simpleMailboxExample () =

    let printerAgent =
        MailboxProcessor.Start(fun inbox ->

            // the message processing function
            let rec messageLoop () =
                async {

                    // read a message
                    let! msg = inbox.Receive()

                    // process a message
                    printfn "message is: %s" msg

                    // loop to top
                    return! messageLoop ()
                }

            // start the loop
            messageLoop ())

    // The MailboxProcessor.Start function takes a simple function parameter. That function loops forever, reading messages
    // from the queue (or “inbox”) and processing them.

    printerAgent.Post "hello"
    printerAgent.Post "hello again"
    printerAgent.Post "hello a third time"

    printfn "We continue execution"
    pause 200    // Sleep 200ms to let the messaging system work in the background
    printfn "simpleMailboxExample done\n"

simpleMailboxExample ()



// Message-based shared state

open System
open System.Threading

// a utility function
type Utility() =
    static let rand = Random()

    static member RandomSleep() =
        let ms = rand.Next(1, 10)
        Thread.Sleep ms

type MessageBasedCounter() =

    static let updateState (count, sum) msg =
        let newSum = sum + msg
        let newCount = count + 1
        printfn "Count is: %i. Sum is: %i" newCount newSum
        // ...emulate a short delay
        Utility.RandomSleep()
        // return the new state
        (newCount, newSum)

    // create the agent
    static let agent =
        MailboxProcessor.Start(fun inbox ->
            // the message processing function
            let rec messageLoop oldState =
                async {
                    let! msg = inbox.Receive() // read a message
                    let newState = updateState oldState msg // do the core logic
                    return! messageLoop newState // loop to top
                }
            // start the loop
            messageLoop (0, 0))

    // public interface to hide the implementation
    // Do not add call updateState directly, but send a message
    // Messages are queued (serialized), executed one at a time, so there's no race issues in case of
    // parallel execution, and no need to use lock
    static member Add i = agent.Post i


let test() =
    // Create a task that will try to access the counter:
    let makeCountingTask addFunction = async {
        for i in 1..3 do
            addFunction i
        }

    let task = makeCountingTask MessageBasedCounter.Add

    // Finally let’s create 5 child tasks that try to access the counter at once.
    printfn "\n5 child tasks accessing the counter at once"
    let messageExample5 =
        [1..5]
            |> List.map (fun i -> makeCountingTask MessageBasedCounter.Add)
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore

    pause 500     // Sleep 500ms to let the messaging system work in the background
    printfn "test done.\n"

test()


// --------------------------------------------------------------
// Shared IO

// IO without serialization

// In order to make the corruption very obvious and repeatable, let’s first create a “slow” console that writes each
// individual character in the log message and pauses for a millisecond between each character. During that millisecond,
// another thread could be writing as well, causing an undesirable interleaving of messages.
let slowConsoleWrite msg =
    msg |> String.iter (fun ch->
        System.Threading.Thread.Sleep(1)
        System.Console.Write ch
        )

// test in isolation
printfn "Test slowConsoleWrite in isolation"
slowConsoleWrite "abc\n"

// Next, we will create a simple task that loops a few times, writing its name each time to the logger:
let makeTask logger taskId = async {
    let name = sprintf "Task%i" taskId
    for i in 1..3 do
        let msg = sprintf "<%s:Loop%i>\n" name i
        logger msg
    }

// test in isolation
printfn "\nTest slowConsoleWrite in loop, 3 times"
let task = makeTask slowConsoleWrite 1
Async.RunSynchronously task
pause 100
printfn ""

// Next, we write a logging class that encapsulates access to the slow console. It has no locking or serialization, and
// is basically not thread-safe:

type UnserializedLogger() =
    // interface
    member this.Log msg = slowConsoleWrite msg

// test in isolation
printfn "Test UnserializedLogger in isolation"
let unserializedLogger = UnserializedLogger()
unserializedLogger.Log "hello\n"
pause 100

// Now let’s combine all these into a real example. We will create five child tasks and run them in parallel, all trying
// to write to the slow console.
printfn "Test UnserializedLogger, 5 child tasks running in //"

let unserializedExample =
    let logger = UnserializedLogger()
    [1..5]
        |> List.map (fun i -> makeTask logger.Log i)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

printfn ""
pause 500
// Output is gargled...


// Now, a serialized logger

type SerializedLogger() =
    // create the mailbox processor
    let agent = MailboxProcessor.Start(fun inbox ->
        // the message processing function
        let rec messageLoop () = async{
            // read a message
            let! msg = inbox.Receive()
            // write it to the log
            slowConsoleWrite msg
            // loop to top
            return! messageLoop ()
            }
        // start the loop
        messageLoop ()
        )
    // public interface
    member this.Log msg = agent.Post msg

// test in isolation
printfn "Test SerializedLogger in isolation"
let serializedLogger = SerializedLogger()
serializedLogger.Log "hello\n"
pause 200

// So now we can repeat the earlier unserialized example but using the SerializedLogger instead. Again, we create five
// child tasks and run them in parallel:
printfn "\nTest SerializedLogger, 5 child tasks running in //"

let serializedExample =
    let logger = SerializedLogger()
    [1..5]
        |> List.map (fun i -> makeTask logger.Log i)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
pause 4000
printfn "test done."
