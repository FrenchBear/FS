﻿// 13 Concurrency
// Learning F#, Exercises based on "Why using F#?", Part 2, Concurrency
//
// 2024-07-11   PV
// 2024-07-12   PV      Refactored to use HeetClient instead of WebRequest (deprecated)

// Traditional asynchronous programming
//
// This shows the use of AutoResetEvent as a synchronization mechanism.
// • A lambda is registered with the Timer.Elapsed event, and when the event is triggered, the AutoResetEvent is signalled.
// • The main thread starts the timer, does something else while waiting, and then blocks until the event is triggered.
// • Finally, the main thread continues, about 2 seconds later.
//
// F# can directly use all the usual .NET suspects, such as Thread AutoResetEvent, BackgroundWorker and IAsyncResult.
//
// Let’s see a simple example where we wait for a timer event to go off:

open System

let userTimerWithCallback () =
    // create an event to wait on
    let event = new System.Threading.AutoResetEvent(false)

    // create a timer and add an event handler that will signal the event
    let timer = new System.Timers.Timer(2000.0)
    timer.Elapsed.Add(fun _ -> event.Set() |> ignore)

    // start
    printfn "Waiting for timer at %O" DateTime.Now.TimeOfDay
    timer.Start()

    // keep working
    printfn "Doing something useful while waiting for event"

    // block on the timer via the AutoResetEvent
    event.WaitOne() |> ignore

    // done
    printfn "Timer ticked at %O\n" DateTime.Now.TimeOfDay

//userTimerWithCallback()


// ----------------------------------------------------------------------
// Asynchronous workflows
//
// F# has a built-in construct called “asynchronous workflows” which makes async code much easier to write. These
// workflows are objects that encapsulate a background task, and provide a number of useful operations to manage them.
//
// Here’s the previous example rewritten to use one.
// Here are the changes:
// • the AutoResetEvent and lambda have disappeared, and are replaced by let timerEvent = Control.Async.AwaitEvent
//   (timer.Elapsed), which creates an async object directly from the event, without needing a lambda. The ignore is added
//   to ignore the result.
// • the event.WaitOne() has been replaced by Async.RunSynchronously timerEvent which blocks on the async object until
//   it has completed.

open System
// open Microsoft.FSharp.Control  // Async.* is in this module.

let userTimerWithAsync () =
    // create a timer and associated async event
    let timer = new System.Timers.Timer(2000.0)
    let timerEvent = Async.AwaitEvent(timer.Elapsed) |> Async.Ignore

    // start
    printfn "Waiting for timer at %O" DateTime.Now.TimeOfDay
    timer.Start()

    // keep working
    printfn "Doing something useful while waiting for event"

    // block on the timer event now by waiting for the async to complete
    Async.RunSynchronously timerEvent

    // done
    printfn "Timer ticked at %O" DateTime.Now.TimeOfDay

// userTimerWithAsync()


// The async workflows can also be used with IAsyncResult, begin/end pairs, and other standard .NET methods.
//
// For example, here’s how you might do an async file write by wrapping the IAsyncResult generated from BeginWrite.

let fileWriteWithAsync () =
    // create a stream to write to
    use stream =
        new System.IO.FileStream(@"C:\Temp\test.txt", System.IO.FileMode.Create)
    // start
    printfn "Starting async write"
    let asyncResult = stream.BeginWrite(Array.empty, 0, 0, null, null)
    // create an async wrapper around an IAsyncResult
    let async = Async.AwaitIAsyncResult(asyncResult) |> Async.Ignore
    // keep working
    printfn "Doing something useful while waiting for write to complete"
    // block on the timer now by waiting for the async to complete
    Async.RunSynchronously async
    // done
    printfn "Async write completed"

// fileWriteWithAsync()



// ----------------------------------------------------------------------
// Creating and nesting asynchronous workflows

// Asynchronous workflows can also be created manually. A new workflow is created using the async keyword and curly
// braces. The braces contain a set of expressions to be executed in the background.
// This simple workflow just sleeps for 2 seconds.
// Note: the code do! Async.Sleep 2000 is similar to Thread.Sleep but designed to work with asynchronous workflows.

let sleepWorkflow =
    async {
        printfn "Starting sleep workflow at %O" DateTime.Now.TimeOfDay
        do! Async.Sleep 2000
        printfn "Finished sleep workflow at %O" DateTime.Now.TimeOfDay
    }

// Async.RunSynchronously sleepWorkflow


// Workflows can contain other async workflows nested inside them. Within the braces, the nested workflows can be
// blocked on by using the let! syntax.
let nestedWorkflow =
    async {
        printfn "Starting parent"
        let! childWorkflow = Async.StartChild sleepWorkflow
        // give the child a chance and then keep working
        do! Async.Sleep 100
        printfn "Doing something useful while waiting "
        // block on the child
        let! result = childWorkflow
        // done
        printfn "Finished parent"
    }

// run the whole workflow
// Async.RunSynchronously nestedWorkflow



// ----------------------------------------------------------------------
// Cancelling workflows
// in F# this technique is built in, using the CancellationToken class.

//Consider a simple task that prints numbers from 1 to 100:
let testLoop = async {
    for i in 1..100 do
        // do something
        printf "%i before.." i
        // sleep a bit
        do! Async.Sleep 10
        printfn "..after"
    }

// We can test it in the usual way:
// Async.RunSynchronously testLoop

// Now let’s say we want to cancel this task half way through. What would be the best way of doing it?
// Here an example of how we might cancel the task:

// open System
// open System.Threading
// // create a cancellation source
// let cancellationSource = new CancellationTokenSource()
// // start the task, but this time pass in a cancellation token
// Async.Start (testLoop,cancellationSource.Token)
// // wait a bit
// Thread.Sleep(200)
// // cancel after 200ms
// cancellationSource.Cancel()

// In F#, any nested async call will check the cancellation token automatically!
// In this case it was the line:
// do! Async.Sleep(10)



// ----------------------------------------------------------------------
// Composing workflows in series and parallel

// Another useful thing about async workflows is that they can be easily combined in various ways: both in series and in parallel.
// Let’s again create a simple workflow that just sleeps for a given time:

let sleepWorkflowMs ms = async {
    printfn "%i ms workflow started" ms
    do! Async.Sleep ms
    printfn "%i ms workflow finished" ms
    }

// Here’s a version that combines two of these in series:
let workflowInSeries = async {
    let! sleep1 = sleepWorkflowMs 1000
    printfn "Finished one"
    let! sleep2 = sleepWorkflowMs 2000
    printfn "Finished two"
    }

// printfn "\nRun in series"
// let sws = System.Diagnostics.Stopwatch.StartNew()
// Async.RunSynchronously workflowInSeries
// printfn "Elapsed serial: %A\n" sws.Elapsed


// And here’s a version that combines two of these in parallel:
// Create them
let sleep1 = sleepWorkflowMs 1000
let sleep2 = sleepWorkflowMs 2000

// run them in parallel
// let swp = System.Diagnostics.Stopwatch.StartNew()
// [sleep1; sleep2]
//     |> Async.Parallel
//     |> Async.RunSynchronously
//     |> ignore
// printfn "Elapsed parallel: %A\n" swp.Elapsed




// ----------------------------------------------------------------------
// Example: an async web downloader

// In this more realistic example, we’ll see how easy it is to convert some existing code from a non-asynchronous style
// to an asynchronous style, and the corresponding performance increase that can be achieved.
// So here is a simple URL downloader, very similar to the one we saw at the start of the series:

open System.Net
open System.Net.Http
open System
open System.IO
let fetchUrl url =
    //let req = WebRequest.Create(Uri(url))
    //use resp = req.GetResponse()
    //use stream = resp.GetResponseStream()
    //use reader = new IO.StreamReader(stream)
    //let html = reader.ReadToEnd()
    let cli = new HttpClient()
    let html = cli.GetStringAsync(Uri(url)).Result

    printfn "finished downloading %s" url

// And here is some code to time it:
// a list of sites to fetch
let sites = ["https://www.bing.com";
             "https://www.google.com";
             "https://www.microsoft.com";
             "https://www.amazon.com";
             "https://www.yahoo.com"]

// let sw1 = System.Diagnostics.Stopwatch.StartNew()
// sites                     // start with the list of sites
// |> List.map fetchUrl      // loop through each site and download
// |> ignore
// printfn "Elapsed simple sequential load: %A\n" sw1.Elapsed


// And now the concurrent F# version of the downloader code:
open Microsoft.FSharp.Control.CommonExtensions
// adds AsyncGetResponse

// Fetch the contents of a web page asynchronously
let fetchUrlAsync url =
    async {
        //let req = WebRequest.Create(Uri(url))
        //use! resp = req.AsyncGetResponse()  // new keyword "use!"
        //use stream = resp.GetResponseStream()
        //use reader = new IO.StreamReader(stream)
        //let html = reader.ReadToEnd()
        let cli = new HttpClient()
        let html = cli.GetStringAsync(Uri(url)).Result

        printfn "finished downloading %s" url
        }


// Note that the new code looks almost exactly the same as the original. There are only a few minor changes:
// • The change from “use resp = ” to “use! resp =” is exactly the change that we talked about above – while the async
//   operation is going on, let other tasks have a turn.
// • We also used the extension method AsyncGetResponse defined in the CommonExtensions namespace. This returns an async
//   workflow that we can nest inside the main workflow.
// • In addition the whole set of steps is contained in the “async {...}” wrapper which turns it into a block that can
//   be run asynchronously.

// The way this works is:
// • fetchUrlAsync is applied to each site. It does not immediately start the download, but returns an async workflow
//   for running later.
// • To set up all the tasks to run at the same time we use the Async.Parallel function
// • Finally we call Async.RunSynchronously to start all the tasks, and wait for them all to stop.


// let sw2 = System.Diagnostics.Stopwatch.StartNew()
// sites
// |> List.map fetchUrlAsync  // make a list of async tasks
// |> Async.Parallel          // set up the tasks to run in parallel
// |> Async.RunSynchronously  // start them off
// |> ignore
// printfn "Elapsed concurrent load: %A\n" sw2.Elapsed



// ----------------------------------------------------------------------
// Example: a parallel computation

// To finish up, let’s have another quick look at a parallel computation again.
// Before we start, I should warn you that the example code below is just to demonstrate the basic principles.
// Benchmarks from “toy” versions of parallelization like this are not meaningful, because any kind of real concurrent
// code has so many dependencies.
// And also be aware that parallelization is rarely the best way to speed up your code. Your time is almost always
// better spent on improving your algorithms. I’ll bet my serial version of quicksort against your parallel version of
// bubblesort any day! (For more details on how to improve performance, see the optimization series)
// Anyway, with that caveat, let’s create a little task that chews up some CPU. We’ll test this serially and in
// parallel.

let childTask() =
    // chew up some CPU.
    for i in 1..1000 do
        for i in 1..10000 do
            do "Hello".Contains("H") |> ignore
            // we don't care about the answer!
// Test the child task on its own.
// Adjust the upper bounds as needed to make this run in about 0.2 sec
let sw3 = System.Diagnostics.Stopwatch.StartNew()
childTask()
printfn "chiltTask alone: %A" sw3.Elapsed
// Adjust the upper bounds of the loops as needed to make this run in about 0.2 seconds.

// Now let’s combine a bunch of these into a single serial task (using composition), and test it with the timer:
let parentTask =
    childTask
    |> List.replicate 20
    |> List.reduce (>>)

// test
let sw4 = System.Diagnostics.Stopwatch.StartNew()
parentTask()
printfn "20×childTask: %A" sw3.Elapsed
//This should take about 4 seconds.


// Now in order to make the childTask parallelizable, we have to wrap it inside an async:
let asyncChildTask = async { return childTask() }

// And to combine a bunch of asyncs into a single parallel task, we use Async.Parallel.
// Let’s test this and compare the timings:
let asyncParentTask =
    asyncChildTask
    |> List.replicate 20
    |> Async.Parallel

// test
let sw5 = System.Diagnostics.Stopwatch.StartNew()
asyncParentTask
|> Async.RunSynchronously
|> ignore
printfn "20×childTask parallel: %A" sw3.Elapsed

// On a dual-core machine, the parallel version is about 50% faster. It will get faster in proportion to the number of
// cores or CPUs, of course, but sublinearly. Four cores will be faster than one core, but not four times faster.
// On the other hand, as with the async web download example, a few minor code changes can make a big difference, while
// still leaving the code easy to read and understand. So in cases where parallelism will genuinely help, it is nice to
// know that it is easy to arrange.

// Well, on my tests, the async // version is much, much slower...


