// 13 Why 2
// Learning F#, Exercises based on "Why using F#?", Part 2
//
// 2024-07-12   PV

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
    use stream = new System.IO.FileStream(@"C:\Temp\test.txt",System.IO.FileMode.Create)
    // start
    printfn "Starting async write"
    let asyncResult = stream.BeginWrite(Array.empty,0,0,null,null)
    // create an async wrapper around an IAsyncResult
    let async = Async.AwaitIAsyncResult(asyncResult) |> Async.Ignore
    // keep working
    printfn "Doing something useful while waiting for write to complete"
    // block on the timer now by waiting for the async to complete
    Async.RunSynchronously async
    // done
    printfn "Async write completed"

fileWriteWithAsync()
