// 15 Events
// Learning F#, Exercises based on "Why using F#?", Part 4, Functional Reactive Programming
//
// 2024-07-12   PV



// A simple event stream
// Let’s start with a simple example to compare the two approaches. We’ll implement the classic event handler approach first.
// First, we define a utility function that will:
// • create a timer
// • register a handler for the Elapsed event
// • run the timer for five seconds and then stop it

open System
open System.Threading

// create a timer and register an event handler, then run the timer for 3.5 seconds
let createTimer timerInterval eventHandler =
    // setup a timer
    let timer = new System.Timers.Timer(float timerInterval)
    timer.AutoReset <- true
    timer.Elapsed.Add eventHandler // add an event handler

    // return an async task
    async {
        timer.Start()
        do! Async.Sleep 3500 // run for 3.5s
        timer.Stop()
    }

// create a handler. The event args are ignored
let basicHandler _ = printfn "tick %A" DateTime.Now

// register the handler
let basicTimer1 = createTimer 1000 basicHandler

// run the task now
Async.RunSynchronously basicTimer1
printfn ""



// Now let’s create a similar utility method to create a timer, but this time it will return an “observable” as well,
// which is the stream of events.

let createTimerAndObservable timerInterval =
    // setup a timer
    let timer = new System.Timers.Timer(float timerInterval)
    timer.AutoReset <- true
    // events are automatically IObservable
    let observable = timer.Elapsed
    // return an async task
    let task =
        async {
            timer.Start()
            do! Async.Sleep 3500
            timer.Stop()
        }
    // return a async task and the observable
    (task, observable)

// And again test it interactively
// create the timer and the corresponding observable
let basicTimer2, timerEventStream = createTimerAndObservable 1000

// register that every time something happens on the event stream, print the time.
timerEventStream
|> Observable.subscribe (fun _ -> printfn "tick %A" DateTime.Now)
|> ignore

// run the task now
Async.RunSynchronously basicTimer2

// The difference is that instead of registering a handler directly with an event, we are “subscribing” to an event
// stream. Subtly different, and important.
printfn ""



// -------------------------------------------------------------------------------
// Counting events

// In this next example, we’ll have a slightly more complex requirement:
// - Create a timer that ticks every 500ms.
// - At each tick, print the number of ticks so far and the current time.

// To do this in a classic imperative way, we would probably create a class with a mutable counter, as below:
type ImperativeTimerCount() =
    let mutable count = 0
    // the event handler. The event args are ignored
    member this.handleEvent _ =
        count <- count + 1
        printfn "timer ticked with count %i" count

// We can reuse the utility functions we created earlier to test it:
// create a handler class
let handler = new ImperativeTimerCount()

// register the handler method
let timerCount1 = createTimer 500 handler.handleEvent

// run the task now
Async.RunSynchronously timerCount1
printfn ""


// Let’s see how we would do this same thing in a functional way:
// create the timer and the corresponding observable
let timerCount2, timerEventStream2 = createTimerAndObservable 500

// set up the transformations on the event stream
timerEventStream2
|> Observable.scan (fun count _ -> count + 1) 0
|> Observable.subscribe (fun count -> printfn "timer ticked with count %i" count)
|> ignore

// run the task now
Async.RunSynchronously timerCount2

// Here we see how you can build up layers of event transformations, just as you do with list transformations in LINQ.
// The first transformation is scan, which accumulates state for each event. It is roughly equivalent to the List.fold
// function that we have seen used with lists. In this case, the accumulated state is just a counter. And then, for each
// event, the count is printed out. Note that in this functional approach, we didn’t have any mutable state, and we
// didn’t need to create any special classes.
