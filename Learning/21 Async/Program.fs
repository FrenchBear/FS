// 21 Async
// Learning F#, Cheatsheet, Play with synchronous programming
//
// 2024-07-28   PV


// .Net tasks
// In F#, .NET Tasks can be constructed using the task { } computational expression. .NET Tasks are "hot" - they
// immediately start running. At the first let! or do!, the Task<'T> is returned and execution continues on the
// ThreadPool.

open System
open System.Threading
open System.Threading.Tasks
open System.IO

let readFile filename ct = task {
    printfn "Started Reading Task"
    // use do! when awaiting a Task
    do! Task.Delay((TimeSpan.FromSeconds 5), cancellationToken = ct)
    // use let! when awaiting a Task<'T>, and unwrap 'T from Task<'T>.
    let! text = File.ReadAllTextAsync(filename, ct)
    return text
}

let readFileTask: Task<string> = readFile "myfile.txt" CancellationToken.None
// (before return) Output: Started Reading Task

// (readFileTask continues execution on the ThreadPool)

// Blocks thread and waits for content. (1)
let fileContent = readFileTask.Result
// Task is already completed, returns same value immediately; no output
let fileContent' = readFileTask.Result



//Async Computations
// Async computations were invented before .NET Tasks existed, which is why F# has two core methods for asynchronous
// programming. However, async computations did not become obsolete. They offer another, but different, approach:
// dataflow. Async computations are constructed using async { } expressions, and the Async module is used to compose and
// execute them. In contrast to .NET Tasks, async expressions are "cold" (need to be explicitly started) and every
// execution propagates a CancellationToken implicitly.

open System
open System.Threading
open System.IO

let readFile2 filename = async {
    do! Async.Sleep(TimeSpan.FromSeconds 5)  // use do! when awaiting an Async
    let! text = File.ReadAllTextAsync(filename) |> Async.AwaitTask  // (1)
    printfn "Finished Reading File"
    return text
}

// compose a new async computation from exising async computations
let readFiles2 = [ readFile2 "A"; readFile2 "B" ] |> Async.Parallel

// execute async computation
let textOfFiles: string[] = readFiles2 |> Async.RunSynchronously
// Out: Finished Reading File
// Out: Finished Reading File

// re-execute async computation again
let textOfFiles': string[] = readFiles2 |> Async.RunSynchronously
// Out: Finished Reading File
// Out: Finished Reading File
