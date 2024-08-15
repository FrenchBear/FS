﻿// 31 Elevator
// Learning F#, Elevator simulation
//
// 2024-08-13   PV      First version, only 1 cabin

// ToDo: Global simulation stats (real duration, number of events, ...)
// ToDo: Final elevator stats: travel distance/time/accelerations, max persons transported, ...
// ToDo: Manage elevator capacity
// ToDo: On landings, last arrived person is 1st on the list, so 1st to enter in the elevator, that should not be the
//       case (pb is that 1st person entering may change cabin direction from NoDirection to what needs the 1st person)
// ToDo: Manage more than 1 elevator

// Runs the simulation
// In charge of master clock progression
// Find next event in line, process it, and iterates until there are no more events to process


System.Console.OutputEncoding <- System.Text.Encoding.UTF8
printfn "Elevator simulation in F#\n"

// Create DataBag
let b =
    { DataBag.levels = 6
      numberOfCabins = 1
      personsToCarry = 1
      arrivalLength = 60
      randomSeed = 1 }

printfn "Simulation parameters:"
printfn $"  {b.personsToCarry} persons to carry arriving over {b.arrivalLength} seconds"
printfn $"  {b.numberOfCabins} elevator, {b.levels} levels"

let elevators = Elevators.createNew b
let persons = Persons.createNew b elevators
elevators.Persons <- Some persons

printfn ""

let rec processNextEvent (clk: Clock) eventCount =
    let comingEvents =
        [ (persons.getNextPersonEventClock (), persons.processEvent)
          (elevators.getNextElevatorEventClock (), elevators.processEvent) ]
        |> List.filter (fun (optClk, _) -> optClk.IsSome)

    if comingEvents.IsEmpty then
        let (Clock iClk) = clk
        printfn "\nEnd simulation clk: %d" iClk
        persons.printFinalStats ()
        elevators.printFinalStats ()
        eventCount
    else
        let minClock = (fst (List.minBy (fun (optClk, _) -> optClk) comingEvents)).Value
        let nextEvents = comingEvents |> List.filter (fun (opt, _) -> opt = Some(minClock))

        for (_, processor) in nextEvents do
            processor minClock

        processNextEvent minClock (eventCount + List.length nextEvents)

let sw = System.Diagnostics.Stopwatch.StartNew()
let eventCount = processNextEvent (Clock 0) 0
sw.Stop()

printfn "\nSimulation stats" 
printfn $"  Real time duration:      {float (sw.ElapsedMilliseconds) / 1000.0:F3}s"
printfn $"  Events processed:        {eventCount}"
