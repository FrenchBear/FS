// 31 Elevator
// Elevator simulation in F#
//
// "Main" simulation module, Run a simulation
// In charge of master clock progression
//
// 2014-08-15   PV


[<AutoOpen>]
module Simulation

// Find next event in line, process it, and iterates until there are no more events to process
let runSimulation (b: DataBag) =
    let elevatorsActor = ElevatorsActor.createNew b
    let personsActor = PersonsActor.createNew b elevatorsActor
    elevatorsActor.Persons <- Some personsActor

    if showLog then
        printfn ""

    let rec processNextEvent (clk: Clock) eventCount =
        // Special case, first elevator event ElevatorOn (check initialization and record stating values in statistics) is always processed first
        if eventCount = 0 then
            elevatorsActor.processEvent (Clock 0)
            processNextEvent clk 1
        else
            let comingEvents =
                [ (personsActor.getNextPersonEventClock (), 0) // Person arrivals are processed before elevator events of the same clock
                  (elevatorsActor.getNextElevatorEventClock (), 1) ]
                |> List.filter (fun (optClk, _) -> optClk.IsSome)

            if comingEvents.IsEmpty then
                let (Clock iClk) = clk

                if showLog then
                    printfn "\nEnd simulation clk: %d\n" iClk

                if showDetailedPersonStats then
                    personsActor.printDetailedPersonStats ()

                let ps = personsActor.getPersonStats ()
                let es = elevatorsActor.getElevatorsStats ()
                clk, eventCount, ps, es
            else
                let minClk = (fst (List.minBy (fun (optClk, _) -> optClk) comingEvents)).Value

                let nextEvents =
                    comingEvents
                    |> List.filter (fun (opt, _) -> opt = Some(minClk))
                    |> List.sortBy (fun (_, pri) -> pri)

                for (_, priority) in nextEvents do
                    match priority with
                    | 0 -> personsActor.processEvent minClk
                    | _ -> elevatorsActor.processEvent minClk

                processNextEvent minClk (eventCount + List.length nextEvents)

    let sw = System.Diagnostics.Stopwatch.StartNew()
    let (Clock iClk), eventCount, ps, es = processNextEvent (Clock 0) 0
    sw.Stop()

    let ss =
        { SimulationDuration = iClk
          SimulationRealTimeDuration = float (sw.ElapsedMilliseconds) / 1000.0
          SimulationEventsCount = eventCount }

    // Returns a SimulationResult
    { SimulationStats = ss
      ElevatorsStats = es
      PersonsStats = ps }

let printSimulationParameters b =
    printfn "Simulation parameters"
    printfn "  Persons"

    match b.SimulationPersons with
    | SimulationRandomGeneration(personsToCarry, arrivalLength, randomSeed, algorithm) ->
        printfn "    Random persons to carry: %d, Algorithm: %A, Seed: %d" personsToCarry algorithm randomSeed
        printfn "    Arrival duration:        %d" arrivalLength
    | SimulationPersonsArray ap -> printfn "    Fixed persons to carry:  %d" (Array.length ap)

    printfn "  Elevators/Building"
    printfn "    Levels:                  %d" b.SimulationElevators.Levels
    printfn "    Number of cabins:        %d" b.SimulationElevators.NumberOfCabins
    printfn "    Capacity of a cabin:     %d" b.SimulationElevators.Capacity

let printSimulationStats ss =
    printfn "\nSimulation stats"
    printfn $"  Simulation duration:       {ss.SimulationDuration}"
    printfn $"  Real time duration:        {ss.SimulationRealTimeDuration:F3}s"
    printfn $"  Events processed:          {ss.SimulationEventsCount}"
