// 31 Elevator
// Learning F#, Elevator simulation
//
// 2024-08-13   PV      First version, only 1 cabin

// "Main" module, Run the simulation
// In charge of master clock progression


System.Console.OutputEncoding <- System.Text.Encoding.UTF8
printfn "Elevator simulation in F#\n"


// Find next event in line, process it, and iterates until there are no more events to process
let runSimulation (b: DataBag) =
    let elevatorsActor = ElevatorsActor.createNew b
    let personsActor = PersonsActor.createNew b elevatorsActor
    elevatorsActor.Persons <- Some personsActor

    printfn ""

    let rec processNextEvent (clk: Clock) eventCount =
        let comingEvents =
            [ (elevatorsActor.getNextElevatorEventClock (), 0)
              (personsActor.getNextPersonEventClock (), 1) ]
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

            for (_, pri) in nextEvents do
                match pri with
                | 0 -> elevatorsActor.processEvent minClk
                | _ -> personsActor.processEvent minClk

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


// Array of specific persons list, no random generation in this case
let tenPersonsArrivingAtTimeZero = [|
    { Id = PersonId 1; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
    { Id = PersonId 2; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
    { Id = PersonId 3; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
    { Id = PersonId 4; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
    { Id = PersonId 5; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
    { Id = PersonId 6; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
    { Id = PersonId 7; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
    { Id = PersonId 8; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
    { Id = PersonId 9; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
    { Id = PersonId 10; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
|]

// Create DataBag
let b =
    { SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
      //SimulationPersons = SimulationPersonsArray tenPersonsArrivingAtTimeZero
      SimulationPersons = SimulationRandomGeneration(50, 600, 1, Ground50Levels50) }
      //SimulationPersons = SimulationRandomGeneration(4, 60, 1, FullRandom) }


printSimulationParameters b
let res = runSimulation b
PersonsActor.printPersonStats res.PersonsStats
ElevatorsActor.printElevatorStats res.ElevatorsStats
printSimulationStats res.SimulationStats

printfn "\nDone."
