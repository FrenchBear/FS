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

    if b.LogDetails.showLog then
        printfn ""

    let rec processNextEvent (clk: Clock) eventCount =
        let hasItem,_,nextClk = b.EventsQueue.TryPeek()

        if not hasItem
        then
            let (Clock iClk) = clk

            if b.LogDetails.showLog then
                printfn "\nEnd simulation clk: %d\n" iClk

            if b.LogDetails.showDetailedPersonStats then
                personsActor.printDetailedPersonStats ()

            let ps = personsActor.getPersonStats ()
            let es = elevatorsActor.getElevatorsStats ()
            let tp = personsActor.getTransportedPersons ()
            clk, eventCount, ps, es, tp

        else
            let rec getComingEventsList lst =
                let hasItem,_,clkPeek = b.EventsQueue.TryPeek()
                if (not hasItem) || clkPeek>nextClk
                then lst
                else
                    let nextEvent = b.EventsQueue.Dequeue()
                    let nep =
                        match nextEvent with
                        | PersonEvent pe -> nextEvent, 0
                        | ElevatorEvent ee -> nextEvent, 1
                    getComingEventsList (nep::lst)
                        
            let nextEvents = (getComingEventsList []) |> List.sortBy (fun (_, pri) -> pri)

            for (evt, _) in nextEvents do
                match evt with
                | PersonEvent pe -> 
                    assert (pe.Clock=nextClk)
                    personsActor.processEvent nextClk pe
                | ElevatorEvent ee -> 
                    assert (ee.Clock=nextClk)
                    elevatorsActor.processEvent nextClk ee

            processNextEvent nextClk (eventCount + List.length nextEvents)

    let sw = System.Diagnostics.Stopwatch.StartNew()
    elevatorsActor.initialize ()
    let (Clock iClk), eventCount, ps, es, tp = processNextEvent (Clock 0) 0
    sw.Stop()

    let ss =
        { SimulationDuration = iClk
          SimulationRealTimeDuration = float (sw.ElapsedMilliseconds) / 1000.0
          SimulationEventsCount = eventCount }

    // Returns a SimulationResult
    { SimulationStats = ss
      ElevatorsStats = es
      PersonsStats = ps
      TransportedPersons = tp }

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
