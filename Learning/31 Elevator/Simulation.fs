// 31 Elevator
// Elevator simulation in F#
//
// "Main" simulation module, Run a simulation
// In charge of master clock progression
//
// 2014-08-15   PV
// 2014-08-28   PV      Simplification with ClockPriority


[<AutoOpen>]
module Simulation

let getSimulationData (b: DataBag) =
    let sd:SimulationData =
        {
                Levels = b.SimulationElevators.Levels
                NumberOfCabins = 1
                Capacity = b.SimulationElevators.Capacity

                FixedPersonsList = match b.SimulationPersons with
                                   | SimulationPersonsArray _ -> true
                                   | _ -> false
                PersonsToCarry = b.SimulationPersons.getPersonsToCarry
                ArrivalLength = match b.SimulationPersons with
                                | SimulationRandomGeneration(personsToCarry, arrivalLength, randomSeed, algorithm) -> Some arrivalLength
                                | _ -> None
                Algorithm = match b.SimulationPersons with
                            | SimulationRandomGeneration(personsToCarry, arrivalLength, randomSeed, algorithm) -> Some algorithm
                            | _ -> None
                RandomSeed = match b.SimulationPersons with
                             | SimulationRandomGeneration(personsToCarry, arrivalLength, randomSeed, algorithm) -> Some randomSeed
                             | _ -> None
                AccelerationDuration = b.Durations.AccelerationDuration
                OneLevelFullSpeed = b.Durations.OneLevelFullSpeed
                FullSpeedBeforeDecisionDuration = b.Durations.FullSpeedBeforeDecisionDuration
                OpeningDoorsDuration = b.Durations.OpeningDoorsDuration
                MoveInDuration = b.Durations.MoveInDuration
                MotorDelayDuration = b.Durations.MotorDelayDuration
        }
    sd

// Find next event in line, process it, and iterates until there are no more events to process
let runSimulation (b: DataBag) =
    let elevatorsActor = ElevatorsActor.createNew b
    let personsActor = PersonsActor.createNew b elevatorsActor
    elevatorsActor.Persons <- Some personsActor // because of mutual cross-reference between elevatorsActor and personsActor

    if b.LogDetails.ShowLog then
        printfn ""

    let rec processNextEvent (clk: Clock) eventCount =
        let hasItem, _, nextClkPri = b.EventsQueue.TryPeek()

        //if clk > Clock 1590 then
        //    System.Diagnostics.Debugger.Break()

        if not hasItem then
            let (Clock iClk) = clk

            if b.LogDetails.ShowLog then
                printfn "\nEnd simulation clk: %d\n" iClk

            let sd = getSimulationData b
            let ps = personsActor.getPersonsStats ()
            let es = elevatorsActor.getElevatorsStats ()
            let tp = personsActor.getTransportedPersons ()
            clk, eventCount, sd, ps, es, tp

        else
            let nextEvent = b.EventsQueue.Dequeue()
            match nextEvent with
            | PersonEvent pe ->
                assert (pe.Clock = nextClkPri.Clock)
                personsActor.processEvent nextClkPri.Clock pe
            | ElevatorEvent ee ->
                assert (ee.Clock = nextClkPri.Clock)
                elevatorsActor.processEvent nextClkPri.Clock ee

            processNextEvent nextClkPri.Clock (eventCount + 1)

    let sw = System.Diagnostics.Stopwatch.StartNew()
    elevatorsActor.initialize ()
    personsActor.initialize ()
    let (Clock iClk), eventCount, sd, ps, es, tp = processNextEvent (Clock.Zero) 0
    sw.Stop()

    let ss =
        { SimulationDuration = iClk
          SimulationRealTimeDuration = float (sw.ElapsedMilliseconds) / 1000.0
          SimulationEventsCount = eventCount }

    // Returns a SimulationResult
    { 
      SimulationData = sd
      SimulationStats = ss
      ElevatorsStats = es
      PersonsStats = ps
      TransportedPersons = tp
    }


let PrintSimulationData (sd:SimulationData) =
    printfn "\nSimulation data"
    printfn "  Persons"

    if sd.FixedPersonsList then
        printfn "    Fixed persons to carry:  %d" sd.PersonsToCarry
    else
        printfn "    Random persons to carry: %d, Algorithm: %A, Random seed: %d" sd.PersonsToCarry sd.Algorithm.Value sd.RandomSeed.Value
        printfn "    Arrival length:          %d" sd.ArrivalLength.Value

    printfn "  Elevators/Building"
    printfn "    Levels:                  %d" sd.Levels
    printfn "    Number of cabins:        %d" sd.NumberOfCabins
    printfn "    Capacity of a cabin:     %d" sd.Capacity

let printSimulationStats ss =
    printfn "\nSimulation stats"
    printfn $"  Simulation duration:       {ss.SimulationDuration}"
    printfn $"  Real time duration:        {ss.SimulationRealTimeDuration:F3}s"
    printfn $"  Events processed:          {ss.SimulationEventsCount}"
