// 31 Elevator
// Elevator simulation in F#
//
// 2014-08-15   PV
// 2014-08-17   PV      Use my own UniformUntRandom to get comparable results in other non .Net languages


[<AutoOpen>]
module PersonsModule

open System.Linq

type PersonsActor with
    static member createNew b elevators =
        { B = b
          TransportedPersons = new System.Collections.Generic.List<Person>()
          Elevators = elevators }

    member this.initialize() =
        let personsArray =
            match this.B.SimulationPersons with
            | SimulationPersonsArray pa -> pa
            | SimulationRandomGeneration(personsToCarry, arrivalLength, randomSeed, algorithm) ->
                let rndPersons = ParkMillerRandom.createNew randomSeed

                let rec getRandomPerson () =
                    let entry, exit =
                        match algorithm with
                        | Ground50Levels50 ->
                            if rndPersons.randInt 0 1 = 0 then
                                Floor.Zero, Floor(rndPersons.randInt 1 (this.B.SimulationElevators.Levels - 1))
                            else
                                Floor(rndPersons.randInt 1 (this.B.SimulationElevators.Levels - 1)), Floor.Zero

                        | FullRandom ->
                            Floor(rndPersons.randInt 0 (this.B.SimulationElevators.Levels - 1)),
                            Floor(rndPersons.randInt 0 (this.B.SimulationElevators.Levels - 1))

                    if entry = exit then
                        getRandomPerson ()
                    else
                        let arrival = Clock(rndPersons.randInt 0 (arrivalLength - 1))

                        { Id = PersonId 0
                          EntryFloor = entry
                          ExitFloor = exit
                          ArrivalClock = arrival
                          EntryClock = None
                          ExitClock = None }

                // First generate a random list
                let tempPersonsArray = [| for _ in 0 .. personsToCarry - 1 -> getRandomPerson () |]

                // Then sort by arrival time and assign Ids in arrival order
                Array.sortInPlaceBy (fun p -> p.ArrivalClock) tempPersonsArray

                [| for i in 0 .. personsToCarry - 1 ->
                       { tempPersonsArray[i] with
                           Id = PersonId(i + 1) } |]

        if this.B.LogDetails.ShowInitialPersons then
            printfn "\nPersons for the simulation"

            for p in personsArray do
                printfn "%s" (p.ToString()) // ToString for comparison with C#

        for p in personsArray do
            let evt =
                { PersonEvent.Clock = p.ArrivalClock
                  Person = p
                  Event = Arrival
                  CabinIndex = 0
                  CreatedOn = p.ArrivalClock }

            this.B.RegisterEvent(PersonEvent evt)

    member this.processEvent clk (evt: PersonEvent) =
        if this.B.LogDetails.ShowEvents then
            //let evtStr = sprintf "%0A" evt
            let evtStr = evt.ToString()
            Logging.logMessage this.B evt.Clock $"Person evt: {evtStr}" System.ConsoleColor.Blue

        assert (clk = evt.Clock)

        match evt.Event with
        | Arrival ->
            Logging.logPersonArrival this.B clk evt.Person
            this.B.AddJournalRecord(JournalPersonArrival(Clock = clk, Id = evt.Person.Id, EntryFloor = evt.Person.EntryFloor, ExitFloor = evt.Person.ExitFloor))

            let (Floor iFloor) = evt.Person.EntryFloor
            let originalLanding = this.Elevators.Landings[iFloor].deepCopy ()

            this.Elevators.Landings[iFloor] <-
                { this.Elevators.Landings[iFloor] with
                    Persons = evt.Person :: this.Elevators.Landings[iFloor].Persons }

            this.Elevators.callElevator clk evt.Person.EntryFloor evt.Person.ExitFloor

            let updatedLanding = this.Elevators.Landings[iFloor]
            Logging.logLandingUpdate this.B clk evt.Person.EntryFloor originalLanding updatedLanding

            if originalLanding.CallUp<>updatedLanding.CallUp then
                if updatedLanding.CallUp then
                    this.B.AddJournalRecord(JournalLandingSetCall(Clock = clk, Floor = evt.Person.EntryFloor, Direction = Up))
                else
                    this.B.AddJournalRecord(JournalLandingClearCall(Clock = clk, Floor = evt.Person.EntryFloor, Direction = Up))
            if originalLanding.CallDown<>updatedLanding.CallDown then
                if updatedLanding.CallDown then
                    this.B.AddJournalRecord(JournalLandingSetCall(Clock = clk, Floor = evt.Person.EntryFloor, Direction = Down))
                else
                    this.B.AddJournalRecord(JournalLandingClearCall(Clock = clk, Floor = evt.Person.EntryFloor, Direction = Down))

        | EndEnterCabin ->
            this.B.AddJournalRecord(JournalPersonCabinEnterEnd(Clock = clk, CabinIndex = 0, Id = evt.Person.Id))

        | ExitCabin ->
            this.TransportedPersons.Add(evt.Person)
            this.B.AddJournalRecord(JournalPersonCabinExitEnd(Clock = clk, CabinIndex = 0, Id = evt.Person.Id))
            Logging.logPersonExit this.B clk evt.Person


    member this.getTransportedPersons() = this.TransportedPersons


    static member PrintTransportedPersons(tp: Person array) =
        printfn "\nTransported persons"
        printfn "    Id    Entry    Exit   ArrTime   EntryT ExitTime    WaitEl TotTrans"
        printfn "  ----  ------- -------  -------- -------- --------  -------- --------"

        for p in tp.OrderBy(fun p -> p.Id) do
            let (PersonId pid) = p.Id
            let (Floor entryFloor) = p.EntryFloor
            let (Floor exitFloor) = p.ExitFloor
            let (Clock iArrival) = p.ArrivalClock
            let (Clock iEntry) = p.EntryClock.Value
            let (Clock iExit) = p.ExitClock.Value

            let waitForElevator = iEntry - iArrival
            let totalTransportTime = iExit - iArrival

            printfn
                $"  {pid, 4}  {entryFloor, 7} {exitFloor, 7}  {iArrival, 8} {iEntry, 8} {iExit, 8}  {waitForElevator, 8} {totalTransportTime, 8}"

    member this.getPersonsStats() =
        let ls = List.ofSeq this.TransportedPersons

        let median (l: double list) : double =
            let ss = l |> List.sort
            let len = List.length ss

            if len % 2 = 1 then
                ss[len >>> 1]
            else
                double (ss[(len >>> 1) - 1] + ss[len >>> 1]) / 2.0


        let avgWaitForElevator =
            if List.length ls = 0 then
                0.0
            else
                ls |> List.map (fun p -> double (p.waitForElevatorTime ())) |> List.average

        let medWaitForElevator =
            if List.length ls = 0 then
                0.0
            else
                ls |> List.map (fun p -> double (p.waitForElevatorTime ())) |> median

        let maxWaitForElevator =
            if List.length ls = 0 then
                0
            else
                ls |> List.map (fun p -> p.waitForElevatorTime ()) |> List.max

        let avgTotalTransport =
            if List.length ls = 0 then
                0.0
            else
                ls |> List.map (fun p -> double (p.totalTransportTime ())) |> List.average

        let medTotalTransport =
            if List.length ls = 0 then
                0.0
            else
                ls |> List.map (fun p -> double (p.totalTransportTime ())) |> median

        let maxTotalTransport =
            if List.length ls = 0 then
                0
            else
                ls |> List.map (fun p -> p.totalTransportTime ()) |> List.max


        // Return a PersonsStats record
        { AvgWaitForElevator = avgWaitForElevator
          Avg95WaitForElevator = -1.0
          MedWaitForElevator = medWaitForElevator
          MaxWaitForElevator = maxWaitForElevator
          Max95WaitForElevator = -1

          AvgTotalTransport = avgTotalTransport
          Avg95TotalTransport = -1.0
          MedTotalTransport = medTotalTransport
          MaxTotalTransport = maxTotalTransport
          Max95TotalTransport = -1 }

    static member printPersonStats ps =
        printfn "\nPerson stats"
        printfn "  Average wait for elevator: %.1f" ps.AvgWaitForElevator
        printfn "  Median wait for elevator:  %.1f" ps.MedWaitForElevator
        printfn "  Avg95 wait for elevator:   %.1f" ps.Avg95WaitForElevator
        printfn "  Max wait for elevator:     %d" ps.MaxWaitForElevator
        printfn "  Max95 wait for elevator:   %d" ps.Max95WaitForElevator

        printfn "  Average total transport:   %.1f" ps.AvgTotalTransport
        printfn "  Median total transport:    %.1f" ps.MedTotalTransport
        printfn "  Avg95 total transport:     %.1f" ps.Avg95TotalTransport
        printfn "  Max total transport:       %d" ps.MaxTotalTransport
        printfn "  Max95 total transport:     %d" ps.Max95TotalTransport
