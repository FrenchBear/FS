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
                                Floor 0, Floor(rndPersons.randInt 1 (this.B.SimulationElevators.Levels - 1))
                            else
                                Floor(rndPersons.randInt 1 (this.B.SimulationElevators.Levels - 1)), Floor 0

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
                printfn "%0A" p

        for p in personsArray do
            let evt =
                { PersonEvent.Clock = p.ArrivalClock
                  Person = p
                  Event = Arrival
                  CreatedOn = p.ArrivalClock }

            this.B.EventsQueue.Enqueue(PersonEvent evt, evt.Clock)

    member this.processEvent clk (evt: PersonEvent) =
        if this.B.LogDetails.ShowEvents then
            Logging.logMessage this.B evt.Clock $"Person evt: {evt}"

        assert (clk = evt.Clock)

        match evt.Event with
        | Arrival ->
            Logging.logPersonArrival this.B clk evt.Person
            let prevList = this.Elevators.Landings.getPersons evt.Person.EntryFloor
            this.Elevators.Landings.setPersons evt.Person.EntryFloor (evt.Person :: prevList)
            this.Elevators.callElevator clk evt.Person.EntryFloor evt.Person.ExitFloor

        | ExitCabin ->
            this.TransportedPersons.Add(evt.Person)
            Logging.logPersonExit this.B clk evt.Person


    member this.getTransportedPersons() = this.TransportedPersons


    member this.printDetailedPersonStats() =
        printfn "Detailed Person stats"
        printfn "    Id    Entry    Exit   ArrTime   EntryT ExitTime    WaitEl TotTrans"
        printfn "  ----  ------- -------  -------- -------- --------  -------- --------"

        for p in this.TransportedPersons.OrderBy(fun p -> p.ArrivalClock) do
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

        printfn ""

    member this.getPersonStats() =
        let ls = List.ofSeq this.TransportedPersons

        let avgWaitForElevator =
            if List.length ls = 0 then
                0.0
            else
                ls |> List.map (fun p -> double (p.waitForElevatorTime ())) |> List.average

        let avgTotalTransport =
            if List.length ls = 0 then
                0.0
            else
                ls |> List.map (fun p -> double (p.totalTransportationTime ())) |> List.average

        let maxWaitForElevator =
            if List.length ls = 0 then
                0
            else
                ls |> List.map (fun p -> p.waitForElevatorTime ()) |> List.max

        let maxTotalTransport =
            if List.length ls = 0 then
                0
            else
                ls |> List.map (fun p -> p.totalTransportationTime ()) |> List.max

        // Return a PersonsStats record
        { AvgWaitForElevator = avgWaitForElevator
          AvgTotalTransport = avgTotalTransport
          MaxWaitForElevator = maxWaitForElevator
          MaxTotalTransport = maxTotalTransport }

    static member printPersonStats ps =
        printfn "\nPerson stats"
        printfn "  Average wait for elevator: %.1f" ps.AvgWaitForElevator
        printfn "  Average total transport:   %.1f" ps.AvgTotalTransport
        printfn "  Max wait for elevator:     %d" ps.MaxWaitForElevator
        printfn "  Max total transportation:  %d" ps.MaxTotalTransport
