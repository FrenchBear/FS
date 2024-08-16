// 31 Elevator
// Elevator simulation in F#
//
// 2014-08-15   PV


[<AutoOpen>]
module PersonsModule

open System.Linq

type PersonsActor with
    static member createNew b elevators =
        let newPersons =
            { B = b
              PersonEventsQueue = new System.Collections.Generic.PriorityQueue<PersonEvent, Clock>()
              TransportedPersons = new System.Collections.Generic.List<Person>()
              Elevators = elevators }

        let personsArray =
            match b.SimulationPersons with
            | SimulationPersonsArray pa -> pa
            | SimulationRandomGeneration(personsToCarry, arrivalLength, randomSeed, algorithm) ->
                let rndPersons = new System.Random(randomSeed)

                let rec getRandomPerson () =
                    let entry, exit =
                        match algorithm with
                        | Ground50Levels50 ->
                            if rndPersons.Next(2) = 0 then
                                Floor 0, Floor(rndPersons.Next(1, b.SimulationElevators.Levels))
                            else
                                Floor(rndPersons.Next(1, b.SimulationElevators.Levels)), Floor 0

                        | FullRandom ->
                            Floor(rndPersons.Next(1, b.SimulationElevators.Levels)),
                            Floor(rndPersons.Next(1, b.SimulationElevators.Levels))

                    if entry = exit then
                        getRandomPerson ()
                    else
                        let arrival = Clock(rndPersons.Next(arrivalLength))

                        { Id = PersonId 0
                          EntryFloor = entry
                          ExitFloor = exit
                          ArrivalTime = arrival
                          EntryTime = None
                          ExitTime = None }

                // First generate a random list
                let tempPersonsArray = [| for _ in 0 .. personsToCarry - 1 -> getRandomPerson () |]

                // Then sort by arrival time and assign Ids in arrival order
                Array.sortInPlaceBy (fun p -> p.ArrivalTime) tempPersonsArray

                [| for i in 0 .. personsToCarry - 1 ->
                       { tempPersonsArray[i] with
                           Id = PersonId(i + 1) } |]

        if showInitialPersons then
            printfn "\nPersons for the simulation"

            for p in personsArray do
                printfn "%0A" p

        for p in personsArray do
            let evt =
                { PersonEvent.Clock = p.ArrivalTime
                  Person = p
                  Event = Arrival }

            newPersons.PersonEventsQueue.Enqueue(evt, evt.Clock)

        newPersons


    member this.getNextPersonEventClock() =
        if this.PersonEventsQueue.Count = 0 then
            None
        else
            let evt = this.PersonEventsQueue.Peek()
            Some(evt.Clock)

    member this.processEvent clk =
        let evt = this.PersonEventsQueue.Dequeue()

        if showEvents then
            Logging.logMessage evt.Clock $"Person evt: {evt}"

        assert (clk = evt.Clock)

        match evt.Event with
        | Arrival ->
            Logging.logPersonArrival clk evt.Person
            let prevList = this.Elevators.Landings.getPersons evt.Person.EntryFloor
            this.Elevators.Landings.setPersons evt.Person.EntryFloor (evt.Person :: prevList)
            this.Elevators.callElevator clk evt.Person.EntryFloor evt.Person.ExitFloor

        | ExitCabin ->
            this.TransportedPersons.Add(evt.Person)
            Logging.logPersonExit clk evt.Person


    member this.printDetailedPersonStats() =
        printfn "Detailed Person stats"
        printfn "    Id    Entry    Exit   ArrTime   EntryT ExitTime    WaitEl TotTrans"
        printfn "  ----  ------- -------  -------- -------- --------  -------- --------"

        for p in this.TransportedPersons.OrderBy(fun p -> p.ArrivalTime) do
            let (PersonId pid) = p.Id
            let (Floor entryFloor) = p.EntryFloor
            let (Floor exitFloor) = p.ExitFloor
            let (Clock iArrival) = p.ArrivalTime
            let (Clock iEntry) = p.EntryTime.Value
            let (Clock iExit) = p.ExitTime.Value

            let waitForElevator = iEntry - iArrival
            let totalTransportTime = iExit - iArrival

            printfn
                $"  {pid, 4}  {entryFloor, 7} {exitFloor, 7}  {iArrival, 8} {iEntry, 8} {iExit, 8}  {waitForElevator, 8} {totalTransportTime, 8}"

        printfn ""

    member this.getPersonStats() =
        let ls = List.ofSeq this.TransportedPersons

        let avgWaitForElevator =
            double (ls |> List.sumBy (fun p -> p.waitForElevator ()))
            / double (List.length ls)

        let avgTotalTransport =
            double (ls |> List.sumBy (fun p -> p.totalTransportation ()))
            / double (List.length ls)

        let maxWaitForElevator = ls |> List.map (fun p -> p.waitForElevator ()) |> List.max

        let maxTotalTransport =
            ls |> List.map (fun p -> p.totalTransportation ()) |> List.max

        // Return a PersonsStats record
        { AvgWaitForElevator = avgWaitForElevator
          AvgTotalTransport = avgTotalTransport
          MaxWaitForElevator = maxWaitForElevator
          MaxTotalTransport = maxTotalTransport }

    static member printPersonStats ps =
        printfn "Person stats"
        printfn "  Average wait for elevator: %.1f" ps.AvgWaitForElevator
        printfn "  Average total transport:   %.1f" ps.AvgTotalTransport
        printfn "  Max wait for elevator:     %d" ps.MaxWaitForElevator
        printfn "  Max total transportation:  %d" ps.MaxTotalTransport
