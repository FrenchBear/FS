// 31 Elevator
// Elevator simulation in F#
//
// 2014-08-15   PV


[<AutoOpen>]
module Person

open System.Linq

type Persons with
    static member createNew b elevators =
        let newPerson = {
            B = b
            PersonEventsQueue = new System.Collections.Generic.PriorityQueue<PersonEvent, Clock>()
            TransportedPersons = new System.Collections.Generic.List<Person>()
            Elevators = elevators
        }

        let rndPersons = new System.Random(b.randomSeed)

        let getRandomPerson () =
            let entry, exit =
                if rndPersons.Next(2) = 0 then
                    Floor 0, Floor(rndPersons.Next(1, b.levels))
                else
                    Floor(rndPersons.Next(1, b.levels)), Floor 0

            let arrival = Clock(rndPersons.Next(b.arrivalLength))

            { Id = PersonId 0
              EntryFloor = entry
              ExitFloor = exit
              ArrivalTime = arrival
              EntryTime = None
              ExitTime = None }

        let tempPersonArray = [| for _ in 0 .. b.personsToCarry - 1 -> getRandomPerson () |]
        Array.sortInPlaceBy (fun p -> p.ArrivalTime) tempPersonArray

        let personArray =
            [| for i in 0 .. b.personsToCarry - 1 ->
                   { tempPersonArray[i] with
                       Id = PersonId i } |]

        if showInitialPersons then
            printfn "\nPersons for the simulation"
            for p in personArray do
                printfn "%0A" p

        for p in personArray do
            let evt =
                { PersonEvent.Clock = p.ArrivalTime
                  Person = p
                  Event = Arrival }
            newPerson.PersonEventsQueue.Enqueue(evt, evt.Clock)

        newPerson


    member this.getNextPersonEventClock () =
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


    member this.printFinalStats () =
        printfn "\nPerson stats"

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

        let ls = List.ofSeq this.TransportedPersons

        let avgWaitForElevator =
            double (ls |> List.sumBy (fun p -> p.waitForElevator ()))
            / double (List.length ls)

        let avgTotalTransport =
            double (ls |> List.sumBy (fun p -> p.totalTransportation ()))
            / double (List.length ls)

        let maxWaitForElevator =
            ls |> List.map (fun p -> p.waitForElevator ()) |> List.max

        let maxTotalTransport =
            ls |> List.map (fun p -> p.totalTransportation ()) |> List.max

        printfn ""
        printfn "  Average wait for elevator: %.1f" avgWaitForElevator
        printfn "  Average total transport:   %.1f" avgTotalTransport
        printfn "  Max wait for elevator:     %d" maxWaitForElevator
        printfn "  Max total transportation:  %d" maxTotalTransport

        // ToDo: show median values, and max for 95% of users
