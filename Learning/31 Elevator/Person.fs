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
            b = b
            personEventsQueue = new System.Collections.Generic.PriorityQueue<PersonEvent, Clock>()
            transportedPersons = new System.Collections.Generic.List<Person>()
            elevators = elevators
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
            newPerson.personEventsQueue.Enqueue(evt, evt.Clock)

        newPerson


    member this.getNextPersonEventClock () =
        if this.personEventsQueue.Count = 0 then
            None
        else
            let evt = this.personEventsQueue.Peek()
            Some(evt.Clock)

    member this.processEvent clk =
        let evt = this.personEventsQueue.Dequeue()

        if showEvents then
            Logging.logMessage evt.Clock $"Person evt: {evt}"

        assert (clk = evt.Clock)

        match evt.Event with
        | Arrival ->
            Logging.logPersonArrival clk evt.Person
            let prevList = this.elevators.landings.getPersons evt.Person.EntryFloor
            this.elevators.landings.setPersons evt.Person.EntryFloor (evt.Person :: prevList)
            this.elevators.callElevator clk evt.Person.EntryFloor evt.Person.ExitFloor

        | ExitCabin ->
            this.transportedPersons.Add(evt.Person)
            Logging.logPersonExit clk evt.Person


    member this.printFinalStats () =
        printfn "\nPerson stats"

        printfn "    Id    Entry    Exit   ArrTime   EntryT ExitTime    WaitEl TotTrans"
        printfn "  ----  ------- -------  -------- -------- --------  -------- --------"

        for p in this.transportedPersons.OrderBy(fun p -> p.ArrivalTime) do
            let (PersonId pid) = p.Id
            let (Floor entryFloor) = p.EntryFloor
            let (Floor exitFloor) = p.ExitFloor
            let (Clock arrivalTime) = p.ArrivalTime
            let (Clock entryTime) = p.EntryTime.Value
            let (Clock exitTime) = p.ExitTime.Value

            let waitForElevator = entryTime - arrivalTime
            let totalTransportTime = exitTime - arrivalTime

            printfn
                $"  {pid, 4}  {entryFloor, 7} {exitFloor, 7}  {arrivalTime, 8} {entryTime, 8} {exitTime, 8}  {waitForElevator, 8} {totalTransportTime, 8}"

        let ls = List.ofSeq this.transportedPersons

        let avgWaitForElevator =
            double (ls |> List.sumBy (fun p -> p.waitForElevator ()))
            / double (List.length ls)

        let avgTotalTransport =
            double (ls |> List.sumBy (fun p -> p.totalTransportation ()))
            / double (List.length ls)

        printfn ""
        printfn "Average wait for elevator: %4.1f" avgWaitForElevator
        printfn "Average total transport:   %4.1f" avgTotalTransport
