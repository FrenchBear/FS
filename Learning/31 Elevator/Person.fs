// 31 Elevator
// Elevator simulation in F#
//
// 2014-08-15   PV


[<AutoOpen>]
module Person

open System.Linq

let transportedPersons = new System.Collections.Generic.List<Person>()

let initialize () =
    let rndPersons = new System.Random(randomSeed)

    let getRandomPerson () =
        let entry, exit =
            if rndPersons.Next(2) = 0 then
                Floor 0, Floor(rndPersons.Next(1, levels))
            else
                Floor(rndPersons.Next(1, levels)), Floor 0

        let arrival = Clock(rndPersons.Next(arrivalLength))

        { Id = PersonId 0
          EntryFloor = entry
          ExitFloor = exit
          ArrivalTime = arrival
          EntryTime = None
          ExitTime = None }

    let tempPersonArray = [| for _ in 0 .. personsToCarry - 1 -> getRandomPerson () |]
    Array.sortInPlaceBy (fun p -> p.ArrivalTime) tempPersonArray

    let personArray =
        [| for i in 0 .. personsToCarry - 1 ->
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

        personEventQueue.Enqueue(evt, evt.Clock)

let getNextPersonEventClock () =
    if personEventQueue.Count = 0 then
        None
    else
        let evt = personEventQueue.Peek()
        Some(evt.Clock)

let processEvent clk =
    let evt = personEventQueue.Dequeue()

    if showEvents then
        Logging.logMessage evt.Clock $"Person evt: {evt}"
        //printfn "\nPerson.processEvent evt=%0A" evt

    assert (clk = evt.Clock)

    match evt.Event with
    | Arrival ->
        Logging.logPersonArrival clk evt.Person
        let prevList = landings.getPersons evt.Person.EntryFloor
        landings.setPersons evt.Person.EntryFloor (evt.Person :: prevList)
        Elevator.callElevator clk evt.Person.EntryFloor evt.Person.ExitFloor

    | ExitCabin ->
        transportedPersons.Add(evt.Person)
        Logging.logPersonExit clk evt.Person


let printFinalStats () =
    printfn "\nPerson stats"

    printfn "    Id    Entry    Exit   ArrTime   EntryT ExitTime    WaitEl TotTrans"
    printfn "  ----  ------- -------  -------- -------- --------  -------- --------"

    for p in transportedPersons.OrderBy(fun p -> p.ArrivalTime) do
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

    let ls = List.ofSeq transportedPersons

    let avgWaitForElevator =
        double (ls |> List.sumBy (fun p -> p.waitForElevator ()))
        / double (List.length ls)

    let avgTotalTransport =
        double (ls |> List.sumBy (fun p -> p.totalTransportation ()))
        / double (List.length ls)

    printfn ""
    printfn "Average wait for elevator: %4.1f" avgWaitForElevator
    printfn "Average total transport:   %4.1f" avgTotalTransport
