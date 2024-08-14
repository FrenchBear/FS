// 31 Elevator
// Learning F#, Elevator simulation
//
// 2024-08-13   PV      First version, only 1 cabin

// ToDo: Manage elevator capacity
// ToDo: Manage more than 1 elevator

//type Zap = {
//    Id: int
//    T: int array
//}

//let z = {Zap.Id=2; T=Array.create 3 0}
//printfn "z = %0A" z
////let z' = {z with Id=3; T=Array.copy z.T}
//let z' = {z with Id=3}
//printfn "z' = %0A" z' 

//z'.T[0] <- 12
//printfn "z' = %0A" z'
//printfn "z = %0A" z

//printfn ""

System.Console.OutputEncoding <- System.Text.Encoding.UTF8
//printfn "→↔↛↝↠↣↦↪↬↭↮↱↳↴↹⇄⇆⇉⇎⇏⇒⇔⇛⇝⇢⇥⇨⇰⇴⇶⇸⇹⇻⇼⇾⇿⍈⍼▻➔➙➛➜➝➞➟➠➡➢➣➤➥➦➧➨➩➪➫➬➭➮➯➱➲➳➵➸➺➻➼➽➾⟴⟶⟷⟹⟺⟼⟾⟿⤀⤁⤃⤄⤅⤇⤍⤏⤐⤑⤔⤕⤖⤗⤘⤚⤜⤞⤠⤳⤴⤵⤷⤸⤾⥂⥃⥄⥅⥇⥈⥰⥱⥲⥴⥵⥸⥹⦨⦪⦬⦮⦳⧴⬄⬌⬎⬏⭃⭄⭆⭇⭈⭌⭢⭤⭬⭲⭼⮀⮂⮆⮊⮌⮕⮙⮚⮞⮡⮣⮥⮧⮩⮫⮭⮯⮱⮳⮵⮷⯮📲🔀🔁🔂🔛🔜🗘🠂🠆🠊🠒🠖🠚🠞🠢🠦🠪🠮🠲🠶🠺🠾🡂🡆🡒🡘🡢🡪🡲🡺🢂🢒🢖🢚🢡🢣🢥🢦🢧🢩🢫🢱"
//printfn ""


open System.Linq

let traceEvents = false
let randomSeed = 1

// Extent of simulation
let personsToCarry = 10
let arrivalLength = 300

// Elevator and building
let numberOfCabins = 1
let levels = 6 // 0=Ground, and levels 1..5

let accelerationDuration = 2 // and deceleration duration
let oneLevelFullSpeed = 4
let fullSpeedBeforeDecisionDuration = 1 // and after decision before deceleration
let openingDoorsDuration = 3 // and closing doors duration; Include delay between motor off/opening and closed/motor on
let moveInDuration = 2 // and move out duration

(*
    One level with acceleration, decision, and deceleration: 6s
    -+----
     |   \
    -+-  | Decelerating: 2s
     |   /
    -+-
     |   FullSpeed: 1s
    -+-  Decision point = half level, decide whether we continue full speed or we stop
     |   FullSpeed: 1s
    -+-
     |   \
    -+-  | Accelerating: 2s
     |   /
    -+----

    One level with full speed, from decision point to next decision point: 4s
*)


type Direction =
    | NoDirection
    | Up
    | Down

// Use typed Floor rather than int alias for better type checking
type Floor =
    | Floor of int

    member this.nextFloor direction =
        let (Floor sf) = this

        match direction with
        | Up -> if sf + 1 < levels then Some(Floor(sf + 1)) else None
        | Down -> if sf > 0 then Some(Floor(sf - 1)) else None
        | NoDirection -> None

// Use typed Clock rather than int alias for better type checking
type Clock =
    | Clock of int

    member this.addOffset offset =
        let (Clock cl) = this
        Clock(cl + offset)

    member this.minus clk =
        let (Clock cl) = this
        let (Clock other) = clk
        cl - other


// ----------------------------------------
// Person

type PersonId = PersonId of int

type Person =
    { Id: PersonId
      EntryFloor: Floor
      ArrivalTime: Clock
      ExitFloor: Floor
      EntryTime: Clock option
      ExitTime: Clock option }

    member private this.calcTime(endTime: Clock option) =
        assert (endTime.IsSome)
        let (Clock arrival) = this.ArrivalTime
        let (Clock endT) = endTime.Value
        endT - arrival

    member this.waitForElevator() = this.calcTime this.EntryTime
    member this.totalTransportation() = this.calcTime this.ExitTime

// ----------------------------------------
// Cabin

type MotorState =
    | Off
    | FullSpeed
    | Accelerating
    | Decelerating

type DoorState =
    | Open
    | Closed
    | Opening
    | Closing

type CabinState =
    | Idle
    | Busy

type Cabin =
    { Floor: Floor
      Motor: MotorState
      Door: DoorState
      Direction: Direction
      Cabin: CabinState
      _StopRequested: bool array
      //Capacity: int
      Persons: Person list }

    member this.getStopRequested floor =
        let (Floor f) = floor
        this._StopRequested[f]

    member this.setStopRequested floor =
        let (Floor f) = floor
        this._StopRequested[f] <- true

    member this.clearStopRequested floor =
        let (Floor f) = floor
        this._StopRequested[f] <- false

    // Need a deepCopy because of array, since an array is just a pointer to a mutable structure
    // On the other hand, Persons is an immutable list, so there's no problem with basic record copy
    member this.deepCopy() =
        { this with _StopRequested = Array.copy this._StopRequested }


let cabinInitialState =
    { Floor = Floor 0
      Motor = Off
      Direction = NoDirection
      Door = Closed
      Cabin = Idle
      _StopRequested = Array.create levels false
      //Capacity = 10
      Persons = [] }

let cabins = Array.create numberOfCabins cabinInitialState


// ----------------------------------------
// Landings

type Landings =
    { _Persons: Person list array }

    member this.getPersons(floor: Floor) =
        let (Floor fl) = floor
        this._Persons[fl]

    member this.setPersons (floor: Floor) value =
        let (Floor fl) = floor
        this._Persons[fl] <- value

let landings = { Landings._Persons = [| for i in 0 .. levels - 1 -> [] |] }


// ----------------------------------------
// Events

type PersonEventDetail =
    | Arrival
    | ExitCabin

type PersonEvent =
    { Clock: Clock
      Event: PersonEventDetail
      Person: Person }


type ElevatorEventDetail =
    | ElevatorOn
    | EndAcceleration
    | EndDeceleration
    | EndMovingFullSpeed
    | Decision
    | EndOpeningDoors
    | EndClosingDoors

type ElevatorEvent =
    { Clock: Clock
      Event: ElevatorEventDetail }


// ----------------------------------------
// Priority queues
let elevatorQueue =
    new System.Collections.Generic.PriorityQueue<ElevatorEvent, Clock>()

let personEventQueue =
    new System.Collections.Generic.PriorityQueue<PersonEvent, Clock>()


// ----------------------------------------
// Modules

module LoggingModule =
    let logMessage clk msg =
        let (Clock iClk) = clk
        printfn $"clk: {iClk, 4}  {msg}"

    let logCabinUpdate clk before after =
        let lst = new System.Collections.Generic.List<string>()

        if before.Floor <> after.Floor then
            lst.Add($"Floor {before.Floor} -> {after.Floor}")

        if before.Motor <> after.Motor then
            lst.Add($"Motor {before.Motor} -> {after.Motor}")

        if before.Door <> after.Door then
            lst.Add($"Door {before.Door} -> {after.Door}")

        if before.Direction <> after.Direction then
            lst.Add($"Direction {before.Direction} -> {after.Direction}")

        if before.Cabin <> after.Cabin then
            lst.Add($"Cabin {before.Cabin} -> {after.Cabin}")


        let lstStopRequested = new System.Collections.Generic.List<string>()

        for i in 0 .. levels - 1 do
            if before._StopRequested[i] <> after._StopRequested[i] then
                lstStopRequested.Add($"StopRequested[{i}]: {before._StopRequested[i]} -> {after._StopRequested[i]}")

        if not (lstStopRequested.Count = 0) then
            lst.Add(System.String.Join(", ", lstStopRequested))


        let lstPersons = new System.Collections.Generic.List<string>()

        if List.length before.Persons <> List.length after.Persons then
            lstPersons.Add($"Persons count {List.length before.Persons} -> {List.length after.Persons}")

        for pb in before.Persons do
            let ixOpt = List.tryFindIndex (fun pa -> pa.Id = pb.Id) after.Persons

            if ixOpt.IsNone then
                let (PersonId pid) = pb.Id
                lstPersons.Add($"Person {pid} left")

        for pa in after.Persons do
            let ixOpt = List.tryFindIndex (fun pb -> pb.Id = pa.Id) before.Persons

            if ixOpt.IsNone then
                let (PersonId pid) = pa.Id
                lstPersons.Add($"Person {pid} entered")

        if not (lstPersons.Count = 0) then
            lst.Add(System.String.Join(", ", lstPersons))

        if not (lst.Count = 0) then
            logMessage clk (System.String.Join(", ", lst))

    let logPersonArrival clk p =
        let (PersonId person) = p.Id
        let (Floor entry) = p.EntryFloor
        let (Floor exit) = p.ExitFloor
        logMessage clk $"Person {person}: Arrival on Floor {entry} to go to Floor {exit}"

    let logPersonExit clk p =
        let (PersonId pid) = p.Id
        let (Clock arrivalClk) = p.ArrivalTime
        let (Floor entry) = p.EntryFloor
        let (Floor exit) = p.ExitFloor
        let (Clock entryClk) = p.EntryTime.Value
        let waitingCabin = entryClk - arrivalClk
        let (Clock exitClk) = p.ExitTime.Value
        let totalTransportationTime = exitClk - arrivalClk

        logMessage
            clk
            $"Person {pid}: Exit on Floor {exit} at {exitClk}, Arrived on Floor {entry} at {arrivalClk}, Entered cabin at {entryClk} after waiting {waitingCabin}, Total time {totalTransportationTime}"


module ElevatorModule =
    // Initial event, just to check that initial state is Ok
    let evt =
        { ElevatorEvent.Clock = Clock 0
          Event = ElevatorOn }

    elevatorQueue.Enqueue(evt, evt.Clock)

    let getNextElevatorEventClock () =
        if elevatorQueue.Count = 0 then
            None
        else
            let evt = elevatorQueue.Peek()
            Some evt.Clock

    let getNextElevatorEvent () = elevatorQueue.Dequeue()

    let processEvent (clk: Clock) =
        let evt = elevatorQueue.Dequeue()
        assert (clk = evt.Clock)

        if traceEvents then
            printfn "\nEvevator.processEvent evt=%0A" evt
            printfn "  cabin: %0A" cabins[0]

        // Keep a deep copy for final logging
        let originalCabin = cabins[0].deepCopy ()

        match evt.Event with
        | ElevatorOn ->
            LoggingModule.logMessage clk "Elevator On and ready"
            let cabin = cabins[0]
            assert (cabin.Motor = Off)
            assert (cabin.Door = Closed)
            assert (cabin.Direction = NoDirection)
            assert (cabin.Cabin = Idle)

        | EndAcceleration ->
            let cabin = cabins[0]
            assert (cabin.Motor = Accelerating)
            assert (cabin.Door = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.Cabin = Busy)

            let evt =
                { ElevatorEvent.Clock = clk.addOffset fullSpeedBeforeDecisionDuration
                  Event = Decision }

            elevatorQueue.Enqueue(evt, evt.Clock)
            cabins[0] <- { cabin with Motor = FullSpeed }

        | Decision ->
            // Update Position
            let cabin = cabins[0]
            assert (cabin.Motor = FullSpeed)
            assert (cabin.Door = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.Cabin = Busy)

            let nf = cabin.Floor.nextFloor cabin.Direction
            assert (nf.IsSome)
            cabins[0] <- { cabin with Floor = nf.Value }

            // Decide if we stop at next floor or not
            let evt =
                if cabin.getStopRequested cabins[0].Floor then

                    { ElevatorEvent.Clock = clk.addOffset fullSpeedBeforeDecisionDuration
                      Event = EndMovingFullSpeed }
                else
                    { ElevatorEvent.Clock = clk.addOffset oneLevelFullSpeed
                      Event = Decision }

            elevatorQueue.Enqueue(evt, evt.Clock)

        | EndMovingFullSpeed ->
            let cabin = cabins[0]
            assert (cabin.Motor = FullSpeed)
            assert (cabin.Door = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.Cabin = Busy)

            let evt =
                { ElevatorEvent.Clock = clk.addOffset accelerationDuration
                  Event = EndDeceleration }

            elevatorQueue.Enqueue(evt, evt.Clock)
            cabins[0] <- { cabin with Motor = Decelerating }

        | EndDeceleration ->
            let cabin = cabins[0]
            assert (cabin.Motor = Decelerating)
            assert (cabin.Door = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.Cabin = Busy)

            // Ok, we arrive at a floor with stop requested
            let evt =
                { ElevatorEvent.Clock = clk.addOffset openingDoorsDuration
                  Event = EndOpeningDoors }

            elevatorQueue.Enqueue(evt, evt.Clock)

            // Clear the stop requested for current floor
            cabin.clearStopRequested cabin.Floor

            // Decide if we still continue with the same direction (returns true) or not (returns false)
            let rec checkRequestsOneDirection (floor: Floor) direction =
                let nf = floor.nextFloor direction

                match nf with
                | None -> false
                | Some fl ->
                    if cabin.getStopRequested fl then
                        true
                    else
                        checkRequestsOneDirection fl direction

            let rec checkRequests (floor: Floor) direction =
                assert (direction <> NoDirection)

                if checkRequestsOneDirection floor direction then
                    direction
                else
                    let oppositeDirection = if direction = Up then Down else Up

                    if checkRequestsOneDirection floor oppositeDirection then
                        oppositeDirection
                    else
                        NoDirection

            let newDirection = checkRequests cabin.Floor cabin.Direction

            cabins[0] <-
                { cabin with
                    Direction = newDirection
                    Door = Opening
                    Motor = Off }

        | EndOpeningDoors ->
            let allowMoveOut () =
                // If there's still in the cabin a person that needs to get out, then give it 3 seconds to move out
                let cabin = cabins[0]
                let ix = List.tryFindIndex (fun p -> p.ExitFloor = cabin.Floor) cabin.Persons

                match ix with
                | None -> false
                | Some i ->
                    let p = cabin.Persons[i]
                    let newPersons = List.removeAt i cabin.Persons
                    cabins[0] <- { cabin with Persons = newPersons }

                    // Elevator event to continue with next person moving out or in the elevator at current floor
                    let evt =
                        { ElevatorEvent.Clock = clk.addOffset moveInDuration
                          Event = EndOpeningDoors }

                    elevatorQueue.Enqueue(evt, evt.Clock)

                    // Person event to record cabin exit for final stats
                    let evt2 =
                        { PersonEvent.Clock = clk.addOffset moveInDuration
                          Person =
                            { p with
                                ExitTime = Some(clk.addOffset moveInDuration) }
                          Event = ExitCabin }

                    personEventQueue.Enqueue(evt2, evt2.Clock)

                    true // Indicates that a person has moved out, so we shouldn't call allowMoveIn yet

            let allowMoveIn () =
                // If there's still a person on the floor that want to enter, give it 3 seconds to move in
                // First version, person moves in the cabin, regardless of cabin direction and ignoring capacity
                let cabin = cabins[0]

                match landings.getPersons cabin.Floor with
                | [] -> false
                | p :: remainingPersons ->
                    let updatedPerson = { p with EntryTime = Some clk }
                    cabin.setStopRequested p.ExitFloor

                    let newDirection =
                        if cabin.Direction = NoDirection then
                            if p.ExitFloor > cabin.Floor then Up else Down
                        else
                            cabin.Direction

                    cabins[0] <-
                        { cabin with
                            Persons = updatedPerson :: cabin.Persons
                            Direction = newDirection }

                    landings.setPersons cabin.Floor remainingPersons

                    // Elevator event to continue with next person moving out or in the elevator at current floor
                    let evt =
                        { ElevatorEvent.Clock = clk.addOffset moveInDuration
                          Event = EndOpeningDoors }

                    elevatorQueue.Enqueue(evt, evt.Clock)

                    true // Indicates that a person moved in, so we must chech again whether another person
            // is candidate to move in before starting motor

            let cabin = cabins[0]
            assert (cabin.Motor = Off)
            assert (cabin.Door = Opening || cabin.Door = Open)
            assert (cabin.Cabin = Busy)
            cabins[0] <- { cabin with Door = Open }

            if not (allowMoveOut ()) then
                if not (allowMoveIn ()) then
                    // Nobody remaining to move out or move in, we can close the doors
                    cabins[0] <- { cabin with Door = Closing }

                    let evt =
                        { ElevatorEvent.Clock = clk.addOffset 3
                          Event = EndClosingDoors }

                    elevatorQueue.Enqueue(evt, evt.Clock)

        | EndClosingDoors ->
            let cabin = cabins[0]
            assert (cabin.Motor = Off)
            assert (cabin.Door = Closing)
            assert (cabin.Cabin = Busy)
            cabins[0] <- { cabin with Door = Closed }

            match cabin.Direction with
            | NoDirection ->
                for l in 0 .. levels - 1 do
                    assert (not (cabin.getStopRequested (Floor l)))
                    assert ((landings.getPersons (Floor l)).IsEmpty)

                assert (cabins[0].Persons.IsEmpty)
                // Ok, we checked to be sure that nobody is waiting, elevator goes into idle state
                cabins[0] <- { cabins[0] with Cabin = Idle }

            | _ ->
                cabins[0] <- { cabins[0] with Motor = Accelerating }

                let evt =
                    { ElevatorEvent.Clock = clk.addOffset openingDoorsDuration
                      Event = EndAcceleration }

                elevatorQueue.Enqueue(evt, evt.Clock)

        LoggingModule.logCabinUpdate clk originalCabin cabins[0]


    let callElevator (clk: Clock) (entry: Floor) (exit: Floor) =
        assert (exit <> entry)

        if traceEvents then
            printfn "\nCalling elevator from level %A to go to level %A" entry exit

        // Keep a deep copy for final logging
        let originalCabin = cabins[0].deepCopy ()

        let cabin = cabins[0]

        // Actually only do something if elevator is idle
        // If elevator is busy, then at some point elevator will arrive
        if cabin.Cabin = Idle then
            assert (cabin.Door = Closed)
            assert (cabin.Motor = Off)
            assert (cabin.Direction = NoDirection)

            // If we call elevator from the floor the cabin is currently waiting, then we just have to open doors
            if cabin.Floor = entry then
                cabins[0] <-
                    { cabin with
                        Cabin = Busy
                        Door = Opening }

                let evt =
                    { ElevatorEvent.Clock = clk.addOffset openingDoorsDuration
                      Event = EndOpeningDoors }

                elevatorQueue.Enqueue(evt, evt.Clock)

            // Otherwise we start accelerating
            else
                cabin.setStopRequested entry

                cabins[0] <-
                    { cabin with
                        Cabin = Busy
                        Motor = Accelerating
                        Direction = if (entry > cabin.Floor) then Up else Down }

                let evt =
                    { ElevatorEvent.Clock = clk.addOffset accelerationDuration
                      Event = EndAcceleration }

                elevatorQueue.Enqueue(evt, evt.Clock)

        // Cabin is not idle, but it may be closing doors with no direction. Update direction in this case
        elif cabin.Direction = NoDirection then
            if entry = cabin.Floor then
                // Cabin is closing doors, so we cancel current event, and register a doors opening event
                // with correct amount of time
                // Note that this kind of manipulation should be handled by scheduler, but because of module
                // order and dependencies order, it's directly managed here.
                assert (cabin.Door = Closing)
                let nextElevatorEvent = getNextElevatorEvent () // Removes the event from queue
                assert (nextElevatorEvent.Event = EndClosingDoors)
                let remainigTime = nextElevatorEvent.Clock.minus clk

                cabins[0] <- { cabin with Door = Opening }      // Door is now opening

                // And we register a new EndOpeningDoors event for the cabin
                let evt =
                    { ElevatorEvent.Clock = clk.addOffset (openingDoorsDuration - remainigTime)
                      Event = EndOpeningDoors }

                elevatorQueue.Enqueue(evt, evt.Clock)

            else
                // Cabin must move up or down, so we set direction and wiat for the doors to close,
                // once the doors are closed, motor will turn on and start accelerating
                cabins[0] <-
                    { cabin with
                        Direction = if (entry > cabin.Floor) then Up else Down }

        LoggingModule.logCabinUpdate clk originalCabin cabins[0]


module PersonModule =
    let transportedPersons = new System.Collections.Generic.List<Person>()

    let rndPersons = new System.Random(randomSeed)

    let getRandomPerson id =
        let entry, exit =
            if rndPersons.Next(2) = 0 then
                Floor 0, Floor(rndPersons.Next(1, levels))
            else
                Floor(rndPersons.Next(1, levels)), Floor 0

        let arrival = Clock(rndPersons.Next(arrivalLength))

        { Id = id
          EntryFloor = entry
          ExitFloor = exit
          ArrivalTime = arrival
          EntryTime = None
          ExitTime = None }

    let personArray = [| for i in 1..personsToCarry -> getRandomPerson (PersonId i) |]

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

        if traceEvents then
            printfn "\nPerson.processEvent evt=%0A" evt

        assert (clk = evt.Clock)

        match evt.Event with
        | Arrival ->
            LoggingModule.logPersonArrival clk evt.Person
            let prevList = landings.getPersons evt.Person.EntryFloor
            landings.setPersons evt.Person.EntryFloor (evt.Person :: prevList)
            ElevatorModule.callElevator clk evt.Person.EntryFloor evt.Person.ExitFloor

        | ExitCabin ->
            transportedPersons.Add(evt.Person)
            LoggingModule.logPersonExit clk evt.Person


    let printFinalStats () =
        printfn "Final stats"

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


// Runs the simulation
// In charge or master clock
// Find next event in line, process it, and iterates until there are no more events to process
module SchedulerModule =

    let rec processNextEvent (clk: Clock) =

        let comingEvents =
            [ (PersonModule.getNextPersonEventClock (), PersonModule.processEvent)
              (ElevatorModule.getNextElevatorEventClock (), ElevatorModule.processEvent) ]
            |> List.filter (fun (optClk, _) -> optClk.IsSome)

        if comingEvents.IsEmpty then
            printfn "\nFin de la simulation clk=%A\n" clk
            PersonModule.printFinalStats ()
        else
            let minClock = (fst (List.minBy (fun (optClk, _) -> optClk) comingEvents)).Value
            let nextEvents = comingEvents |> List.filter (fun (opt, _) -> opt = Some(minClock))

            for (_, processor) in nextEvents do
                processor minClock

            processNextEvent minClock

    processNextEvent (Clock 0)
