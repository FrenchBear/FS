// 31 Elevator
// Learning F#, Elevator simulation
//
// 2024-08-13   PV      First version, only 1 cabin

// ToDo: manage direction change when service is not requested anymore in current direction
// ToDo: Use more realistic durations based on actual building elevator (18s from 0th to 1st floor is a bit too much)
// ToDo: Build a clean log of events and state changes
// ToDo: Manage elevator capacity


// Extent of simulation
let personsToCarry = 1
let arrivalLength = 60

// Elevator and building
let numberOfCabins = 1
let levels = 6 // 0=Ground, and levels 1..5

let accelerationDuration = 2 // and deceleration duration
let oneLevelFullSpeed = 4
let fullSpeedBeforeDecisionDuration = 1 // and after decision before deceleration
let openingDoorsDuration = 3 // and closing doors duration; Include delay between motor off/opening and closed/motor on
let moveInDuration = 2 // and move out duration


type DirectionState =
    | NoDirection
    | Up
    | Down

type Floor = 
    Floor of int

    with
        member this.nextFloor direction =
            let (Floor sf) = this
            match direction with
            | Up -> if sf + 1 < levels then Some(Floor(sf + 1)) else None
            | Down -> if sf > 0 then Some(Floor(sf - 1)) else None
            | NoDirection -> None

type Clock = int

type Person =
    { EntryFloor: Floor
      ArrivalTime: Clock
      EntryTime: Clock
      ExitFloor: Floor
      ExitTime: Clock }

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
      Direction: DirectionState
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


type Landings = 
    { _Persons: Person list array }

    with
        member this.getPersons (floor:Floor) =
            let (Floor fl) = floor
            this._Persons[fl]

        member this.setPersons (floor:Floor) value =
            let (Floor fl) = floor
            this._Persons[fl] <- value
            
let landings = {Landings._Persons = [| for i in 0..levels-1 -> [] |]}

type PersonEventDetail =
    | Arrival
    | ExitCabin

type PersonEvent =
    { Clock: int
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
    { Clock: int
      Event: ElevatorEventDetail }


// Priority queues
let elevatorQueue =
    new System.Collections.Generic.PriorityQueue<ElevatorEvent, Clock>()

let personEventQueue =
    new System.Collections.Generic.PriorityQueue<PersonEvent, Clock>()


module ElevatorModule =
    let evt =
        { ElevatorEvent.Clock = 0
          Event = ElevatorOn }

    elevatorQueue.Enqueue(evt, evt.Clock)

    let getNextElevatorEventClock () =
        if elevatorQueue.Count = 0 then
            None
        else
            let evt = elevatorQueue.Peek()
            Some evt.Clock

    let processEvent clk =
        let evt = elevatorQueue.Dequeue()
        printfn "\nEvevator.processEvent evt=%0A" evt
        printfn "  cabin: %0A" cabins[0]

        assert (clk = evt.Clock)

        match evt.Event with
        | ElevatorOn ->
            printfn "Elevator On and ready"
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
                { ElevatorEvent.Clock = clk + fullSpeedBeforeDecisionDuration
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

                    { ElevatorEvent.Clock = clk + fullSpeedBeforeDecisionDuration
                      Event = EndMovingFullSpeed }
                else
                    { ElevatorEvent.Clock = clk + oneLevelFullSpeed
                      Event = Decision }

            elevatorQueue.Enqueue(evt, evt.Clock)

        | EndMovingFullSpeed ->
            let cabin = cabins[0]
            assert (cabin.Motor = FullSpeed)
            assert (cabin.Door = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.Cabin = Busy)

            let evt =
                { ElevatorEvent.Clock = clk + accelerationDuration
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
                { ElevatorEvent.Clock = clk + openingDoorsDuration
                  Event = EndOpeningDoors }

            elevatorQueue.Enqueue(evt, evt.Clock)

            // Clear the stop requested for current floor
            cabin.clearStopRequested cabin.Floor

            // Decide if we still continue with the same direction (returns true) or not (returns false)
            let rec checkRequestsOneDirection (floor:Floor) direction =
                let nf = floor.nextFloor direction
                match nf with
                | None -> false
                | Some fl ->
                    if cabin.getStopRequested fl
                    then true
                    else checkRequestsOneDirection fl direction

            let rec checkRequests (floor:Floor) direction =
                assert(direction<>NoDirection)
                if checkRequestsOneDirection floor direction 
                then direction
                else
                    let oppositeDirection = if direction=Up then Down else Up
                    if checkRequestsOneDirection floor oppositeDirection 
                    then oppositeDirection
                    else NoDirection

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
                        { ElevatorEvent.Clock = clk + moveInDuration
                          Event = EndOpeningDoors }

                    elevatorQueue.Enqueue(evt, evt.Clock)

                    // Person event to record cabin exit for final stats
                    let evt2 =
                        { PersonEvent.Clock = clk + moveInDuration
                          Person =
                            { p with
                                ExitTime = clk + moveInDuration }
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
                    let updatedPerson = { p with EntryTime = clk }
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
                        { ElevatorEvent.Clock = clk + moveInDuration
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
                        { ElevatorEvent.Clock = clk + 3
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
                for l in 0..levels-1 do
                    assert(not (cabin.getStopRequested (Floor l)))
                    assert((landings.getPersons (Floor l)).IsEmpty)
                assert (cabins[0].Persons.IsEmpty)
                // Ok, we checked to be sure that nobody is waiting, elevator goes into idle state
                cabins[0] <- { cabins[0] with Cabin = Idle }

            | _ ->
                cabins[0] <- { cabins[0] with Motor = Accelerating }

                let evt =
                    { ElevatorEvent.Clock = clk + openingDoorsDuration
                      Event = EndAcceleration }

                elevatorQueue.Enqueue(evt, evt.Clock)


    let callElevator clk (entry:Floor) (exit:Floor) =
        printfn "\nCalling elevator from level %A to go to level %A" entry exit
        assert (exit <> entry)
        let cabin = cabins[0]

        // Actually only do something if elevator is idle; If elevator is busy, then at some point
        // elevator will arrive
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
                    { ElevatorEvent.Clock = clk + openingDoorsDuration
                      Event = EndOpeningDoors }

                elevatorQueue.Enqueue(evt, evt.Clock)

            // Otherwise we start accelerating
            else
                cabin.setStopRequested exit

                cabins[0] <-
                    { cabin with
                        Cabin = Busy
                        Motor = Accelerating
                        Direction = if (entry > exit) then Up else Down }

                let evt =
                    { ElevatorEvent.Clock = clk + accelerationDuration
                      Event = EndAcceleration }

                elevatorQueue.Enqueue(evt, evt.Clock)


module PersonModule =
    let transportedPersons = new System.Collections.Generic.List<Person>()

    let rndPersons = new System.Random(1)

    let getRandomPerson () =
        let entry, exit =
            if rndPersons.Next(2) = 0 then
                Floor 0, Floor (rndPersons.Next(1, levels))
            else
                Floor (rndPersons.Next(1, levels)), Floor 0

        let arrival = rndPersons.Next(arrivalLength)

        { EntryFloor = entry
          ExitFloor = exit
          ArrivalTime = arrival
          EntryTime = 0
          ExitTime = 0 }

    let personArray = [| for _ in 1..personsToCarry -> getRandomPerson () |]

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
        printfn "\nPerson.processEvent evt=%0A" evt
        assert (clk = evt.Clock)

        match evt.Event with
        | Arrival ->
            let prevList = landings.getPersons evt.Person.EntryFloor
            landings.setPersons evt.Person.EntryFloor (evt.Person :: prevList)
            ElevatorModule.callElevator clk evt.Person.EntryFloor evt.Person.ExitFloor

        | ExitCabin -> transportedPersons.Add(evt.Person)

    let printFinalStats () =
        printfn "Final stats"

        for p in transportedPersons do
            printfn "  %0A" p


// Runs the simulation
// In charge or master clock
// Find next event in line, process it, and iterates until there are no more events to process
module SchedulerModule =

    let rec processNextEvent clk =

        let comingEvents =
            [ (PersonModule.getNextPersonEventClock (), PersonModule.processEvent)
              (ElevatorModule.getNextElevatorEventClock (), ElevatorModule.processEvent) ]
            |> List.filter (fun (optClk, _) -> optClk.IsSome)

        if comingEvents.IsEmpty then
            printfn "\nFin de la simulation clk=%d\n" clk
            PersonModule.printFinalStats ()
        else
            let minClock = (fst (List.minBy (fun (optClk, _) -> optClk) comingEvents)).Value
            let nextEvents = comingEvents |> List.filter (fun (opt, _) -> opt = Some(minClock))

            for (_, processor) in nextEvents do
                processor minClock

            processNextEvent minClock

    processNextEvent 0
