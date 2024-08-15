// 31 Elevator
// Elevator simulation in F#
//
// 2014-08-15   PV

[<AutoOpen>]
module Elevator

let initialize () =
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

let rec isStopRequestedBeyondPosition (cabin: Cabin) floor direction =
    if cabin.getStopRequested floor then
        true
    else
        let nf = floor.nextFloor direction

        match nf with
        | None -> false
        | Some f -> isStopRequestedBeyondPosition cabin f direction

let processEvent (clk: Clock) =
    let evt = elevatorQueue.Dequeue()
    assert (clk = evt.Clock)

    if showEvents then
        Logging.logMessage evt.Clock $"Elevator evt: {evt}, cabin: {cabins[0]}"
        //printfn "\nEvevator.processEvent evt=%0A" evt
        //printfn "  cabin: %0A" cabins[0]

    // Keep a deep copy for final logging
    let originalCabin = cabins[0].deepCopy ()

    match evt.Event with
    | ElevatorOn ->
        Logging.logMessage clk "Elevator On and ready"
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

            // For landings, we stop only if there is at least one person going in the same direction as the cabin
            let landingStopRequest =
                let ll = landings.getPersons cabins[0].Floor

                if ll.IsEmpty then
                    false
                else
                    let shouldContinueBecauseStopRequested =
                        isStopRequestedBeyondPosition cabins[0] cabins[0].Floor cabins[0].Direction

                    if not shouldContinueBecauseStopRequested then
                        true // If no more stop requested and there's at least 1 person waiting on landing, then we stop, regardless of whether it's moving up or down
                    else
                        let rec checkPersonGoingInCabinDirection lst =
                            match lst with
                            | [] -> false
                            | p :: remainingPersons ->
                                if
                                    (cabin.Direction = Up && p.ExitFloor > cabins[0].Floor)
                                    || (cabin.Direction = Down && p.ExitFloor < cabins[0].Floor)
                                then
                                    true
                                else
                                    checkPersonGoingInCabinDirection remainingPersons

                        checkPersonGoingInCabinDirection ll

            if cabin.getStopRequested cabins[0].Floor || landingStopRequest then
                { ElevatorEvent.Clock = clk.addOffset fullSpeedBeforeDecisionDuration; Event = EndMovingFullSpeed }
            else
                { ElevatorEvent.Clock = clk.addOffset oneLevelFullSpeed; Event = Decision }

        elevatorQueue.Enqueue(evt, evt.Clock)

    | EndMovingFullSpeed ->
        let cabin = cabins[0]
        assert (cabin.Motor = FullSpeed)
        assert (cabin.Door = Closed)
        assert (cabin.Direction <> NoDirection)
        assert (cabin.Cabin = Busy)

        let evt =
            { ElevatorEvent.Clock = clk.addOffset accelerationDuration; Event = EndDeceleration }

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
            { ElevatorEvent.Clock = clk.addOffset openingDoorsDuration; Event = EndOpeningDoors }

        elevatorQueue.Enqueue(evt, evt.Clock)

        // Clear the stop requested for current floor
        cabin.clearStopRequested cabin.Floor

        // Decide if we still continue with the same direction (returns true) or not (returns false)
        let rec checkRequestsOneDirection (floor: Floor) direction =
            let nf = floor.nextFloor direction

            match nf with
            | None -> false
            | Some fl ->
                if cabin.getStopRequested fl then true
                elif not (List.isEmpty (landings.getPersons fl)) then true
                else checkRequestsOneDirection fl direction

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
                      Person = { p with ExitTime = Some(clk.addOffset moveInDuration) }
                      Event = ExitCabin }

                personEventQueue.Enqueue(evt2, evt2.Clock)

                true // Indicates that a person has moved out, so we shouldn't call allowMoveIn yet

        let allowMoveIn () =
            // If there's still a person on the floor that want to enter, give it 3 seconds to move in
            // First version, ignoring capacity
            let cabin = cabins[0]

            let rec processPersonGoingInSameDirectionAsCabin lst =
                match lst with
                | [] -> false // Nobody moved in
                | p :: remainingPersons ->

                    let personGoesInSaveDirectionAsCabin =
                        if cabin.Direction = NoDirection then
                            true // If cabin has no direction, then let 1st person enter, it will decide on cabin direction
                        else
                            (cabin.Direction = Up && p.ExitFloor > cabin.Floor)
                            || (cabin.Direction = Down && p.ExitFloor < cabin.Floor)

                    if (personGoesInSaveDirectionAsCabin) then
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
                            { ElevatorEvent.Clock = clk.addOffset moveInDuration; Event = EndOpeningDoors }

                        elevatorQueue.Enqueue(evt, evt.Clock)
                        true // Indicates that a person moved in, so when it's done, we must check again whether another
                    // person is candidate to move in before starting motor

                    else
                        processPersonGoingInSameDirectionAsCabin remainingPersons

            processPersonGoingInSameDirectionAsCabin (landings.getPersons cabin.Floor)

        let cabin = cabins[0]
        assert (cabin.Motor = Off)
        assert (cabin.Door = Opening || cabin.Door = Open)
        assert (cabin.Cabin = Busy)
        cabins[0] <- { cabin with Door = Open }

        if not (allowMoveOut ()) then
            if not (allowMoveIn ()) then
                // Nobody remaining to move out or move in, we can close the doors
                cabins[0] <- { cabin with Door = Closing }

                let evt = { ElevatorEvent.Clock = clk.addOffset 3; Event = EndClosingDoors }

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
                { ElevatorEvent.Clock = clk.addOffset openingDoorsDuration; Event = EndAcceleration }

            elevatorQueue.Enqueue(evt, evt.Clock)

    Logging.logCabinUpdate clk originalCabin cabins[0]


let callElevator (clk: Clock) (entry: Floor) (exit: Floor) =
    assert (exit <> entry)

    if showEvents then
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
                { ElevatorEvent.Clock = clk.addOffset openingDoorsDuration; Event = EndOpeningDoors }

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
                { ElevatorEvent.Clock = clk.addOffset accelerationDuration; Event = EndAcceleration }

            elevatorQueue.Enqueue(evt, evt.Clock)

    // Cabin is not idle, but it may be closing doors with no direction. Update direction in this case
    elif cabin.Direction = NoDirection then
        if entry = cabin.Floor then
            if cabin.Door = Closing then
                // Cabin is closing doors, so we cancel current event, and register a doors opening event
                // with correct amount of time
                // Note that this kind of manipulation should be handled by scheduler, but because of module
                // order and dependencies order, it's directly managed here.
                let nextElevatorEvent = getNextElevatorEvent () // Removes the event from queue
                assert (nextElevatorEvent.Event = EndClosingDoors)
                let remainigTime = nextElevatorEvent.Clock.minus clk

                cabins[0] <- { cabin with Door = Opening } // Door is now opening

                // And we register a new EndOpeningDoors event for the cabin
                let evt =
                    { ElevatorEvent.Clock = clk.addOffset (openingDoorsDuration - remainigTime); Event = EndOpeningDoors }

                elevatorQueue.Enqueue(evt, evt.Clock)

            else
                assert (cabin.Door = Opening)
                () // Just wait for the door to open

        else
            // Cabin must move up or down, so we set direction and wait for the doors to close,
            // once the doors are closed, motor will turn on and start accelerating
            cabin.setStopRequested entry

            cabins[0] <-
                { cabin with
                    Direction = if (entry > cabin.Floor) then Up else Down }

    Logging.logCabinUpdate clk originalCabin cabins[0]


let printFinalStats () =
    printfn "\nElevator stats"
    printfn "  ToDo"

