// 31 Elevator
// Elevator simulation in F#
// Elevator business logic
//
// 2014-08-15   PV

[<AutoOpen>]
module ElevatorBusinessLogic

type ElevatorsActor with

    member this.getEndClosingDoorEventRemainingTime clk =
        let rec findEndClosingDoorsEvent (lst: (struct (CommonEvent * ClockPriority)) list) =
            let hasItem, _, nextClkPri = this.B.EventsQueue.TryPeek()

            if not hasItem then
                lst, 0
            else
                let nextEvent = this.B.EventsQueue.Dequeue()
                let st: struct (CommonEvent * ClockPriority) = (nextEvent, nextClkPri)

                match nextEvent with
                | PersonEvent pe -> findEndClosingDoorsEvent (st :: lst)
                | ElevatorEvent ee ->
                    assert (ee.Event = EndClosingDoors)
                    let remainigTime = ee.Clock.minus clk
                    (st :: lst), remainigTime

        let toEnqueueAgain, remainigTime = findEndClosingDoorsEvent []

        if not (List.isEmpty toEnqueueAgain) then
            this.B.EventsQueue.EnqueueRange(toEnqueueAgain)

        remainigTime


    // When a person has just arrived
    member this.callElevator (clk: Clock) (entry: Floor) (exit: Floor) =
        assert (exit <> entry)

        if this.B.LogDetails.ShowEvents then
            printfn "\nCalling elevator from %A to go to %A" entry exit

        // Keep a deep copy for final logging
        let originalCabin = this.Cabins[0].deepCopy ()
        let cabin = this.Cabins[0]

        // Update Landing[entry] CallUp/CallDown
        if entry <> cabin.Floor || (entry = cabin.Floor && cabin.DoorStatus = Closed) then
            let (Floor iEntry) = entry
            if exit > entry then
                // Going Up
                if not this.Landings[iEntry].CallUp then
                    this.Landings[iEntry] <- { this.Landings[iEntry] with CallUp = true };
            else
                // Going Down
                if not this.Landings[iEntry].CallDown then
                    this.Landings[iEntry] <- { this.Landings[iEntry] with CallDown = true };

        // Actually only do something if elevator is idle
        // If elevator is busy, then at some point elevator will arrive
        if cabin.CabinStatus = Idle then
            assert (cabin.DoorStatus = Closed)
            assert (cabin.MotorStatus = Off)
            assert (cabin.Direction = NoDirection)

            // If we call elevator from the floor the cabin is currently waiting, then we just have to open doors
            if cabin.Floor = entry then
                this.B.AddJournalRecord(JournalCabinSetState(Clock = clk, CabinIndex = 0, CabinState = Busy))

                this.Cabins[0] <-
                    { cabin with
                        CabinStatus = Busy
                        DoorStatus = Opening }

                this.B.RegisterEvent (ElevatorEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.OpeningDoorsDuration
                      CabinIndex = 0
                      Event = EndOpeningDoors
                      CreatedOn = clk })
                this.B.AddJournalRecord(JournalCabinDoorsOpenBegin(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor))

            // Otherwise we start accelerating
            else
                let oldDirection = this.Cabins[0].Direction
                this.Cabins[0] <-
                    { cabin with
                        CabinStatus = Busy
                        MotorStatus = Accelerating
                        Direction = if (entry > cabin.Floor) then Up else Down }

                this.B.AddJournalRecord(JournalCabinSetState(Clock = clk, CabinIndex = 0, CabinState = Busy))
                if this.Cabins[0].Direction<>oldDirection then
                    this.B.AddJournalRecord(JournalCabinSetDirection(Clock = clk, CabinIndex = 0, Direction = this.Cabins[0].Direction))
                this.B.AddJournalRecord(JournalMotorAccelerating(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor, Direction = this.Cabins[0].Direction))

                this.B.RegisterEvent (ElevatorEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.AccelerationDuration
                      CabinIndex = 0
                      Event = EndAcceleration
                      CreatedOn = clk })

        // Cabin is not idle, so we need to check two special cases

        // Entry is on current cabin floor, and the door is closing: we stop the closing, and switch to opening mode
        elif cabin.Floor = entry then

            if cabin.DoorStatus = Closing then
                // Cabin is closing doors, so we cancel current event, and register a doors opening event with correct amount of time
                // Since it's complex to find next elevator event in the queue (there could be person events before), do it in a separate function
                this.B.AddJournalRecord(JournalCabinDoorsCloseInterrupt(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor))

                let remainigTime = this.getEndClosingDoorEventRemainingTime clk

                this.Cabins[0] <-
                    { cabin with
                        DoorStatus = Opening
                        IgnoreNextEndClosingDoorsEvent = true } // Door is now opening, we'll ignore coming EndClosingDoorsEvent

                // And we register a new EndOpeningDoors event for the cabin
                this.B.RegisterEvent (ElevatorEvent
                    { ElevatorEvent.Clock = clk.addOffset (this.B.Durations.OpeningDoorsDuration - remainigTime)
                      CabinIndex = 0
                      Event = EndOpeningDoors
                      CreatedOn = clk })

        // Cabin is not idle, on a different floor, but with no direction. If door is closing, then update its direction
        elif cabin.Floor <> entry && cabin.Direction = NoDirection then

            // Cabin must move up or down, so we set direction and wait for the doors to close,
            // once the doors are closed, motor will turn on and start accelerating
            this.Cabins[0] <-
                { cabin with
                    Direction = if (entry > cabin.Floor) then Up else Down }
            this.B.AddJournalRecord(JournalCabinSetDirection(Clock = clk, CabinIndex = 0, Direction = this.Cabins[0].Direction))


        Logging.logCabinUpdate this.B clk originalCabin this.Cabins[0]


    member this.isStopRequestedBeyondPosition (cabin: Cabin) floor direction =
        if cabin.getStopRequested floor then
            true
        else
            let nf = floor.nextFloor this.levels direction

            match nf with
            | None -> false
            | Some f -> this.isStopRequestedBeyondPosition cabin f direction


    member this.getDecisionEvent(clk: Clock) =
        // For landings, we stop only if there is at least one person going in the same direction as the cabin
        let landingStopRequest =
            let (Floor iFloor) = this.Cabins[0].Floor
            let ll = this.Landings[iFloor].Persons

            if ll.IsEmpty then
                false
            else
                let shouldContinueBecauseStopRequested =
                    this.isStopRequestedBeyondPosition this.Cabins[0] this.Cabins[0].Floor this.Cabins[0].Direction

                if not shouldContinueBecauseStopRequested then
                    true // If no more stop requested and there's at least 1 person waiting on landing, then we stop, regardless of whether it's moving up or down
                else
                    let rec checkPersonGoingInCabinDirection lst =
                        match lst with
                        | [] -> false
                        | p :: remainingPersons ->
                            if
                                (this.Cabins[0].Direction = Up && p.ExitFloor > this.Cabins[0].Floor)
                                || (this.Cabins[0].Direction = Down && p.ExitFloor < this.Cabins[0].Floor)
                            then
                                true
                            else
                                checkPersonGoingInCabinDirection remainingPersons

                    checkPersonGoingInCabinDirection ll

        if this.Cabins[0].getStopRequested this.Cabins[0].Floor || landingStopRequest then
            { ElevatorEvent.Clock = clk.addOffset this.B.Durations.FullSpeedBeforeDecisionDuration
              CabinIndex = 0
              Event = EndMovingFullSpeed
              CreatedOn = clk }
        else
            this.B.AddJournalRecord(JournalMotorFullSpeed(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor))

            { ElevatorEvent.Clock = clk.addOffset this.B.Durations.OneLevelFullSpeed
              CabinIndex = 0
              Event = Decision
              CreatedOn = clk }
