// 31 Elevator
// Elevator simulation in F#
// Elevator business logic
//
// 2014-08-15   PV

[<AutoOpen>]
module ElevatorBusinessLogic

type ElevatorsActor with

    member this.callElevator (clk: Clock) (entry: Floor) (exit: Floor) =
        assert (exit <> entry)

        if this.B.LogDetails.showEvents then
            printfn "\nCalling elevator from level %A to go to level %A" entry exit

        // Keep a deep copy for final logging
        let originalCabin = this.Cabins[0].deepCopy ()

        let cabin = this.Cabins[0]

        // Actually only do something if elevator is idle
        // If elevator is busy, then at some point elevator will arrive
        if cabin.Cabin = Idle then
            assert (cabin.Door = Closed)
            assert (cabin.Motor = Off)
            assert (cabin.Direction = NoDirection)

            // If we call elevator from the floor the cabin is currently waiting, then we just have to open doors
            if cabin.Floor = entry then
                this.recordStat clk 0 StatCabinBusy

                this.Cabins[0] <-
                    { cabin with
                        Cabin = Busy
                        Door = Opening }

                this.registerEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.openingDoorsDuration
                      CabinIndex = 0
                      Event = EndOpeningDoors
                      CreatedOn = clk
                    }

            // Otherwise we start accelerating
            else
                this.recordStat clk 0 StatCabinBusy
                this.recordStat clk 0 StatMotorAccelerating

                this.Cabins[0] <-
                    { cabin with
                        Cabin = Busy
                        Motor = Accelerating
                        Direction = if (entry > cabin.Floor) then Up else Down }

                this.registerEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.accelerationDuration
                      CabinIndex = 0
                      Event = EndAcceleration
                      CreatedOn = clk
                    }

        // Cabin is not idle, but it may be closing doors with no direction. Update direction in this case
        elif cabin.Direction = NoDirection then
            if entry = cabin.Floor then
                if cabin.Door = Closing then
                    // Cabin is closing doors, so we cancel current event, and register a doors opening event
                    // with correct amount of time
                    // Note that this kind of manipulation should be handled by scheduler, but because of module
                    // order and dependencies order, it's directly managed here.
                    let nextElevatorEvent = this.getNextElevatorEvent () // Removes the event from queue
                    assert (nextElevatorEvent.Event = EndClosingDoors)
                    let remainigTime = nextElevatorEvent.Clock.minus clk

                    this.Cabins[0] <- { cabin with Door = Opening } // Door is now opening

                    // And we register a new EndOpeningDoors event for the cabin
                    this.registerEvent
                        { ElevatorEvent.Clock = clk.addOffset (this.B.Durations.openingDoorsDuration - remainigTime)
                          CabinIndex = 0
                          Event = EndOpeningDoors
                          CreatedOn = clk
                        }

                else
                    assert (cabin.Door = Opening || cabin.Door = Open)
                    () // Just wait for the door to open

            else
                // Cabin must move up or down, so we set direction and wait for the doors to close,
                // once the doors are closed, motor will turn on and start accelerating
                this.Cabins[0] <-
                    { cabin with
                        Direction = if (entry > cabin.Floor) then Up else Down }

        Logging.logCabinUpdate clk this.B originalCabin this.Cabins[0]

    member this.isStopRequestedBeyondPosition (cabin: Cabin) floor direction =
        if cabin.getStopRequested floor then
            true
        else
            let nf = floor.nextFloor this.levels direction

            match nf with
            | None -> false
            | Some f -> this.isStopRequestedBeyondPosition cabin f direction

    member this.getDecisionEvent (clk:Clock) =
        // For landings, we stop only if there is at least one person going in the same direction as the cabin
        let landingStopRequest =
            let ll = this.Landings.getPersons this.Cabins[0].Floor

            if ll.IsEmpty then
                false
            else
                let shouldContinueBecauseStopRequested =
                    this.isStopRequestedBeyondPosition
                        this.Cabins[0]
                        this.Cabins[0].Floor
                        this.Cabins[0].Direction

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
            { ElevatorEvent.Clock = clk.addOffset this.B.Durations.fullSpeedBeforeDecisionDuration
              CabinIndex = 0
              Event = EndMovingFullSpeed
              CreatedOn = clk
            }
        else
            this.recordStat clk 0 StatMotorFullSpeed

            { ElevatorEvent.Clock = clk.addOffset this.B.Durations.oneLevelFullSpeed
              CabinIndex = 0
              Event = Decision
              CreatedOn = clk
            }
