// 31 Elevator
// Elevator simulation in F#
//
// 2014-08-15   PV

[<AutoOpen>]
module ElevatorsModule

type ElevatorsActor with
    static member createNew b =
        let cabinInitialState =
            { Floor = Floor 0
              Motor = Off
              Direction = NoDirection
              Door = Closed
              Cabin = Idle
              _StopRequested = Array.create b.SimulationElevators.Levels false
              Capacity = b.SimulationElevators.Capacity
              Persons = [] }

        let newElevator =
            { B = b
              ElevatorEventsQueue = new System.Collections.Generic.PriorityQueue<ElevatorEvent, Clock>()
              Cabins = Array.create b.SimulationElevators.NumberOfCabins cabinInitialState
              Statistics = Array.create b.SimulationElevators.NumberOfCabins []
              Landings = { Landings._Persons = [| for i in 0 .. b.SimulationElevators.Levels - 1 -> [] |] }
              Persons = None }

        // Initial event, just to check that initial state is Ok
        newElevator.registerEvent
            { ElevatorEvent.Clock = Clock 0
              CabinIndex = 0
              Event = ElevatorOn }

        newElevator

    member this.getNextElevatorEventClock() =
        if this.ElevatorEventsQueue.Count = 0 then
            None
        else
            let evt = this.ElevatorEventsQueue.Peek()
            Some evt.Clock

    member this.getNextElevatorEvent() = this.ElevatorEventsQueue.Dequeue()

    member this.isStopRequestedBeyondPosition (cabin: Cabin) floor direction =
        if cabin.getStopRequested floor then
            true
        else
            let nf = floor.nextFloor this.levels direction

            match nf with
            | None -> false
            | Some f -> this.isStopRequestedBeyondPosition cabin f direction

    member this.recordStat clk ixCabin stat =
        this.Statistics[ixCabin] <- (clk, stat) :: this.Statistics[ixCabin]

    member this.registerEvent evt =
        this.ElevatorEventsQueue.Enqueue(evt, evt.Clock)

    member this.processEvent(clk: Clock) =
        let evt = this.ElevatorEventsQueue.Dequeue()
        assert (clk = evt.Clock)

        if this.B.LogDetails.showEvents then
            logMessage this.B evt.Clock $"Elevator evt: {evt}, cabin: {this.Cabins[0]}"

        // Keep a deep copy for final logging
        let originalCabin = this.Cabins[0].deepCopy ()

        match evt.Event with
        | ElevatorOn ->
            Logging.logMessage this.B clk "Elevator On and ready"
            let cabin = this.Cabins[0]
            assert (cabin.Motor = Off)
            assert (cabin.Door = Closed)
            assert (cabin.Direction = NoDirection)
            assert (cabin.Cabin = Idle)
            assert (cabin.Floor = Floor 0)

            this.recordStat clk 0 StatCabinIdle
            this.recordStat clk 0 (StatMotorOff(Floor 0))
            this.recordStat clk 0 StatDoorsClosed
            this.recordStat clk 0 (StatPersonsInCabin 0)

        | EndAcceleration ->
            let cabin = this.Cabins[0]
            assert (cabin.Motor = Accelerating)
            assert (cabin.Door = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.Cabin = Busy)

            this.registerEvent
                { ElevatorEvent.Clock = clk.addOffset this.B.Durations.fullSpeedBeforeDecisionDuration
                  CabinIndex = 0
                  Event = Decision }

            this.Cabins[0] <- { cabin with Motor = FullSpeed }

        | Decision ->
            // Update Position
            let cabin = this.Cabins[0]
            assert (cabin.Motor = FullSpeed)
            assert (cabin.Door = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.Cabin = Busy)

            let nf = cabin.Floor.nextFloor this.levels cabin.Direction
            assert (nf.IsSome)
            this.Cabins[0] <- { cabin with Floor = nf.Value }

            // Decide if we stop at next floor or not
            let evt =

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
                                        (cabin.Direction = Up && p.ExitFloor > this.Cabins[0].Floor)
                                        || (cabin.Direction = Down && p.ExitFloor < this.Cabins[0].Floor)
                                    then
                                        true
                                    else
                                        checkPersonGoingInCabinDirection remainingPersons

                            checkPersonGoingInCabinDirection ll

                if cabin.getStopRequested this.Cabins[0].Floor || landingStopRequest then
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.fullSpeedBeforeDecisionDuration
                      CabinIndex = 0
                      Event = EndMovingFullSpeed }
                else
                    this.recordStat clk 0 (StatMotorFullSpeed nf.Value)

                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.oneLevelFullSpeed
                      CabinIndex = 0
                      Event = Decision }

            this.registerEvent evt

        | EndMovingFullSpeed ->
            let cabin = this.Cabins[0]
            assert (cabin.Motor = FullSpeed)
            assert (cabin.Door = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.Cabin = Busy)

            this.registerEvent
                { ElevatorEvent.Clock = clk.addOffset this.B.Durations.accelerationDuration
                  CabinIndex = 0
                  Event = EndDeceleration }

            this.recordStat clk 0 StatMotorDecelerating
            this.Cabins[0] <- { cabin with Motor = Decelerating }

        | EndDeceleration ->
            let cabin = this.Cabins[0]
            assert (cabin.Motor = Decelerating)
            assert (cabin.Door = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.Cabin = Busy)

            this.recordStat clk 0 (StatMotorOff cabin.Floor)

            // Ok, we arrive at a floor with stop requested
            this.registerEvent
                { ElevatorEvent.Clock = clk.addOffset this.B.Durations.openingDoorsDuration
                  CabinIndex = 0
                  Event = EndOpeningDoors }

            // Clear the stop requested for current floor
            cabin.clearStopRequested cabin.Floor

            // Decide if we still continue with the same direction (returns true) or not (returns false)
            let rec checkRequestsOneDirection (floor: Floor) direction =
                let nf = floor.nextFloor this.levels direction

                match nf with
                | None -> false
                | Some fl ->
                    if cabin.getStopRequested fl then
                        true
                    elif not (List.isEmpty (this.Landings.getPersons fl)) then
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

            this.Cabins[0] <-
                { cabin with
                    Direction = newDirection
                    Door = Opening
                    Motor = Off }

        | EndOpeningDoors ->
            let doorsJustOpening = this.Cabins[0].Door = Opening

            let allowMoveOut () =
                // If there's still in the cabin a person that needs to get out, then give it 3 seconds to move out
                let cabin = this.Cabins[0]
                let ix = List.tryFindIndexBack (fun p -> p.ExitFloor = cabin.Floor) cabin.Persons

                match ix with
                | None -> false
                | Some i ->
                    let p = cabin.Persons[i]
                    let newPersons = List.removeAt i cabin.Persons
                    this.Cabins[0] <- { cabin with Persons = newPersons }

                    // Elevator event to continue with next person moving out or in the elevator at current floor
                    this.registerEvent
                        { ElevatorEvent.Clock = clk.addOffset this.B.Durations.moveInDuration
                          CabinIndex = 0
                          Event = EndOpeningDoors }

                    // Person event to record cabin exit for final stats
                    let evt2 =
                        { PersonEvent.Clock = clk.addOffset this.B.Durations.moveInDuration
                          Person =
                            { p with
                                ExitTime = Some(clk.addOffset this.B.Durations.moveInDuration) }
                          Event = ExitCabin }

                    this.Persons.Value.PersonEventsQueue.Enqueue(evt2, evt2.Clock)

                    true // Indicates that a person has moved out, so we shouldn't call allowMoveIn yet

            let allowMoveIn () =
                // If there's still a person on the floor that want to enter, give it 3 seconds to move in
                // First version, ignoring capacity
                let cabin = this.Cabins[0]

                let rec processPersonGoingInSameDirectionAsCabin lst =
                    if List.length cabin.Persons = cabin.Capacity then
                        if doorsJustOpening then this.recordStat clk 0 StatUselessStop
                        false
                    else
                        match lst with
                        | [] -> false // Nobody moved in
                        | p :: remainingPersons ->

                            let personGoesInSameDirectionAsCabin =
                                if cabin.Direction = NoDirection then
                                    true // If cabin has no direction, then let 1st person enter, it will decide on cabin direction
                                else
                                    (cabin.Direction = Up && p.ExitFloor > cabin.Floor)
                                    || (cabin.Direction = Down && p.ExitFloor < cabin.Floor)

                            if (personGoesInSameDirectionAsCabin) then
                                let updatedPerson = { p with EntryTime = Some clk }
                                cabin.setStopRequested p.ExitFloor

                                let newDirection =
                                    if cabin.Direction = NoDirection then
                                        if p.ExitFloor > cabin.Floor then Up else Down
                                    else
                                        cabin.Direction

                                this.Cabins[0] <-
                                    { cabin with
                                        Persons = updatedPerson :: cabin.Persons
                                        Direction = newDirection }

                                this.Landings.setPersons cabin.Floor remainingPersons

                                // Elevator event to continue with next person moving out or in the elevator at current floor
                                this.registerEvent
                                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.moveInDuration
                                      CabinIndex = 0
                                      Event = EndOpeningDoors }

                                true // Indicates that a person moved in, so when it's done, we must check again whether another
                            // person is candidate to move in before starting motor

                            else
                                processPersonGoingInSameDirectionAsCabin remainingPersons

                // Use List.rev to make sure that the person who arrived first on the landing enters first in the cabin
                processPersonGoingInSameDirectionAsCabin (List.rev (this.Landings.getPersons cabin.Floor))

            let cabin = this.Cabins[0]
            assert (cabin.Motor = Off)
            assert (cabin.Door = Opening || cabin.Door = Open)
            assert (cabin.Cabin = Busy)

            if cabin.Door = Opening then
                this.recordStat clk 0 StatDoorsOpen

            this.Cabins[0] <- { cabin with Door = Open }

            if not (allowMoveOut ()) then
                if not (allowMoveIn ()) then
                    // Nobody remaining to move out or move in, we can close the doors
                    this.Cabins[0] <- { cabin with Door = Closing }
                    this.recordStat clk 0 (StatPersonsInCabin(List.length this.Cabins[0].Persons))

                    this.registerEvent
                        { ElevatorEvent.Clock = clk.addOffset this.B.Durations.openingDoorsDuration
                          CabinIndex = 0
                          Event = EndClosingDoors }

        | EndClosingDoors ->
            let cabin = this.Cabins[0]
            assert (cabin.Motor = Off)
            assert (cabin.Door = Closing)
            assert (cabin.Cabin = Busy)

            this.Cabins[0] <- { cabin with Door = Closed }
            this.recordStat clk 0 StatDoorsClosed

            match cabin.Direction with
            | NoDirection ->
                for l in 0 .. this.levels - 1 do
                    assert (not (cabin.getStopRequested (Floor l)))
                    assert ((this.Landings.getPersons (Floor l)).IsEmpty)

                assert (this.Cabins[0].Persons.IsEmpty)

                // Ok, we checked to be sure that nobody is waiting, elevator goes into idle state
                this.Cabins[0] <- { this.Cabins[0] with Cabin = Idle }
                this.recordStat clk 0 StatCabinIdle

            | _ ->
                this.Cabins[0] <-
                    { this.Cabins[0] with
                        Motor = Accelerating }

                this.registerEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.accelerationDuration
                      CabinIndex = 0
                      Event = EndAcceleration }

                this.recordStat clk 0 (StatMotorAccelerating cabin.Floor)

        Logging.logCabinUpdate clk this.B originalCabin this.Cabins[0]


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
                      Event = EndOpeningDoors }

            // Otherwise we start accelerating
            else
                this.recordStat clk 0 StatCabinBusy
                this.recordStat clk 0 (StatMotorAccelerating cabin.Floor)

                this.Cabins[0] <-
                    { cabin with
                        Cabin = Busy
                        Motor = Accelerating
                        Direction = if (entry > cabin.Floor) then Up else Down }

                this.registerEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.accelerationDuration
                      CabinIndex = 0
                      Event = EndAcceleration }

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
                          Event = EndOpeningDoors }

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


    member this.getElevatorsStats() =

        // Register special stat event to make sure final states are cumulated correctly
        let (clk, _) = List.head this.Statistics[0]
        this.recordStat clk 0 StatEndSimulation

        let (Clock duration) = clk
        let elsl = List.rev this.Statistics[0]

        if this.B.LogDetails.showDetailedElevatorStatRecordss then
            for clk, ce in elsl do
                let (Clock iClk) = clk
                printf $"clk: {iClk, 4}  "
                printfn "%0A" ce

        let rec cumulate list (acc: RunningStatus) =
            match list with
            | [] -> acc
            | (clk: Clock, ce: CabinStatistic) :: tail ->
                let newAcc =
                    match ce with
                    | StatMotorOff fl ->
                        assert (clk = Clock 0 || acc.IsMotorOn = true)

                        if clk > Clock 0 then
                            acc.LevelsCovered[acc.PersonsInCabin] <- acc.LevelsCovered[acc.PersonsInCabin] + 1


                        { acc with
                            MotorOnTime = acc.MotorOnTime + (clk.minus acc.LastMotorOn)
                            IsMotorOn = false
                            LastMotorOff = clk }

                    | StatMotorAccelerating fl ->
                        { acc with
                            MotorOffTime = acc.MotorOffTime + (clk.minus acc.LastMotorOff)
                            IsMotorOn = true
                            LastMotorOn = clk }

                    | StatMotorFullSpeed fl ->
                        acc.LevelsCovered[acc.PersonsInCabin] <- acc.LevelsCovered[acc.PersonsInCabin] + 1
                        acc

                    | StatCabinIdle ->
                        assert (clk = Clock 0 || acc.IsCabinBusy = true)

                        { acc with
                            CabinBusyTime = acc.CabinBusyTime + (clk.minus acc.LastCabinBusy)
                            IsCabinBusy = false
                            LastCabinIdle = clk }

                    | StatCabinBusy ->
                        assert (acc.IsCabinBusy = false)

                        { acc with
                            CabinIdleTime = acc.CabinIdleTime + (clk.minus acc.LastCabinIdle)
                            IsCabinBusy = true
                            LastCabinBusy = clk }

                    | StatPersonsInCabin np ->
                        { acc with
                            MaxPersonsInCabin = max acc.MaxPersonsInCabin np
                            PersonsInCabin = np }

                    | StatEndSimulation ->
                        assert (acc.IsMotorOn = false)
                        assert (acc.IsCabinBusy = false)

                        { acc with
                            MotorOffTime = acc.MotorOffTime + (clk.minus acc.LastMotorOff)
                            CabinIdleTime = acc.CabinIdleTime + (clk.minus acc.LastCabinIdle)
                            LastMotorOff = clk }

                    | StatUselessStop ->
                        { acc with UselessStops=acc.UselessStops+1 }

                    | _ -> acc

                cumulate tail newAcc

        let start =
            { LastMotorOn = Clock 0
              LastMotorOff = Clock 0
              IsMotorOn = false
              MotorOnTime = 0
              MotorOffTime = 0

              LastCabinBusy = Clock 0
              LastCabinIdle = Clock 0
              IsCabinBusy = false
              CabinBusyTime = 0
              CabinIdleTime = 0

              UselessStops = 0
              PersonsInCabin = 0
              MaxPersonsInCabin = 0
              LevelsCovered = Array.create (this.Cabins[0].Capacity+1) 0 }

        let final = cumulate elsl start
        assert (duration = final.MotorOnTime + final.MotorOffTime)
        assert (duration = final.CabinBusyTime + final.CabinIdleTime)
        assert (final.PersonsInCabin = 0)

        // Return an ElevatorStats record
        { SimulationDuration = duration
          MotorOnTime = final.MotorOnTime
          MotorOffTime = final.MotorOffTime
          CabinBusyTime = final.CabinBusyTime
          CabinIdleTime = final.CabinIdleTime
          UselessStops = final.UselessStops
          MaxPersonsInCabin = final.MaxPersonsInCabin
          TotalFloorsTraveled = Array.sum final.LevelsCovered
          LevelsCovered = final.LevelsCovered }


    static member printElevatorStats(es: ElevatorsStats) =
        printfn "\nElevator stats"
        printfn "  Simulation duration:       %d" es.SimulationDuration

        printfn
            "  Motor on time:             %d = %.1f%% of simulation"
            es.MotorOnTime
            (100.0 * double es.MotorOnTime / double es.SimulationDuration)

        printfn
            "  Motor off time:            %d = %.1f%% of simulation"
            es.MotorOffTime
            (100.0 * double es.MotorOffTime / double es.SimulationDuration)

        printfn
            "  Cabin busy time:           %d = %.1f%% of simulation"
            es.CabinBusyTime
            (100.0 * double es.CabinBusyTime / double es.SimulationDuration)

        printfn
            "  Cabin idle time:           %d = %.1f%% of simulation"
            es.CabinIdleTime
            (100.0 * double es.CabinIdleTime / double es.SimulationDuration)

        printfn "  Max persons in cabin:      %d" es.MaxPersonsInCabin
        printfn "  Useless stops:             %d" es.UselessStops

        printfn "  Total levels traveled:     %d" es.TotalFloorsTraveled

        for i in 0 .. es.MaxPersonsInCabin do
            if es.LevelsCovered[i] > 0 then
                printfn
                    "    Levels traveled with %d person(s) in cabin: %d = %.1f%% of total"
                    i
                    es.LevelsCovered[i]
                    (100.0 * double es.LevelsCovered[i] / double es.TotalFloorsTraveled)
