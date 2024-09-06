// 31 Elevator
// Elevator simulation in F#
// Basic elevator event processing, "business logic" moved to a separate module
//
// 2014-08-15   PV

[<AutoOpen>]
module ElevatorsModule

type ElevatorsActor with

    static member createNew b =
        let cabinInitialState =
            { Floor = Floor.Zero
              MotorStatus = Off
              Direction = NoDirection
              DoorStatus = Closed
              PowerStatus = Idle
              _StopRequested = Array.create b.SimulationElevators.Levels false
              IgnoreNextEndClosingDoorsEvent = false
              Capacity = b.SimulationElevators.Capacity
              Persons = [] }

        let landingInitialState =
            { Landing.Persons = []
              CallUp = false
              CallDown = false }

        { B = b
          Cabins = Array.create b.SimulationElevators.NumberOfCabins cabinInitialState
          Landings = Array.create b.SimulationElevators.Levels landingInitialState
          Persons = None }


    member this.initialize() =
        let cz = Clock.Zero
        //Logging.logMessage this.B cz "Elevator On and ready"
        let cabin = this.Cabins[0]
        assert (this.Cabins[0].MotorStatus = Off)
        assert (this.Cabins[0].DoorStatus = Closed)
        assert (this.Cabins[0].Direction = NoDirection)
        assert (this.Cabins[0].PowerStatus = Idle)
        assert (this.Cabins[0].Floor = Floor.Zero)


    member this.processEvent (clk: Clock) (evt: ElevatorEvent) =

        if this.B.LogDetails.ShowEvents then
            //let evtStr = sprintf "%0A" evt
            let evtStr = evt.ToString()
            //let cabStr = sprintf "%0A" this.Cabins[0]
            let cabStr = this.Cabins[0].ToString()
            logMessage this.B evt.Clock $"Elevator evt: {evtStr}, Cabins[0]: {cabStr}" System.ConsoleColor.Green

        // Keep a deep copy for final logging
        let originalCabin = this.Cabins[0].deepCopy ()
        let (Floor iFloor) = this.Cabins[0].Floor
        let originalLanding = this.Landings[iFloor].deepCopy ()

        match evt.Event with

        | EndAcceleration ->
            assert (this.Cabins[0].MotorStatus = Accelerating)
            assert (this.Cabins[0].DoorStatus = Closed)
            assert (this.Cabins[0].Direction <> NoDirection)
            assert (this.Cabins[0].PowerStatus = Busy)

            this.B.RegisterEvent(
                ElevatorEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.FullSpeedBeforeDecisionDuration
                      CabinIndex = 0
                      Event = Decision
                      CreatedOn = clk }
            )

            this.Cabins[0] <-
                { this.Cabins[0] with
                    MotorStatus = FullSpeed }

            this.B.AddJournalRecord(JournalMotorFullSpeed(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor))

        | Decision ->
            assert (this.Cabins[0].MotorStatus = FullSpeed)
            assert (this.Cabins[0].DoorStatus = Closed)
            assert (this.Cabins[0].Direction <> NoDirection)
            assert (this.Cabins[0].PowerStatus = Busy)

            // Update Position
            let nf = this.Cabins[0].Floor.nextFloor this.levels this.Cabins[0].Direction
            assert (nf.IsSome)
            this.Cabins[0] <- { this.Cabins[0] with Floor = nf.Value }

            // Some business logic to decide whether cabin should start decelerating and stop at this floor, or continue full speed to next floor (and take decision again)
            let decisionEvt = this.getDecisionEvent clk
            this.B.RegisterEvent(ElevatorEvent decisionEvt)

        | EndMovingFullSpeed ->
            assert (this.Cabins[0].MotorStatus = FullSpeed)
            assert (this.Cabins[0].DoorStatus = Closed)
            assert (this.Cabins[0].Direction <> NoDirection)
            assert (this.Cabins[0].PowerStatus = Busy)

            this.B.RegisterEvent(
                ElevatorEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.AccelerationDuration
                      CabinIndex = 0
                      Event = EndDeceleration
                      CreatedOn = clk }
            )

            this.B.AddJournalRecord(JournalMotorDecelerating(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor))

            this.Cabins[0] <-
                { this.Cabins[0] with
                    MotorStatus = Decelerating }

        | EndDeceleration ->
            assert (this.Cabins[0].MotorStatus = Decelerating)
            assert (this.Cabins[0].DoorStatus = Closed)
            assert (this.Cabins[0].Direction <> NoDirection)
            assert (this.Cabins[0].PowerStatus = Busy)

            this.B.AddJournalRecord(JournalMotorOff(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor))

            // Ok, we arrive at a floor with stop requested
            this.B.RegisterEvent(
                ElevatorEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.MotorDelayDuration
                      CabinIndex = 0
                      Event = EndMotorDelay
                      CreatedOn = clk }
            )

            // Clear the stop requested for current floor
            if this.Cabins[0].getStopRequested this.Cabins[0].Floor then
                this.Cabins[0].clearStopRequested this.Cabins[0].Floor

                this.B.AddJournalRecord(
                    JournalCabinClearStopRequested(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor)
                )


            // When motor goes off, cabin direction is updated based only on persons in the cabin
            // - If cabin is empty, or everyone will mouve out at current floor, then set to NoDirection
            // - If there's at leat a person in the cagin going in current cabin direction, keep it, or reverse it

            let oldDirection = this.Cabins[0].Direction

            let newDirection =
                // If cabin is empty, then set cabin direction to NoDirection
                if List.length this.Cabins[0].Persons = 0 then
                    NoDirection
                // If all persons in the cabin exit at current floor, then set cabin direction to NoDirection
                elif not (List.exists (fun p -> p.ExitFloor <> this.Cabins[0].Floor) this.Cabins[0].Persons) then
                    NoDirection
                else if

                    // If cabin is going down, but nobody remaining in cabin go down, then we're going up
                    oldDirection = Down
                then
                    if
                        not (List.exists (fun (p: Person) -> p.ExitFloor < this.Cabins[0].Floor) this.Cabins[0].Persons)
                    then
                        Up
                    else
                        oldDirection

                // If cabin is going up, but nobody remaining in cabin go up, then we're going down
                elif oldDirection = Up then
                    if
                        not (List.exists (fun (p: Person) -> p.ExitFloor > this.Cabins[0].Floor) this.Cabins[0].Persons)
                    then
                        Down
                    else
                        oldDirection

                else
                    oldDirection


            this.Cabins[0] <-
                { this.Cabins[0] with
                    Direction = newDirection
                    MotorStatus = Off }

            if newDirection <> oldDirection then
                this.B.AddJournalRecord(JournalCabinSetDirection(Clock = clk, CabinIndex = 0, Direction = newDirection))


        | EndMotorDelay ->
            assert (this.Cabins[0].MotorStatus = Off)
            assert (this.Cabins[0].DoorStatus = Closed)
            assert (this.Cabins[0].PowerStatus = Busy)

            this.Cabins[0] <-
                { this.Cabins[0] with
                    DoorStatus = Opening }

            this.B.AddJournalRecord(
                JournalCabinDoorsOpenBegin(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor)
            )

            this.B.RegisterEvent(
                ElevatorEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.OpeningDoorsDuration
                      CabinIndex = 0
                      Event = EndOpeningDoors
                      CreatedOn = clk }
            )

        | EndOpeningDoors ->
            let doorsJustOpening = this.Cabins[0].DoorStatus = Opening
            let (Floor iFloor) = this.Cabins[0].Floor

            let allowMoveOut () =
                // If there's still in the cabin a person that needs to get out, then give it 3 seconds to move out
                let ix =
                    List.tryFindIndexBack (fun p -> p.ExitFloor = this.Cabins[0].Floor) this.Cabins[0].Persons

                match ix with
                | None -> false
                | Some i ->
                    let p = this.Cabins[0].Persons[i]
                    let newPersons = List.removeAt i this.Cabins[0].Persons

                    this.Cabins[0] <-
                        { this.Cabins[0] with
                            Persons = newPersons }

                    // Elevator event to continue with next person moving out or in the elevator at current floor
                    this.B.RegisterEvent(
                        ElevatorEvent
                            { ElevatorEvent.Clock = clk.addOffset this.B.Durations.MoveInDuration
                              CabinIndex = 0
                              Event = EndOpeningDoors
                              CreatedOn = clk }
                    )

                    this.B.AddJournalRecord(JournalPersonCabinExitBegin(Clock = clk, CabinIndex = 0, Id = p.Id))

                    // Person event to record cabin exit for final stats
                    let evt2 =
                        { PersonEvent.Clock = clk.addOffset this.B.Durations.MoveInDuration
                          Person =
                            { p with
                                ExitClock = Some(clk.addOffset this.B.Durations.MoveInDuration) }
                          CabinIndex = 0
                          Event = ExitCabin
                          CreatedOn = clk }

                    this.B.RegisterEvent(PersonEvent evt2)

                    true // Indicates that a person has moved out, so we shouldn't call allowMoveIn yet

            let allowMoveIn () =
                // If there's still a person on the floor that want to enter, give it 3 seconds to move in
                let rec processPersonGoingInSameDirectionAsCabin lst =
                    if List.length this.Cabins[0].Persons = this.Cabins[0].Capacity then
                        if doorsJustOpening then
                            this.B.AddJournalRecord(JournalCabinUselessStop(Clock = clk, CabinIndex = 0))

                        false
                    else
                        match lst with
                        | [] -> false // Nobody moved in
                        | p :: remainingPersons ->

                            let personGoesInSameDirectionAsCabin =
                                if this.Cabins[0].Direction = NoDirection then
                                    true // If cabin has no direction, then let 1st person enter, it will decide on cabin direction
                                else
                                    (this.Cabins[0].Direction = Up && p.ExitFloor > this.Cabins[0].Floor)
                                    || (this.Cabins[0].Direction = Down && p.ExitFloor < this.Cabins[0].Floor)

                            if (personGoesInSameDirectionAsCabin) then
                                let updatedPerson = { p with EntryClock = Some clk }

                                this.B.AddJournalRecord(
                                    JournalPersonCabinEnterBegin(Clock = clk, CabinIndex = 0, Id = p.Id)
                                )

                                if not (this.Cabins[0].getStopRequested p.ExitFloor) then
                                    this.Cabins[0].setStopRequested p.ExitFloor

                                    this.B.AddJournalRecord(
                                        JournalCabinSetStopRequested(Clock = clk, CabinIndex = 0, Floor = p.ExitFloor)
                                    )

                                let newDirection =
                                    if this.Cabins[0].Direction = NoDirection then
                                        if p.ExitFloor > this.Cabins[0].Floor then Up else Down
                                    else
                                        this.Cabins[0].Direction

                                if this.Cabins[0].Direction <> newDirection then
                                    this.B.AddJournalRecord(
                                        JournalCabinSetDirection(Clock = clk, CabinIndex = 0, Direction = newDirection)
                                    )

                                // Add person to cabin
                                this.Cabins[0] <-
                                    { this.Cabins[0] with
                                        Persons = updatedPerson :: this.Cabins[0].Persons
                                        Direction = newDirection }

                                // Remove person from landing
                                let lp = this.Landings[iFloor].Persons
                                let ix = lp |> List.findIndex (fun p -> p.Id = updatedPerson.Id) // Will raise an exception if not found

                                this.Landings[iFloor] <-
                                    { this.Landings[iFloor] with
                                        Persons = List.removeAt ix lp }

                                // Automatic update Landing CallUp/CallDown (not exactly the reality, but let's keep things simple)
                                let callUp =
                                    let rec processList lst =
                                        match lst with
                                        | [] -> false
                                        | pl :: tail ->
                                            if pl.ExitFloor > this.Cabins[0].Floor then
                                                true
                                            else
                                                processList tail

                                    processList this.Landings[iFloor].Persons

                                let callDown =
                                    let rec processList lst =
                                        match lst with
                                        | [] -> false
                                        | pl :: tail ->
                                            if pl.ExitFloor < this.Cabins[0].Floor then
                                                true
                                            else
                                                processList tail

                                    processList this.Landings[iFloor].Persons

                                this.Landings[iFloor] <-
                                    { this.Landings[iFloor] with
                                        CallUp = callUp
                                        CallDown = callDown }

                                // Event to add JournalPersonCabinEndEnter into the journal
                                this.B.RegisterEvent(
                                    PersonEvent
                                        { Clock = clk.addOffset (this.B.Durations.MoveInDuration)
                                          Person = updatedPerson
                                          CabinIndex = 0
                                          Event = PersonEventDetail.EndEnterCabin
                                          CreatedOn = clk }
                                )

                                // Elevator event to continue with next person moving out or in the elevator at current floor
                                this.B.RegisterEvent(
                                    ElevatorEvent
                                        { ElevatorEvent.Clock = clk.addOffset this.B.Durations.MoveInDuration
                                          CabinIndex = 0
                                          Event = EndOpeningDoors
                                          CreatedOn = clk }
                                )

                                true // Indicates that a person moved in, so when it's done, we must check again later (once this person has moved in)
                            // whether another person is candidate to move in before starting motor

                            else
                                processPersonGoingInSameDirectionAsCabin remainingPersons

                // Use List.rev to make sure that the person who arrived first on the landing enters first in the cabin
                // This produces a copy contrary to C#
                processPersonGoingInSameDirectionAsCabin (List.rev (this.Landings[iFloor].Persons))

            assert (this.Cabins[0].MotorStatus = Off)
            assert (this.Cabins[0].DoorStatus = Opening || this.Cabins[0].DoorStatus = Open)
            assert (this.Cabins[0].PowerStatus = Busy)

            if this.Cabins[0].DoorStatus = Opening then
                this.B.AddJournalRecord(
                    JournalCabinDoorsOpenEnd(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor)
                )

            this.Cabins[0] <-
                { this.Cabins[0] with
                    DoorStatus = Open }

            if not (allowMoveOut ()) then
                if not (allowMoveIn ()) then
                    // Nobody remaining to move out or move in, we can close the doors
                    this.Cabins[0] <-
                        { this.Cabins[0] with
                            DoorStatus = Closing }

                    this.B.RegisterEvent(
                        ElevatorEvent
                            { ElevatorEvent.Clock = clk.addOffset this.B.Durations.OpeningDoorsDuration
                              CabinIndex = 0
                              Event = EndClosingDoors
                              CreatedOn = clk }
                    )

                    this.B.AddJournalRecord(
                        JournalCabinDoorsCloseBegin(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor)
                    )


        | EndClosingDoors when this.Cabins[0].IgnoreNextEndClosingDoorsEvent ->
            this.Cabins[0] <-
                { this.Cabins[0] with
                    IgnoreNextEndClosingDoorsEvent = false }

        | EndClosingDoors ->
            assert (this.Cabins[0].MotorStatus = Off)
            assert (this.Cabins[0].DoorStatus = Closing)
            assert (this.Cabins[0].PowerStatus = Busy)

            this.Cabins[0] <-
                { this.Cabins[0] with
                    DoorStatus = Closed }

            this.B.AddJournalRecord(
                JournalCabinDoorsCloseEnd(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor)
            )

            match this.Cabins[0].Direction with
            | NoDirection ->
                // Special case, person arriving at current level exactly when doors close -> it has priority
                // We should check if there's someone waiting on another landing
                // In case of multiple waiting, le't chose the one that arrived first

                // Let's handle the special case
                let (Floor iFloor) = this.Cabins[0].Floor

                if List.length this.Landings[iFloor].Persons > 0 then
                    assert (List.forall (fun p -> p.ArrivalClock = clk) this.Landings[iFloor].Persons)

                    // We consider reopening only if cabin is not full
                    if List.length this.Cabins[0].Persons < this.Cabins[0].Capacity then

                        // We reopen only if at least 1 person goes in the same direction as cabin
                        if
                            this.Cabins[0].Direction = NoDirection
                            || (this.Landings[iFloor].Persons
                                |> List.exists (fun p -> this.Cabins[0].Direction = p.goDirection this.Cabins[0].Floor))
                        then

                            // Ok, re-open the door for the lucky guy(s)
                            this.B.RegisterEvent(
                                ElevatorEvent
                                    { Clock = clk.addOffset (this.B.Durations.OpeningDoorsDuration)
                                      CabinIndex = 0
                                      Event = ElevatorEventDetail.EndClosingDoors
                                      CreatedOn = clk }
                            )

                            this.Cabins[0] <-
                                { this.Cabins[0] with
                                    DoorStatus = Opening }

                            this.B.AddJournalRecord(
                                JournalCabinDoorsOpenBegin(Clock = clk, CabinIndex = 0, Floor = this.Cabins[0].Floor)
                            )
                else

                    // We build a list of persons waitin on other landings, so we car sort it by arrival time
                    let rec getWaitingList iFloor listSoFar =
                        if iFloor = this.B.SimulationElevators.Levels then
                            listSoFar
                        else
                            getWaitingList (iFloor + 1) (listSoFar @ this.Landings[iFloor].Persons)

                    let waitingForElevator = getWaitingList 0 []

                    // If noone is waiting, we switch to idle state
                    if List.length waitingForElevator = 0 then
                        this.Cabins[0] <-
                            { this.Cabins[0] with
                                PowerStatus = Idle }

                        this.B.AddJournalRecord(
                            JournalCabinSetPower(Clock = clk, CabinIndex = 0, PowerState = PowerState.Idle)
                        )

                    else
                        // Winner is the one arrived first (with lower Id if multiple arrived at same time)
                        let winner =
                            waitingForElevator |> List.sortBy (fun p -> p.ArrivalClock) |> List.head

                        let newDir =
                            if winner.EntryFloor > this.Cabins[0].Floor then
                                Up
                            else
                                Down

                        this.Cabins[0] <-
                            { this.Cabins[0] with
                                Direction = newDir }

                        this.B.AddJournalRecord(
                            JournalCabinSetDirection(Clock = clk, CabinIndex = 0, Direction = newDir)
                        )

                        // Wait a bit to start acceleration
                        this.B.RegisterEvent(
                            ElevatorEvent
                                { Clock = clk.addOffset (this.B.Durations.MotorDelayDuration)
                                  CabinIndex = 0
                                  Event = ElevatorEventDetail.StartAcceleration
                                  CreatedOn = clk }
                        )

            | _ ->
                // Wait a bit before starting acceleration
                this.B.RegisterEvent(
                    ElevatorEvent
                        { ElevatorEvent.Clock = clk.addOffset this.B.Durations.MotorDelayDuration
                          CabinIndex = 0
                          Event = StartAcceleration
                          CreatedOn = clk }
                )


        | StartAcceleration ->
            assert (this.Cabins[0].MotorStatus = Off)
            assert (this.Cabins[0].DoorStatus = Closed)
            assert (this.Cabins[0].PowerStatus = Busy)

            this.Cabins[0] <-
                { this.Cabins[0] with // Beware, cabin has already been updated...
                    MotorStatus = Accelerating }

            this.B.AddJournalRecord(
                JournalMotorAccelerating(
                    Clock = clk,
                    CabinIndex = 0,
                    Floor = this.Cabins[0].Floor,
                    Direction = this.Cabins[0].Direction
                )
            )

            this.B.RegisterEvent(
                ElevatorEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.AccelerationDuration
                      CabinIndex = 0
                      Event = EndAcceleration
                      CreatedOn = clk }
            )


        Logging.logCabinUpdate this.B clk originalCabin this.Cabins[0]

        // Decision event is special, changes floor, and breaks originalLanging/updatedLanding comparison
        // Anyway, Decision event doesn't update Landing
        if evt.Event <> Decision then
            let (Floor iFloor) = this.Cabins[0].Floor
            let updatedLanding = this.Landings[iFloor]
            Logging.logLandingUpdate this.B clk this.Cabins[0].Floor originalLanding updatedLanding

            if originalLanding.CallUp <> updatedLanding.CallUp then
                if updatedLanding.CallUp then
                    this.B.AddJournalRecord(
                        JournalLandingSetCall(Clock = clk, Floor = this.Cabins[0].Floor, Direction = Up)
                    )
                else
                    this.B.AddJournalRecord(
                        JournalLandingClearCall(
                            Clock = clk,
                            Floor = this.Cabins[0].Floor,
                            Direction = Up
                        )
                    )

            if originalLanding.CallDown <> updatedLanding.CallDown then
                if updatedLanding.CallDown then
                    this.B.AddJournalRecord(
                        JournalLandingSetCall(
                            Clock = clk,
                            Floor = this.Cabins[0].Floor,
                            Direction = Down
                        )
                    )
                else
                    this.B.AddJournalRecord(
                        JournalLandingClearCall(
                            Clock = clk,
                            Floor = this.Cabins[0].Floor,
                            Direction = Down
                        )
                    )

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
        printfn "  Closing doors interrupted: %d" es.ClosingDoorsInterrupted

        printfn "  Total levels traveled:     %d" es.TotalFloorsTraveled

        for i in 0 .. es.MaxPersonsInCabin do
            if es.LevelsCovered[i] > 0 then
                printfn
                    "    Levels traveled with %d person(s) in cabin: %d = %.1f%% of total"
                    i
                    es.LevelsCovered[i]
                    (100.0 * double es.LevelsCovered[i] / double es.TotalFloorsTraveled)
