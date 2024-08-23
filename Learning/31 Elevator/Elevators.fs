﻿// 31 Elevator
// Elevator simulation in F#
// Basic event processing, "business logic" moved to a separate module
//
// 2014-08-15   PV

[<AutoOpen>]
module ElevatorsModule

type ElevatorsActor with

    member this.initialize() =
        let cz = Clock 0
        Logging.logMessage this.B cz "Elevator On and ready"
        let cabin = this.Cabins[0]
        assert (cabin.MotorStatus = Off)
        assert (cabin.DoorStatus = Closed)
        assert (cabin.Direction = NoDirection)
        assert (cabin.CabinStatus = Idle)
        assert (cabin.Floor = Floor 0)

        this.recordStat cz 0 StatCabinIdle
        this.recordStat cz 0 StatMotorOff
        this.recordStat cz 0 StatDoorsClosed
        this.recordStat cz 0 (StatPersonsInCabin 0)


    member this.processEvent (clk: Clock) (evt: ElevatorEvent) =

        if this.B.LogDetails.ShowEvents then
            logMessage this.B evt.Clock $"Elevator evt: {evt}, cabin: {this.Cabins[0]}"

        // Keep a deep copy for final logging
        let originalCabin = this.Cabins[0].deepCopy ()

        match evt.Event with

        | EndAcceleration ->
            let cabin = this.Cabins[0]
            assert (cabin.MotorStatus = Accelerating)
            assert (cabin.DoorStatus = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.CabinStatus = Busy)

            this.registerEvent
                { ElevatorEvent.Clock = clk.addOffset this.B.Durations.FullSpeedBeforeDecisionDuration
                  CabinIndex = 0
                  Event = Decision
                  CreatedOn = clk }

            this.Cabins[0] <- { cabin with MotorStatus = FullSpeed }

        | Decision ->
            let cabin = this.Cabins[0]
            assert (cabin.MotorStatus = FullSpeed)
            assert (cabin.DoorStatus = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.CabinStatus = Busy)

            // Update Position
            let nf = cabin.Floor.nextFloor this.levels cabin.Direction
            assert (nf.IsSome)
            this.Cabins[0] <- { cabin with Floor = nf.Value }

            // Some business logic to decide whether cabin should start decelerating and stop at this floor, or continue full speed to next floor (and take decision again)
            let decisionEvt = this.getDecisionEvent clk
            this.registerEvent decisionEvt

        | EndMovingFullSpeed ->
            let cabin = this.Cabins[0]
            assert (cabin.MotorStatus = FullSpeed)
            assert (cabin.DoorStatus = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.CabinStatus = Busy)

            this.registerEvent
                { ElevatorEvent.Clock = clk.addOffset this.B.Durations.AccelerationDuration
                  CabinIndex = 0
                  Event = EndDeceleration
                  CreatedOn = clk }

            this.recordStat clk 0 StatMotorDecelerating
            this.Cabins[0] <- { cabin with MotorStatus = Decelerating }

        | EndDeceleration ->
            let cabin = this.Cabins[0]
            assert (cabin.MotorStatus = Decelerating)
            assert (cabin.DoorStatus = Closed)
            assert (cabin.Direction <> NoDirection)
            assert (cabin.CabinStatus = Busy)

            this.recordStat clk 0 StatMotorOff

            // Ok, we arrive at a floor with stop requested
            this.registerEvent
                { ElevatorEvent.Clock = clk.addOffset this.B.Durations.OpeningDoorsDuration
                  CabinIndex = 0
                  Event = EndOpeningDoors
                  CreatedOn = clk }

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

            let checkRequests (floor: Floor) direction =
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
                    DoorStatus = Opening
                    MotorStatus = Off }

        | EndOpeningDoors ->
            let doorsJustOpening = this.Cabins[0].DoorStatus = Opening

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
                        { ElevatorEvent.Clock = clk.addOffset this.B.Durations.MoveInDuration
                          CabinIndex = 0
                          Event = EndOpeningDoors
                          CreatedOn = clk }

                    // Person event to record cabin exit for final stats
                    let evt2 =
                        { PersonEvent.Clock = clk.addOffset this.B.Durations.MoveInDuration
                          Person =
                            { p with
                                ExitClock = Some(clk.addOffset this.B.Durations.MoveInDuration) }
                          Event = ExitCabin
                          CreatedOn = clk }

                    this.B.EventsQueue.Enqueue(PersonEvent evt2, evt2.Clock)

                    true // Indicates that a person has moved out, so we shouldn't call allowMoveIn yet

            let allowMoveIn () =
                // If there's still a person on the floor that want to enter, give it 3 seconds to move in
                let cabin = this.Cabins[0]

                let rec processPersonGoingInSameDirectionAsCabin lst =
                    if List.length cabin.Persons = cabin.Capacity then
                        if doorsJustOpening then
                            this.recordStat clk 0 StatUselessStop

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
                                let updatedPerson = { p with EntryClock = Some clk }
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
                                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.MoveInDuration
                                      CabinIndex = 0
                                      Event = EndOpeningDoors
                                      CreatedOn = clk }

                                true // Indicates that a person moved in, so when it's done, we must check again later (once this person has moved in) 
                                     // whether another person is candidate to move in before starting motor

                            else
                                processPersonGoingInSameDirectionAsCabin remainingPersons

                // Use List.rev to make sure that the person who arrived first on the landing enters first in the cabin
                processPersonGoingInSameDirectionAsCabin (List.rev (this.Landings.getPersons cabin.Floor))

            let cabin = this.Cabins[0]
            assert (cabin.MotorStatus = Off)
            assert (cabin.DoorStatus = Opening || cabin.DoorStatus = Open)
            assert (cabin.CabinStatus = Busy)

            if cabin.DoorStatus = Opening then
                this.recordStat clk 0 StatDoorsOpen

            this.Cabins[0] <- { cabin with DoorStatus = Open }

            if not (allowMoveOut ()) then
                if not (allowMoveIn ()) then
                    // Nobody remaining to move out or move in, we can close the doors
                    this.Cabins[0] <- { cabin with DoorStatus = Closing }
                    this.recordStat clk 0 (StatPersonsInCabin(List.length this.Cabins[0].Persons))

                    this.registerEvent
                        { ElevatorEvent.Clock = clk.addOffset this.B.Durations.OpeningDoorsDuration
                          CabinIndex = 0
                          Event = EndClosingDoors
                          CreatedOn = clk }

        | EndClosingDoors when this.Cabins[0].IgnoreNextEndClosingDoorsEvent ->
            this.Cabins[0] <- { this.Cabins[0] with IgnoreNextEndClosingDoorsEvent=false }

        | EndClosingDoors ->
            let cabin = this.Cabins[0]
            assert (cabin.MotorStatus = Off)
            assert (cabin.DoorStatus = Closing)
            assert (cabin.CabinStatus = Busy)

            this.Cabins[0] <- { cabin with DoorStatus = Closed }
            this.recordStat clk 0 StatDoorsClosed

            match cabin.Direction with
            | NoDirection ->
                for l in 0 .. this.levels - 1 do
                    assert (not (cabin.getStopRequested (Floor l)))
                    assert ((this.Landings.getPersons (Floor l)).IsEmpty)

                assert (this.Cabins[0].Persons.IsEmpty)

                // Ok, we checked to be sure that nobody is waiting, elevator goes into idle state
                this.Cabins[0] <- { this.Cabins[0] with CabinStatus = Idle }
                this.recordStat clk 0 StatCabinIdle

            | _ ->
                this.Cabins[0] <-
                    { this.Cabins[0] with       // Beware, cabin has already been updated...
                        MotorStatus = Accelerating }

                this.registerEvent
                    { ElevatorEvent.Clock = clk.addOffset this.B.Durations.AccelerationDuration
                      CabinIndex = 0
                      Event = EndAcceleration
                      CreatedOn = clk }

                this.recordStat clk 0 StatMotorAccelerating

        Logging.logCabinUpdate clk this.B originalCabin this.Cabins[0]


    member this.getElevatorsStats() =

        // Register special stat event to make sure final states are cumulated correctly
        let (clk, _) = List.head this.Statistics[0]
        this.recordStat clk 0 StatEndSimulation

        let (Clock duration) = clk
        let elsl = List.rev this.Statistics[0]

        if this.B.LogDetails.ShowDetailedElevatorStatRecords then
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
                    | StatMotorOff ->
                        assert (clk = Clock 0 || acc.IsMotorOn = true)

                        if clk > Clock 0 then
                            acc.LevelsCovered[acc.PersonsInCabin] <- acc.LevelsCovered[acc.PersonsInCabin] + 1


                        { acc with
                            MotorOnTime = acc.MotorOnTime + (clk.minus acc.LastMotorOnClock)
                            IsMotorOn = false
                            LastMotorOffClock = clk }

                    | StatMotorAccelerating ->
                        { acc with
                            MotorOffTime = acc.MotorOffTime + (clk.minus acc.LastMotorOffClock)
                            IsMotorOn = true
                            LastMotorOnClock = clk }

                    | StatMotorFullSpeed ->
                        acc.LevelsCovered[acc.PersonsInCabin] <- acc.LevelsCovered[acc.PersonsInCabin] + 1
                        acc

                    | StatCabinIdle ->
                        assert (clk = Clock 0 || acc.IsCabinBusy = true)

                        { acc with
                            CabinBusyTime = acc.CabinBusyTime + (clk.minus acc.LastCabinBusyClock)
                            IsCabinBusy = false
                            LastCabinIdleClock = clk }

                    | StatCabinBusy ->
                        assert (acc.IsCabinBusy = false)

                        { acc with
                            CabinIdleTime = acc.CabinIdleTime + (clk.minus acc.LastCabinIdleClock)
                            IsCabinBusy = true
                            LastCabinBusyClock = clk }

                    | StatPersonsInCabin np ->
                        { acc with
                            MaxPersonsInCabin = max acc.MaxPersonsInCabin np
                            PersonsInCabin = np }

                    | StatEndSimulation ->
                        assert (acc.IsMotorOn = false)
                        assert (acc.IsCabinBusy = false)

                        { acc with
                            MotorOffTime = acc.MotorOffTime + (clk.minus acc.LastMotorOffClock)
                            CabinIdleTime = acc.CabinIdleTime + (clk.minus acc.LastCabinIdleClock)
                            LastMotorOffClock = clk }

                    | StatUselessStop ->
                        { acc with
                            UselessStops = acc.UselessStops + 1 }

                    | StatClosingDoorsInterrupted ->
                        { acc with
                            ClosingDoorsInterrupted = acc.ClosingDoorsInterrupted + 1 }

                    | _ -> acc

                cumulate tail newAcc

        let start =
            { LastMotorOnClock = Clock 0
              LastMotorOffClock = Clock 0
              IsMotorOn = false
              MotorOnTime = 0
              MotorOffTime = 0

              LastCabinBusyClock = Clock 0
              LastCabinIdleClock = Clock 0
              IsCabinBusy = false
              CabinBusyTime = 0
              CabinIdleTime = 0

              UselessStops = 0
              ClosingDoorsInterrupted = 0
              PersonsInCabin = 0
              MaxPersonsInCabin = 0
              LevelsCovered = Array.create (this.Cabins[0].Capacity + 1) 0 }

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
          ClosingDoorsInterrupted = final.ClosingDoorsInterrupted
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
        printfn "  Closing doors interrupted: %d" es.ClosingDoorsInterrupted

        printfn "  Total levels traveled:     %d" es.TotalFloorsTraveled

        for i in 0 .. es.MaxPersonsInCabin do
            if es.LevelsCovered[i] > 0 then
                printfn
                    "    Levels traveled with %d person(s) in cabin: %d = %.1f%% of total"
                    i
                    es.LevelsCovered[i]
                    (100.0 * double es.LevelsCovered[i] / double es.TotalFloorsTraveled)