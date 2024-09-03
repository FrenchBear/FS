// 31 Elevator
// Elevator simulation in F#
// Module Journal, Full recording or events, replace original Statistics structure
//
// 2024-09-01   PV      Retrofit C# Journal classes

[<AutoOpen>]
module Journal

open System.Linq

let checkJournalAndComputeStatistics (journal: System.Collections.Generic.List<JournalRecord>) showJournal =
    if showJournal then
        printfn "\nValidating journal"

    let mutable doorState = Closed
    let mutable lastDoorUpdateStart = Clock.Zero
    let mutable cabinDirection = NoDir
    let mutable powerState = Idle
    let mutable motorState = Off

    let (sd: SimulationData) =
        match journal[0] with
        | JournalSimulationData(Clock = clk; SD = sd) -> sd
        | _ -> failwith "Internal error"

    let (Clock duration) =
        match journal[journal.Count - 1] with
        | JournalEndSimulationData(Clock = clk) -> clk
        | _ -> failwith "Internal error"

    let personsOpt: Person option array = Array.create sd.PersonsToCarry None
    let personsEndEnterCabin = Array.create sd.PersonsToCarry false
    let personsEndExitCabin = Array.create sd.PersonsToCarry false
    let landingsCallUp = Array.create sd.Levels false
    let landingsCallDown = Array.create sd.Levels false
    let cabinStopRequested = Array.create sd.Levels false

    let mutable recCount = 0

    for record in journal do
        recCount <- recCount + 1

        if showJournal then
            let srec = sprintf "%0A" record
            let p = srec.IndexOf('(')
            printfn $"{srec[.. (p - 1)], -35} {srec[p..]}"

        match record with
        | JournalSimulationData(Clock = clk; SD = sd) -> assert (recCount = 1)

        | JournalEndSimulationData(Clock = clk) -> assert (recCount = journal.Count)

        | JournalPersonArrival(Clock = clk; Id = (PersonId iId); EntryFloor = entryFloor; ExitFloor = exitfloor) ->
            assert (iId > 0 && iId <= sd.PersonsToCarry)
            let pp = personsOpt[iId - 1]
            assert (personsOpt[iId - 1].IsNone)

            personsOpt[iId - 1] <-
                Some
                    { Person.Id = PersonId iId
                      EntryFloor = entryFloor
                      ArrivalClock = clk
                      ExitFloor = exitfloor
                      EntryClock = None
                      ExitClock = None }

        | JournalPersonCabinEnterBegin(Clock = clk; Id = (PersonId iId); CabinIndex = cabinIndex) ->
            assert (iId > 0 && iId <= sd.PersonsToCarry)
            assert (personsOpt[iId - 1].IsSome)

            assert (personsOpt[iId - 1].Value.EntryClock.IsNone)

            personsOpt[iId - 1] <-
                Some
                    { personsOpt[iId - 1].Value with
                        EntryClock = Some clk }

        | JournalPersonCabinEnterEnd(Clock = clk; Id = (PersonId iId); CabinIndex = cabinIndex) ->
            assert (iId > 0 && iId <= sd.PersonsToCarry)
            assert (personsOpt[iId - 1].IsSome)

            let entryClockOpt = personsOpt[iId - 1].Value.EntryClock
            assert (entryClockOpt.IsSome)
            assert (clk.minus (entryClockOpt.Value) = sd.MoveInDuration)
            assert (not personsEndEnterCabin[iId - 1])
            personsEndEnterCabin[iId - 1] <- true

        | JournalPersonCabinExitBegin(Clock = clk; Id = (PersonId iId); CabinIndex = cabinIndex) ->
            assert (iId > 0 && iId <= sd.PersonsToCarry)
            assert (personsOpt[iId - 1].IsSome)

            assert (personsOpt[iId - 1].Value.ExitClock.IsNone)

            personsOpt[iId - 1] <-
                Some
                    { personsOpt[iId - 1].Value with
                        ExitClock = Some clk } // ExitClock temp value

        | JournalPersonCabinExitEnd(Clock = clk; Id = (PersonId iId); CabinIndex = cabinIndex) ->
            assert (iId > 0 && iId <= sd.PersonsToCarry)
            assert (personsOpt[iId - 1].IsSome)

            let exitClockOpt = personsOpt[iId - 1].Value.ExitClock
            assert (exitClockOpt.IsSome)
            assert (clk.minus (exitClockOpt.Value) = sd.MoveInDuration)

            personsOpt[iId - 1] <-
                Some
                    { personsOpt[iId - 1].Value with
                        ExitClock = Some clk } // ExitClock real final value

            assert (not personsEndExitCabin[iId - 1])
            personsEndExitCabin[iId - 1] <- true

        | JournalCabinDoorsOpenBegin(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            assert (doorState = Closed)
            lastDoorUpdateStart <- clk
            doorState <- Opening

        | JournalCabinDoorsOpenEnd(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            assert (doorState = Opening)
            assert (clk.minus (lastDoorUpdateStart) <= sd.OpeningDoorsDuration) // <= because of possible interruption while closing
            doorState <- Open

        | JournalCabinDoorsCloseBegin(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            assert (doorState = Open)
            lastDoorUpdateStart <- clk
            doorState <- Closing

        | JournalCabinDoorsCloseEnd(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            assert (doorState = Closing)
            assert (clk.minus (lastDoorUpdateStart) = sd.OpeningDoorsDuration)
            doorState <- Closed

        | JournalCabinDoorsCloseInterrupt(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            assert (doorState = Closing)
            assert (clk.minus (lastDoorUpdateStart) <= sd.OpeningDoorsDuration)
            lastDoorUpdateStart <- clk
            doorState <- Opening

        | JournalCabinSetDirection(Clock = clk; CabinIndex = cabinIndex; Direction = direction) ->
            assert (cabinDirection <> direction)
            cabinDirection <- direction

        | JournalCabinSetState(Clock = clk; CabinIndex = cabinIndex; PowerState = state) ->
            assert (powerState <> state)
            powerState <- state

        | JournalMotorOff(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            assert (motorState = Decelerating)
            motorState <- Off

        | JournalMotorAccelerating(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            assert (motorState = Off)
            motorState <- Accelerating

        | JournalMotorFullSpeed(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            assert (motorState = Accelerating || motorState = FullSpeed)
            motorState <- FullSpeed

        | JournalMotorDecelerating(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            assert (motorState = FullSpeed)
            motorState <- Decelerating

        | JournalLandingSetCall(Clock = clk; CabinIndex = cabinIndex; Floor = (Floor iFloor); Direction = direction) ->
            if direction = Up then
                assert(not landingsCallUp[iFloor])
                landingsCallUp[iFloor] <- true
            else
                assert(not landingsCallDown[iFloor])
                landingsCallDown[iFloor] <- true

        | JournalLandingClearCall(Clock = clk; CabinIndex = cabinIndex; Floor = (Floor iFloor); Direction = direction) ->
            if direction = Up then
                assert(landingsCallUp[iFloor])
                landingsCallUp[iFloor] <- false
            else
                assert(landingsCallDown[iFloor])
                landingsCallDown[iFloor] <- false

        | JournalCabinSetStopRequested(Clock = clk; CabinIndex = cabinIndex; Floor = (Floor iFloor)) ->
            assert(not cabinStopRequested[iFloor])
            cabinStopRequested[iFloor] <- true

        | JournalCabinClearStopRequested(Clock = clk; CabinIndex = cabinIndex; Floor = (Floor iFloor)) ->
            assert(cabinStopRequested[iFloor])
            cabinStopRequested[iFloor] <- false

        | JournalCabinUselessStop(Clock = clk; CabinIndex = cabinIndex) -> ()

    // Validate final state
    assert (doorState = DoorState.Closed)
    assert (cabinDirection = Direction.NoDir)
    assert (powerState = PowerState.Idle)
    assert (motorState = MotorState.Off)

    // No need to check persons array, if these arrays are all trus, all required data is present
    assert (personsEndEnterCabin |> Array.forall (id))
    assert (personsEndExitCabin |> Array.forall (id))
    assert (landingsCallUp |> Array.forall (fun b -> not b))
    assert (landingsCallDown |> Array.forall (fun b -> not b))
    assert (cabinStopRequested |> Array.forall (fun b -> not b))

    let persons = personsOpt |> Array.map (fun p -> p.Value)

    // Now compute stats

    // Persons stats
    let ArrayMedian (a: double array) : double =
        let ss = a |> Array.sort
        let len = Array.length ss

        if len % 2 = 1 then
            ss[len >>> 1]
        else
            double (ss[(len >>> 1) - 1] + ss[len >>> 1]) / 2.0

    let ArrayAverage95 (a: double array) : double =
        let len = Array.length a

        if len < 20 then
            Array.average a
        else
            let ss = a |> Array.sort
            let ix025 = int (double (len) * 0.025 + 0.5)
            Array.average ss[ix025 .. (len - ix025 - 1)]

    let ArrayMax95 (a: int array) : int =
        let len = Array.length a

        if len < 10 then
            Array.max a
        else
            let ss = a |> Array.sort
            let ix05 = int (double (len) * 0.05 + 0.5)
            ss[len - ix05]


    let avgWaitForElevator =
        if Array.length persons = 0 then
            0.0
        else
            persons
            |> Array.map (fun p -> double (p.waitForElevatorTime ()))
            |> Array.average

    let avg95WaitForElevator =
        if Array.length persons = 0 then
            0.0
        else
            persons
            |> Array.map (fun p -> double (p.waitForElevatorTime ()))
            |> ArrayAverage95

    let medWaitForElevator =
        if Array.length persons = 0 then
            0.0
        else
            persons |> Array.map (fun p -> double (p.waitForElevatorTime ())) |> ArrayMedian

    let maxWaitForElevator =
        if Array.length persons = 0 then
            0
        else
            persons |> Array.map (fun p -> p.waitForElevatorTime ()) |> Array.max

    let max95WaitForElevator =
        if Array.length persons = 0 then
            0
        else
            persons |> Array.map (fun p -> p.waitForElevatorTime ()) |> ArrayMax95


    let avgTotalTransport =
        if Array.length persons = 0 then
            0.0
        else
            persons
            |> Array.map (fun p -> double (p.totalTransportTime ()))
            |> Array.average

    let avg95TotalTransport =
        if Array.length persons = 0 then
            0.0
        else
            persons
            |> Array.map (fun p -> double (p.totalTransportTime ()))
            |> ArrayAverage95

    let medTotalTransport =
        if Array.length persons = 0 then
            0.0
        else
            persons |> Array.map (fun p -> double (p.totalTransportTime ())) |> ArrayMedian

    let maxTotalTransport =
        if Array.length persons = 0 then
            0
        else
            persons |> Array.map (fun p -> p.totalTransportTime ()) |> Array.max

    let max95TotalTransport =
        if Array.length persons = 0 then
            0
        else
            persons |> Array.map (fun p -> p.totalTransportTime ()) |> ArrayMax95


    let ps: PersonsStats =
        { PersonsStats.AvgWaitForElevator = avgWaitForElevator
          Avg95WaitForElevator = avg95WaitForElevator
          MedWaitForElevator = medWaitForElevator
          MaxWaitForElevator = maxWaitForElevator
          Max95WaitForElevator = max95WaitForElevator

          AvgTotalTransport = avgTotalTransport
          Avg95TotalTransport = avg95TotalTransport
          MedTotalTransport = medTotalTransport
          MaxTotalTransport = maxTotalTransport
          Max95TotalTransport = max95TotalTransport }

    // Elevator stats
    let mutable acc: RunningStatus =
        { LastMotorOnClock = Clock.Zero
          LastMotorOffClock = Clock.Zero
          IsMotorOn = false
          MotorOnTime = 0
          MotorOffTime = 0

          LastCabinBusyClock = Clock.Zero
          LastCabinIdleClock = Clock.Zero
          IsCabinBusy = false
          CabinBusyTime = 0
          CabinIdleTime = 0

          UselessStops = 0
          ClosingDoorsInterrupted = 0
          PersonsInCabin = 0
          MaxPersonsInCabin = 0
          LevelsCovered = Array.create (sd.Capacity + 1) 0 }

    for record in journal do
        match record with
        | JournalMotorOff(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            acc <-
                { acc with
                    MotorOnTime = acc.MotorOnTime + clk.minus (acc.LastMotorOnClock)
                    IsMotorOn = false
                    LastMotorOffClock = clk }

        | JournalMotorAccelerating(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            acc <-
                { acc with
                    MotorOffTime = acc.MotorOffTime + clk.minus (acc.LastMotorOffClock)
                    IsMotorOn = true
                    LastMotorOnClock = clk }

        | JournalMotorFullSpeed(Clock = clk; CabinIndex = cabinIndex; Floor = floor) ->
            acc.LevelsCovered[acc.PersonsInCabin] <- acc.LevelsCovered[acc.PersonsInCabin] + 1

        | JournalMotorDecelerating(Clock = clk; CabinIndex = cabinIndex; Floor = floor) -> ()

        | JournalCabinSetState(Clock = clk; CabinIndex = cabinIndex; PowerState = state) ->
            if state = Idle then
                assert (clk = Clock.Zero || acc.IsCabinBusy)

                acc <-
                    { acc with
                        CabinBusyTime = acc.CabinBusyTime + clk.minus (acc.LastCabinBusyClock)
                        IsCabinBusy = false
                        LastCabinIdleClock = clk }
            else
                assert (not acc.IsCabinBusy)

                acc <-
                    { acc with
                        CabinIdleTime = acc.CabinIdleTime + clk.minus (acc.LastCabinIdleClock)
                        IsCabinBusy = true
                        LastCabinBusyClock = clk }

        | JournalPersonCabinEnterEnd(Clock = clk; Id = (PersonId iId); CabinIndex = cabinIndex) ->
            acc <-
                { acc with
                    PersonsInCabin = acc.PersonsInCabin + 1
                    MaxPersonsInCabin = max acc.MaxPersonsInCabin (acc.PersonsInCabin + 1) }

        | JournalPersonCabinExitEnd(Clock = clk; Id = (PersonId iId); CabinIndex = cabinIndex) ->
            acc <-
                { acc with
                    PersonsInCabin = acc.PersonsInCabin - 1 }

        | JournalCabinUselessStop(Clock = clk; CabinIndex = cabinIndex) ->
            acc <-
                { acc with
                    UselessStops = acc.UselessStops + 1 }

        | JournalCabinDoorsCloseInterrupt(Clock = clk; CabinIndex = cabinIndex) ->
            acc <-
                { acc with
                    ClosingDoorsInterrupted = acc.ClosingDoorsInterrupted + 1 }

        | JournalEndSimulationData(Clock = clk) ->
            assert (not acc.IsMotorOn)
            assert (not acc.IsCabinBusy)

            acc <-
                { acc with
                    MotorOffTime = acc.MotorOffTime + clk.minus (acc.LastMotorOffClock)
                    CabinIdleTime = acc.CabinIdleTime + clk.minus (acc.LastCabinIdleClock)
                    LastMotorOffClock = clk }

        | _ -> ()

    assert (duration = acc.MotorOnTime + acc.MotorOffTime)
    assert (duration = acc.CabinBusyTime + acc.CabinIdleTime)
    assert (acc.PersonsInCabin = 0)

    // Return an ElevatorStats record
    let es: ElevatorsStats =
        { SimulationDuration = duration
          MotorOnTime = acc.MotorOnTime
          MotorOffTime = acc.MotorOffTime
          CabinBusyTime = acc.CabinBusyTime
          CabinIdleTime = acc.CabinIdleTime
          UselessStops = acc.UselessStops
          ClosingDoorsInterrupted = acc.ClosingDoorsInterrupted
          MaxPersonsInCabin = acc.MaxPersonsInCabin
          TotalFloorsTraveled = acc.LevelsCovered.Sum()
          LevelsCovered = acc.LevelsCovered }

    sd, ps, es, persons
