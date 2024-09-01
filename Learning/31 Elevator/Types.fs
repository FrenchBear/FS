// 31 Elevator
// Elevator simulation in F#
// Module Types, first module loaded, define constants and types
//
// 2024-08-15   PV
// 2024-08-28   PV      ClockPriority as PriorityQueue priority to simplify code

[<AutoOpen>]
module Types

open System

// ----------------------------------------------------------------------------
// Simulation parameters

type LogDetails =
    { ShowLog: bool
      ShowEvents: bool
      ShowInitialPersons: bool
      ShowDetailedPersonsStats: bool
      ShowDetailedElevatorStatRecords: bool }

type Durations =
    { AccelerationDuration: int // and deceleration duration
      OneLevelFullSpeed: int
      FullSpeedBeforeDecisionDuration: int // and after decision before deceleration
      OpeningDoorsDuration: int // and closing doors duration; Include delay between motor off/opening and closed/motor on
      MoveInDuration: int // and move out duration
      MotorDelayDuration: int } // Between motorOff (endDeceleration) and startOpeningDoors, or endClosingDoors and startAccelerating

let standardLogDetails =
    { ShowLog = false
      ShowEvents = false
      ShowInitialPersons = false
      ShowDetailedPersonsStats = false
      ShowDetailedElevatorStatRecords = false }

let standardDurations =
    { AccelerationDuration = 2
      OneLevelFullSpeed = 2
      FullSpeedBeforeDecisionDuration = 1
      OpeningDoorsDuration = 2
      MoveInDuration = 2
      MotorDelayDuration = 1 }

(*
    One level with acceleration, decision, and deceleration: 6s
    -+----
     |   \
    -+-  | Decelerating: accelerationDuration (2s), including extra dead time after cabin stopped before doors open
     |   /
    -+-
     |   FullSpeed: fullSpeedBeforeDecisionDuration (1s)
    -+-  Decision point = half level, decide whether we continue full speed or we stop
     |   FullSpeed: fullSpeedBeforeDecisionDuration (1s)
    -+-
     |   \
    -+-  | Accelerating: accelerationDuration (2s), including dead time after door close before cabin starts moving
     |   /
    -+----

    One level with full speed, from decision point to next decision point: oneLevelFullSpeed (2s)
*)


// ----------------------------------------------------------------------------
// Types

type Direction =
    | NoDirection
    | Up
    | Down

// Use typed Floor rather than int alias for better type checking
type Floor =
    | Floor of int

    member this.nextFloor levels direction =
        let (Floor sf) = this

        match direction with
        | Up -> if sf + 1 < levels then Some(Floor(sf + 1)) else None
        | Down -> if sf > 0 then Some(Floor(sf - 1)) else None
        | NoDirection -> None

    static member Zero = Floor 0


// Use typed Clock rather than int alias for better type checking
type Clock =
    | Clock of int

    member this.addOffset offset =
        let (Clock iClk) = this
        Clock(iClk + offset)

    member this.minus otherClk =
        let (Clock iClk) = this
        let (Clock iOther) = otherClk
        iClk - iOther

    static member Zero = Clock 0

// ----------------------------------------
// Person

type PersonId = PersonId of int

type Person =
    { Id: PersonId
      EntryFloor: Floor
      ArrivalClock: Clock
      ExitFloor: Floor
      EntryClock: Clock option
      ExitClock: Clock option }

    // Calculate difference between Clock? parameter endTime and arrivalTime
    member private this.timeSinceArrival(endClock: Clock option) =
        assert (endClock.IsSome)
        let (Clock iArrival) = this.ArrivalClock
        let (Clock iEndTime) = endClock.Value
        iEndTime - iArrival

    member this.waitForElevatorTime() = this.timeSinceArrival this.EntryClock
    member this.totalTransportTime() = this.timeSinceArrival this.ExitClock

    // For C# comparisons
    override this.ToString() =
        let (PersonId iId) = this.Id

        let sEntryClock =
            match this.EntryClock with
            | None -> ""
            | Some clk -> sprintf "%0A" clk

        let sExitClock =
            match this.ExitClock with
            | None -> ""
            | Some clk -> sprintf "%0A" clk

        $"Person {{ Id = Person {iId}, EntryFloor = {this.EntryFloor}, ArrivalClock = {this.ArrivalClock}, ExitFloor = {this.ExitFloor}, EntryClock = {sEntryClock}, ExitClock = {sExitClock} }}"

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
      MotorStatus: MotorState
      DoorStatus: DoorState
      Direction: Direction
      CabinStatus: CabinState
      _StopRequested: bool array
      IgnoreNextEndClosingDoorsEvent: bool
      Capacity: int
      Persons: Person list }

    // For C# comparisons
    override this.ToString() =
        let strSR =
            "["
            + System.String.Join(", ", this._StopRequested |> Array.map (fun x -> if x then "True" else "False"))
            + "]"

        let strPersons =
            "["
            + System.String.Join(", ", this.Persons |> List.map (fun p -> p.ToString()))
            + "]"

        $"Cabin {{ Floor = {this.Floor}, MotorStatus = {this.MotorStatus}, DoorStatus = {this.DoorStatus}, Direction = {this.Direction}, CabinStatus = {this.CabinStatus}, StopRequested = {strSR}, IgnoreNextEndClosingDoorsEvent = {this.IgnoreNextEndClosingDoorsEvent}, Capacity = {this.Capacity}, Persons = {strPersons} }}"

    member this.getStopRequested floor =
        let (Floor f) = floor
        this._StopRequested[f]

    // StopRequested is only for people inside cabin
    member this.setStopRequested floor =
        let (Floor f) = floor
        this._StopRequested[f] <- true

    member this.clearStopRequested floor =
        let (Floor f) = floor
        this._StopRequested[f] <- false

    // Need a deepCopy because of array, since an array is just a pointer to a mutable structure
    // On the other hand, Persons is an immutable list, so there's no problem with basic record copy
    member this.deepCopy() =
        { this with
            _StopRequested = Array.copy this._StopRequested }


// ----------------------------------------
// Simulation parameters

type RandomPersonsAlgorithm =
    | Ground50Levels50
    | FullRandom

type SimulationElevators =
    { Levels: int
      NumberOfCabins: int
      Capacity: int }

type SimulationPersons =
    | SimulationPersonsArray of Person array
    | SimulationRandomGeneration of
        personsToCarry: int *
        arrivalLength: int *
        randomSeed: int *
        algorithm: RandomPersonsAlgorithm

    member this.getPersonsToCarry =
        match this with
        | SimulationPersonsArray pa -> Array.length pa
        | SimulationRandomGeneration(personsToCarry, arrivalLength, randomSeed, algorithm) -> personsToCarry


// ----------------------------------------
// Simulation Results

type SimulationData =
    {
      // Elevators/Building
      Levels: int
      NumberOfCabins: int
      Capacity: int

      // Persons
      FixedPersonsList: bool
      PersonsToCarry: int
      ArrivalLength: int option
      Algorithm: RandomPersonsAlgorithm option
      RandomSeed: int option

      // Durations
      AccelerationDuration: int
      OneLevelFullSpeed: int
      FullSpeedBeforeDecisionDuration: int
      OpeningDoorsDuration: int
      MoveInDuration: int
      MotorDelayDuration: int }

type PersonsStats =
    { AvgWaitForElevator: float
      Avg95WaitForElevator: float
      MedWaitForElevator: float
      MaxWaitForElevator: int
      Max95WaitForElevator: int

      AvgTotalTransport: float
      Avg95TotalTransport: float
      MedTotalTransport: float
      MaxTotalTransport: int
      Max95TotalTransport: int }

type ElevatorsStats =
    { SimulationDuration: int
      MotorOnTime: int
      MotorOffTime: int
      CabinBusyTime: int
      CabinIdleTime: int
      UselessStops: int
      ClosingDoorsInterrupted: int
      MaxPersonsInCabin: int
      TotalFloorsTraveled: int
      LevelsCovered: int array }

type SimulationStats =
    { SimulationDuration: int
      SimulationRealTimeDuration: float
      SimulationEventsCount: int }

type SimulationResult =
    { SimulationData: SimulationData
      ElevatorsStats: ElevatorsStats
      PersonsStats: PersonsStats
      TransportedPersons: Person array
      SimulationStats: SimulationStats }


// ----------------------------------------
// Journal

type JournalRecord =
    | JournalSimulationData of Clock: Clock * SD: SimulationData
    | JournalEndSimulationData of Clock: Clock

    | JournalCabinDoorsOpenBegin of Clock: Clock * CabinIndex: int * Floor: Floor
    | JournalCabinDoorsOpenEnd of Clock: Clock * CabinIndex: int * Floor: Floor
    | JournalCabinDoorsCloseBegin of Clock: Clock * CabinIndex: int * Floor: Floor
    | JournalCabinDoorsCloseEnd of Clock: Clock * CabinIndex: int * Floor: Floor
    | JournalCabinDoorsCloseInterrupt of Clock: Clock * CabinIndex: int * Floor: Floor

    | JournalCabinUselessStop of Clock: Clock * CabinIndex: int
    | JournalCabinSetDirection of Clock: Clock * CabinIndex: int * Direction: Direction
    | JournalCabinSetState of Clock: Clock * CabinIndex: int * CabinState: CabinState

    | JournalCabinSetStopRequested of Clock: Clock * CabinIndex: int * Floor: Floor
    | JournalCabinClearStopRequested of Clock: Clock * CabinIndex: int * Floor: Floor

    | JournalLandingSetCall of Clock: Clock * CabinIndex: int * Floor: Floor * Direction: Direction
    | JournalLandingClearCall of Clock: Clock * CabinIndex: int * Floor: Floor * Direction: Direction

    | JournalPersonArrival of Clock: Clock * Id: PersonId * EntryFloor: Floor * ExitFloor: Floor

    | JournalPersonCabinEnterBegin of Clock: Clock * Id: PersonId * CabinIndex: int
    | JournalPersonCabinEnterEnd of Clock: Clock * Id: PersonId * CabinIndex: int
    | JournalPersonCabinExitBegin of Clock: Clock * Id: PersonId * CabinIndex: int
    | JournalPersonCabinExitEnd of Clock: Clock * Id: PersonId * CabinIndex: int

    | JournalMotorAccelerating of Clock: Clock * CabinIndex: int * Floor: Floor * Direction: Direction
    | JournalMotorFullSpeed of Clock: Clock * CabinIndex: int * Floor: Floor
    | JournalMotorDecelerating of Clock: Clock * CabinIndex: int * Floor: Floor
    | JournalMotorOff of Clock: Clock * CabinIndex: int * Floor: Floor


type RunningStatus =
    { LastMotorOnClock: Clock
      LastMotorOffClock: Clock
      IsMotorOn: bool
      MotorOnTime: int
      MotorOffTime: int

      LastCabinBusyClock: Clock
      LastCabinIdleClock: Clock
      IsCabinBusy: bool
      CabinBusyTime: int
      CabinIdleTime: int

      UselessStops: int
      ClosingDoorsInterrupted: int
      PersonsInCabin: int
      MaxPersonsInCabin: int
      LevelsCovered: int array }

// ----------------------------------------
// Landings

type Landing =
    { Persons: Person List
      CallUp: bool
      CallDown: bool }

    member this.deepCopy() =
        { this with
            Persons = List.map id this.Persons }

// ----------------------------------------
// Events

type PersonEventDetail =
    | Arrival
    | EndEnterCabin
    | ExitCabin

type PersonEvent =
    { Clock: Clock
      Event: PersonEventDetail
      Person: Person
      CabinIndex: int
      CreatedOn: Clock }

    // For C# comparisons
    override this.ToString() =
        $"PersonEvent {{ Clock = {this.Clock}, Event = {this.Event}, Person = {this.Person}, CabinIndex = {this.CabinIndex}, CreatedOn = {this.CreatedOn} }}"



type ElevatorEventDetail =
    | StartAcceleration
    | EndAcceleration
    | Decision
    | EndMovingFullSpeed
    | EndDeceleration
    | EndMotorDelay
    | EndOpeningDoors
    | EndClosingDoors

type ElevatorEvent =
    { Clock: Clock
      Event: ElevatorEventDetail
      CabinIndex: int
      CreatedOn: Clock }

    // For C# comparisons
    override this.ToString() =
        $"ElevatorEvent {{ Clock = {this.Clock}, Event = {this.Event}, CabinIndex = {this.CabinIndex}, CreatedOn = {this.CreatedOn} }}"


type CommonEvent =
    | ElevatorEvent of ElevatorEvent
    | PersonEvent of PersonEvent

// Type used for PriorityQueue priority; it's clock + [priority 0 (higher) for persons or priority 1 for elevators]
// So scheduler will get directly next event with lowest value of clock and priority
[<CustomEquality; CustomComparison>]
type ClockPriority =
    { Clock: Clock
      Priority: int }

    interface IEquatable<ClockPriority> with
        member this.Equals other =
            this.Clock.Equals other.Clock && this.Priority.Equals other.Priority

    override this.Equals other =
        match other with
        | :? ClockPriority as p -> (this :> IEquatable<_>).Equals p
        | _ -> false

    override this.GetHashCode() =
        this.Clock.GetHashCode() ^^^ this.Priority.GetHashCode()


    interface IComparable<ClockPriority> with
        member this.CompareTo other =
            let (Clock iClkThis) = this.Clock
            let (Clock iClkOther) = other.Clock
            let res = iClkThis.CompareTo iClkOther
            // Compare on clock first, and if clocks are the same, compare on priority
            if res = 0 then
                this.Priority.CompareTo other.Priority
            else
                res

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? ClockPriority as p -> (this :> IComparable<_>).CompareTo p
            | _ -> -1

type DataBag =
    { 
      SimulationElevators: SimulationElevators
      SimulationPersons: SimulationPersons
      LogDetails: LogDetails
      Durations: Durations 

      EventsQueue: System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>
      Journal: System.Collections.Generic.List<JournalRecord>
    }

    member this.RegisterEvent evt =
        match evt with
        | ElevatorEvent ee -> this.EventsQueue.Enqueue(evt, { Clock = ee.Clock; Priority = 1 })
        | PersonEvent pe -> this.EventsQueue.Enqueue(evt, { Clock = pe.Clock; Priority = 0 }) // Higher priority

    member this.AddJournalRecord record =
        this.Journal.Add(record)

// ----------------------------------------
// Actors

type ElevatorsActor =
    { B: DataBag
      Cabins: Cabin array
      Landings: Landing array
      mutable Persons: PersonsActor option } // Since Elevators contains a Persons reference, and Persons a Elevators reference, at least one is mutable

    member this.levels = this.B.SimulationElevators.Levels

and

    PersonsActor =
    { B: DataBag
      TransportedPersons: System.Collections.Generic.List<Person>
      Elevators: ElevatorsActor }

