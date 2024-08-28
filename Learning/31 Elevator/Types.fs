// 31 Elevator
// Elevator simulation in F#
// Module Types, first module loaded, define constants and types
//
// 2014-08-15   PV

[<AutoOpen>]
module Types

open System

// ----------------------------------------------------------------------------
// Simulation parameters

type LogDetails =
    { ShowLog: bool
      ShowEvents: bool
      ShowInitialPersons: bool
      ShowDetailedPersonStats: bool
      ShowDetailedElevatorStatRecords: bool }

type Durations =
    { AccelerationDuration: int // and deceleration duration
      OneLevelFullSpeed: int
      FullSpeedBeforeDecisionDuration: int // and after decision before deceleration
      OpeningDoorsDuration: int // and closing doors duration; Include delay between motor off/opening and closed/motor on
      MoveInDuration: int } // and move out duration

let standardLogDetails =
    { ShowLog = false
      ShowEvents = false
      ShowInitialPersons = false
      ShowDetailedPersonStats = false
      ShowDetailedElevatorStatRecords = false }

let standardDurations =
    { AccelerationDuration = 2
      OneLevelFullSpeed = 2
      FullSpeedBeforeDecisionDuration = 1
      OpeningDoorsDuration = 2
      MoveInDuration = 2 }

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
    member this.totalTransportationTime() = this.timeSinceArrival this.ExitClock

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

type CabinStatistic =
    | StatCabinIdle
    | StatCabinBusy
    | StatMotorOff
    | StatMotorAccelerating
    | StatMotorFullSpeed
    | StatMotorDecelerating
    | StatDoorsOpen
    | StatDoorsClosed
    | StatClosingDoorsInterrupted
    | StatUselessStop
    | StatPersonsInCabin of int
    | StatEndSimulation

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

type Landings =
    { _Persons: Person list array }

    member this.getPersons(floor: Floor) =
        let (Floor fl) = floor
        this._Persons[fl]

    member this.setPersons (floor: Floor) value =
        let (Floor fl) = floor
        this._Persons[fl] <- value


// ----------------------------------------
// Events

type PersonEventDetail =
    | Arrival
    | ExitCabin

type PersonEvent =
    { Clock: Clock
      Event: PersonEventDetail
      Person: Person
      CreatedOn: Clock }


type ElevatorEventDetail =
    | EndAcceleration
    | EndDeceleration
    | EndMovingFullSpeed
    | Decision
    | EndOpeningDoors
    | EndClosingDoors

type ElevatorEvent =
    { Clock: Clock
      CabinIndex: int
      Event: ElevatorEventDetail
      CreatedOn: Clock }

type CommonEvent =
    | ElevatorEvent of ElevatorEvent
    | PersonEvent of PersonEvent

// ----------------------------------------
// Simulation data

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

(*
printfn "Test ClockPriority"
let cp12_0 = { Clock = Clock 12; Priority = 0 }
let cp23_0 = { Clock = Clock 23; Priority = 0 }
let cp23_1 = { Clock = Clock 23; Priority = 1 }
let cp23_2 = { Clock = Clock 23; Priority = 2 }
let cp30_1 = { Clock = Clock 30; Priority = 1 }

let pq = new System.Collections.Generic.PriorityQueue<String, ClockPriority>()

pq.Enqueue("cp23_2", cp23_2)
pq.Enqueue("cp12_0", cp12_0)
pq.Enqueue("cp30_1", cp30_1)
pq.Enqueue("cp23_0", cp23_0)
pq.Enqueue("cp23_1", cp23_1)

let rec printQueue (q: System.Collections.Generic.PriorityQueue<String, ClockPriority>) =
    if q.Count = 0 then
        ()
    else
        let cp = q.Dequeue()
        printfn "%0A" cp
        printQueue q

printQueue pq
printfn ""
*)

type DataBag =
    { EventsQueue: System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>
      SimulationElevators: SimulationElevators
      SimulationPersons: SimulationPersons
      LogDetails: LogDetails
      Durations: Durations }

    member this.Enqueue (evt:CommonEvent) =
        match evt with
        | ElevatorEvent ee -> this.EventsQueue.Enqueue(evt, {Clock=ee.Clock; Priority=1})
        | PersonEvent pe -> this.EventsQueue.Enqueue(evt, {Clock=pe.Clock; Priority=1})

// ----------------------------------------
// Actors

type ElevatorsActor =
    { B: DataBag
      Cabins: Cabin array
      Statistics: (Clock * CabinStatistic) list array
      Landings: Landings
      mutable Persons: PersonsActor option } // Since Elevators contains a Persons reference, and Persons a Elevators reference, at least one is mutable

    member this.levels = this.B.SimulationElevators.Levels

and

    PersonsActor =
    { B: DataBag
      TransportedPersons: System.Collections.Generic.List<Person>
      Elevators: ElevatorsActor }

// ----------------------------------------
// Simulation Results

type PersonsStats =
    { AvgWaitForElevator: float
      AvgTotalTransport: float
      MaxWaitForElevator: int
      MaxTotalTransport: int }

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
    { SimulationStats: SimulationStats
      ElevatorsStats: ElevatorsStats
      PersonsStats: PersonsStats
      TransportedPersons: System.Collections.Generic.List<Person> }
