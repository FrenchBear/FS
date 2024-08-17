// 31 Elevator
// Elevator simulation in F#
// Module Types, first module loaded, define constants and types
//
// 2014-08-15   PV

[<AutoOpen>]
module Types

// ----------------------------------------------------------------------------
// Simulation parameters

type LogDetails =
    { showLog: bool
      showEvents: bool
      showInitialPersons: bool
      showDetailedPersonStats: bool
      showDetailedElevatorStatRecordss: bool }

type Durations =
    { accelerationDuration: int // and deceleration duration
      oneLevelFullSpeed: int
      fullSpeedBeforeDecisionDuration: int // and after decision before deceleration
      openingDoorsDuration: int // and closing doors duration; Include delay between motor off/opening and closed/motor on
      moveInDuration: int } // and move out duration

let standardLogDetails =
    { showLog = false
      showEvents = false
      showInitialPersons = false
      showDetailedPersonStats = false
      showDetailedElevatorStatRecordss = false }

let standardDurations =
    { accelerationDuration = 2
      oneLevelFullSpeed = 2
      fullSpeedBeforeDecisionDuration = 1
      openingDoorsDuration = 2
      moveInDuration = 2 }

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
      ArrivalTime: Clock
      ExitFloor: Floor
      EntryTime: Clock option
      ExitTime: Clock option }

    member private this.calcTime(endTime: Clock option) =
        assert (endTime.IsSome)
        let (Clock iArrival) = this.ArrivalTime
        let (Clock iEndTime) = endTime.Value
        iEndTime - iArrival

    member this.waitForElevator() = this.calcTime this.EntryTime
    member this.totalTransportation() = this.calcTime this.ExitTime

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
      Motor: MotorState
      Door: DoorState
      Direction: Direction
      Cabin: CabinState
      _StopRequested: bool array
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
    | StatMotorOff of Floor
    | StatMotorAccelerating of Floor
    | StatMotorFullSpeed of Floor
    | StatMotorDecelerating
    | StatDoorsOpen
    | StatDoorsClosed
    | StatUselessStop
    | StatPersonsInCabin of int
    | StatEndSimulation

type RunningStatus =
    { LastMotorOn: Clock
      LastMotorOff: Clock
      IsMotorOn: bool
      MotorOnTime: int
      MotorOffTime: int

      LastCabinBusy: Clock
      LastCabinIdle: Clock
      IsCabinBusy: bool
      CabinBusyTime: int
      CabinIdleTime: int

      UselessStops: int
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
    { Clock: Clock
      CabinIndex: int
      Event: ElevatorEventDetail }


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

type DataBag =
    { SimulationElevators: SimulationElevators
      SimulationPersons: SimulationPersons
      LogDetails: LogDetails
      Durations: Durations }


// ----------------------------------------
// Actors

type ElevatorsActor =
    { B: DataBag
      ElevatorEventsQueue: System.Collections.Generic.PriorityQueue<ElevatorEvent, Clock>
      Cabins: Cabin array
      Statistics: (Clock * CabinStatistic) list array
      Landings: Landings
      mutable Persons: PersonsActor option } // Since Elevators contains a Persons reference, and Persons a Elevators reference, at least one is mutable

    member this.levels = this.B.SimulationElevators.Levels

and

    PersonsActor =
    { B: DataBag
      PersonEventsQueue: System.Collections.Generic.PriorityQueue<PersonEvent, Clock>
      TransportedPersons: System.Collections.Generic.List<Person>
      Elevators: ElevatorsActor }

    member this.levels = this.B.SimulationElevators.Levels


// ----------------------------------------
// Simulation Results

// ToDo: Add median values, and max for 95% of users
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
      PersonsStats: PersonsStats }
