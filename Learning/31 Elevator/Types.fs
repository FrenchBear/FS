// 31 Elevator
// Elevator simulation in F#
// Module Types, first module loaded, define constants and types
//
// 2014-08-15   PV

[<AutoOpen>]
module Types

// ----------------------------------------------------------------------------
// Simulation parameters

let showLog = false
let showEvents = false
let showInitialPersons = false

let accelerationDuration = 2 // and deceleration duration
let oneLevelFullSpeed = 4
let fullSpeedBeforeDecisionDuration = 1 // and after decision before deceleration
let openingDoorsDuration = 3 // and closing doors duration; Include delay between motor off/opening and closed/motor on
let moveInDuration = 2 // and move out duration

(*
    One level with acceleration, decision, and deceleration: 6s
    -+----
     |   \
    -+-  | Decelerating: 2s
     |   /
    -+-
     |   FullSpeed: 1s
    -+-  Decision point = half level, decide whether we continue full speed or we stop
     |   FullSpeed: 1s
    -+-
     |   \
    -+-  | Accelerating: 2s
     |   /
    -+----

    One level with full speed, from decision point to next decision point: 4s
*)


// ----------------------------------------------------------------------------
// Types

type DataBag =
    { levels: int
      numberOfCabins: int
      personsToCarry: int
      arrivalLength: int
      randomSeed: int }

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
        let (Clock cl) = this
        Clock(cl + offset)

    member this.minus clk =
        let (Clock cl) = this
        let (Clock other) = clk
        cl - other


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
        let (Clock arrival) = this.ArrivalTime
        let (Clock endT) = endTime.Value
        endT - arrival

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
      //Capacity: int
      Persons: Person list }

    member this.getStopRequested floor =
        let (Floor f) = floor
        this._StopRequested[f]

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
      Event: ElevatorEventDetail }



// ----------------------------------------
// Actors

type Elevators =
    { b: DataBag
      elevatorEventsQueue: System.Collections.Generic.PriorityQueue<ElevatorEvent, Clock>
      cabins: Cabin array
      landings: Landings
      mutable persons: Persons option }

    member this.levels = this.b.levels

and

    Persons =
    { b: DataBag
      personEventsQueue: System.Collections.Generic.PriorityQueue<PersonEvent, Clock>
      transportedPersons: System.Collections.Generic.List<Person>
      elevators: Elevators }

    member this.levels = this.b.levels
