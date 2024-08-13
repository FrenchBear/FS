// 31 Ascenseur
// Learning F#
//
// 2024-08-13   PV

type Floor = int
type Clock = int

type Person =
    { EntryLevel: Floor
      ArrivalTime: Clock
      EntryTime: Clock
      ExitLevel: Floor
      ExitTime: Clock }

type Landing = { Persons: Person list }

type MotorState =
    | Stop
    | Moving
    | Accelerating
    | Decelerating

type DoorState =
    | Open
    | Closed
    | Opening
    | Closing

type CabinState =
    | Idle
    | OpeningDoors
    | DoorsOpen
    | ClosingDoors
    | Moving

type DirectionState =
    | NoDirection
    | Up
    | Down

type Cabin =
    { StateTime: int
      Position: Floor
      Motor: MotorState
      Door: DoorState
      Direction: DirectionState
      Cabin: CabinState
      StopRequested: bool[]
      //Capacity: int
      Persons: Person list }


let levels = 6 // 0=Ground, and levels 1..5
let landings = Array.create levels { Landing.Persons = [] }

let cabinInitialState =
    { StateTime = 0
      Position = 0
      Motor = Stop
      Direction = NoDirection
      Door = Closed
      Cabin = Idle
      StopRequested = Array.create levels false
      //Capacity = 10
      Persons = [] }

let numberOfCabins = 1
let cabins = Array.create numberOfCabins cabinInitialState


type PersonEventDetail =
    | Arrival of Person

type PersonEvent =
    { Clock: int
      Event: PersonEventDetail 
    }


type ElevatorEventDetail =
    | ElevatorOn
    | EndAcceleration
    | EndDeceleration
    | EndMovingFullSpeed
    | Decision
    | EndOpeningDoors

type ElevatorEvent =
    { Clock: int
      Event: ElevatorEventDetail }

module ElevatorModule =
    let elevatorQueue =
        new System.Collections.Generic.PriorityQueue<ElevatorEvent, Clock>()

    let evt =
        { ElevatorEvent.Clock = 0
          Event = ElevatorOn }

    elevatorQueue.Enqueue(evt, evt.Clock)

    let getNextElevatorEventClock () =
        if elevatorQueue.Count = 0 then
            None
        else
            let evt = elevatorQueue.Peek()
            Some evt.Clock

    let processEvent clk =
        let evt = elevatorQueue.Dequeue()
        printfn "Evevator.processEvent evt=%0A" evt
        assert (clk = evt.Clock)

        match evt.Event with
        | ElevatorOn -> 
            printfn "Elevator On and ready"
        
        | EndAcceleration ->
            let evt =
                { ElevatorEvent.Clock = clk + 2
                  Event = Decision }
            elevatorQueue.Enqueue(evt, evt.Clock)

        | Decision ->
            // Update Position
            let cabin = cabins[0]
            let newCabin = { cabin with Position = cabin.Position + if cabin.Direction=Up then 1 else -1 }
            cabins[0] <- newCabin

            // Decide if we stop at next floor or not
            let evt =
                if cabin.StopRequested[cabin.Position] then
                    { ElevatorEvent.Clock = clk + 1
                      Event = EndMovingFullSpeed }
                else
                    { ElevatorEvent.Clock = clk + 4
                      Event = Decision }
            elevatorQueue.Enqueue(evt, evt.Clock)

        | EndMovingFullSpeed ->
            let evt =
                { ElevatorEvent.Clock = clk + 2
                  Event = EndDeceleration }
            elevatorQueue.Enqueue(evt, evt.Clock)

        | EndDeceleration ->
            // Ok, we arrive at a floor with stop requested
            let evt =
                { ElevatorEvent.Clock = clk + 2
                  Event = EndOpeningDoors }
            elevatorQueue.Enqueue(evt, evt.Clock)

        | EndOpeningDoors ->
            // ToDo
            ()

    let callElevator clk entry exit =
        printfn "Calling elevator from level %d to go to level %d" entry exit
        assert (exit <> entry)
        let cabin = cabins[0]

        if cabin.Cabin = Idle then
            assert (cabin.Door = Closed)
            assert (cabin.Motor = Stop)
            cabin.StopRequested[exit] <- true

            let newCabin = 
                { cabin with
                    Cabin = Moving
                    Motor = Accelerating
                    Direction = if (entry>exit) then Up else Down
                }

            cabins[0] <- newCabin
            let evt =
                { ElevatorEvent.Clock = clk + 2
                  Event = EndAcceleration
                }

            elevatorQueue.Enqueue(evt, evt.Clock)



module PersonModule =
    let personsToCarry = 1
    let arrivalLength = 60
    let rndPersons = new System.Random(1)

    let getRandomPerson () =
        let entry, exit =
            if rndPersons.Next(2) = 0 then
                0, rndPersons.Next(1, levels)
            else
                rndPersons.Next(1, levels), 0

        let arrival = rndPersons.Next(arrivalLength)

        { EntryLevel = entry
          ExitLevel = exit
          ArrivalTime = arrival
          EntryTime = 0
          ExitTime = 0 }

    let personQueue = new System.Collections.Generic.PriorityQueue<PersonEvent, Clock>()

    for i in 0 .. personsToCarry - 1 do
        let p = getRandomPerson ()
        let evt = { PersonEvent.Clock = p.ArrivalTime 
                    Event = Arrival p
                  }
        personQueue.Enqueue(evt, evt.Clock)

    let peekNextPersonEvent () =
        if personQueue.Count = 0 then
            None
        else
            let evt = personQueue.Peek()
            Some(evt.Clock)

    let processEvent clk =
        let evt = personQueue.Dequeue()
        printfn "Evevator.processEvent evt=%0A" evt
        assert (clk = evt.Clock)

        match evt.Event with
        | Arrival p ->
            let prevList = landings[p.EntryLevel].Persons
            landings[p.EntryLevel] <- { Persons = p :: prevList }
            ElevatorModule.callElevator clk p.EntryLevel p.ExitLevel


// Runs the simulation
// In charge or master clock
// Find next event in line, process it, and iterates until there are no more events to process
module SchedulerModule =

    let rec processNextEvent clk =
        let nextPersonEventClock = PersonModule.peekNextPersonEvent ()
        let nextElevatorEventClock = ElevatorModule.getNextElevatorEventClock ()
        printfn "Scheduler.processNextEvent clk=%d pe=%0A ee=%0A" clk nextPersonEventClock nextElevatorEventClock

        let nextClock =
            match nextPersonEventClock, nextElevatorEventClock with
            | None, None -> -1
            | None, Some elevatorNextClock ->
                assert (elevatorNextClock >= clk)
                ElevatorModule.processEvent elevatorNextClock
                elevatorNextClock
            | Some personNextClock, None ->
                assert (personNextClock >= clk)
                PersonModule.processEvent personNextClock
                personNextClock
            | Some personNextClock, Some elevatorNextClock when personNextClock > elevatorNextClock ->
                assert (elevatorNextClock >= clk)
                ElevatorModule.processEvent elevatorNextClock
                elevatorNextClock
            | Some personNextClock, Some elevatorNextClock when personNextClock < elevatorNextClock ->
                assert (personNextClock >= clk)
                PersonModule.processEvent personNextClock
                personNextClock
            | Some personNextClock, Some elevatorNextClock ->
                assert (personNextClock = elevatorNextClock)
                assert (personNextClock >= clk)
                PersonModule.processEvent personNextClock
                ElevatorModule.processEvent elevatorNextClock
                personNextClock

        match nextClock with
        | -1 -> printfn "Fin de la simulation clk=%d" clk
        | _ -> processNextEvent nextClock

    processNextEvent 0
