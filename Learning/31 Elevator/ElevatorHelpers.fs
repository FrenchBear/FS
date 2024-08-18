// 31 Elevator
// Elevator simulation in F#
// Few helpers for ElevatorsActor
//
// 2014-08-15   PV

[<AutoOpen>]
module ElevatorHelpers

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

    member this.recordStat clk ixCabin stat =
        this.Statistics[ixCabin] <- (clk, stat) :: this.Statistics[ixCabin]

    member this.registerEvent evt =
        this.ElevatorEventsQueue.Enqueue(evt, evt.Clock)
