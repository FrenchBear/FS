// 31 Elevator
// Elevator simulation in F#
// Simple helpers for ElevatorsActor
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

        { B = b
          Cabins = Array.create b.SimulationElevators.NumberOfCabins cabinInitialState
          Statistics = Array.create b.SimulationElevators.NumberOfCabins []
          Landings = { Landings._Persons = [| for i in 0 .. b.SimulationElevators.Levels - 1 -> [] |] }
          Persons = None }


    member this.recordStat clk ixCabin stat =
        this.Statistics[ixCabin] <- (clk, stat) :: this.Statistics[ixCabin]

    member this.registerEvent evt =
        this.B.EventsQueue.Enqueue(ElevatorEvent evt, evt.Clock)
