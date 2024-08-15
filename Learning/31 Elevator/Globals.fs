// 31 Elevator
// Elevator simulation in F#
// Module Globals, global variables
//
// 2014-08-16   PV

[<AutoOpen>]
module Globals

let cabinInitialState =
    {   Floor = Floor 0
        Motor = Off
        Direction = NoDirection
        Door = Closed
        Cabin = Idle
        _StopRequested = Array.create levels false
        //Capacity = 10
        Persons = [] }

let cabins = Array.create numberOfCabins cabinInitialState

let landings = { Landings._Persons = [| for i in 0 .. levels - 1 -> [] |] }


let elevatorQueue =
    new System.Collections.Generic.PriorityQueue<ElevatorEvent, Clock>()

let personEventQueue =
    new System.Collections.Generic.PriorityQueue<PersonEvent, Clock>()
