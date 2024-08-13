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
    | Up
    | Down
    | Accelerating
    | Decelerating

type DoorState =
    | Open
    | Closed
    | Opening
    | Closing

type Cabin =
    { StateTime: int
      Position: float
      Motor: MotorState
      Door: DoorState
      StopRequested: bool[]
      Capacity: int
      Persons: Person list }


let levels = 6 // 0=Ground, and levels 1..5
let landings = Array.create levels { Landing.Persons = [] }

let cabin =
    { StateTime = 0
      Position = 0
      Motor = Stop
      Door = Closed
      StopRequested = Array.create levels false
      Capacity = 10
      Persons = [] }


module PersonsModule =
    let personsToCarry = 15
    let arrivalLength = 5 * 60
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

    let personsArray = [| for _ in 1..personsToCarry -> getRandomPerson () |]
    let sortedPersonsArray = Array.sortBy (fun p -> p.ArrivalTime) personsArray
    for p in sortedPersonsArray do
        printfn "%0A" p
    printfn ""

    (* Could use a priority queue...
    let personsQueue = new System.Collections.Generic.PriorityQueue<Person, Clock>()
    for i in 0..personsToCarry-1 do
        //let p = getRandomPerson()
        let p = personsArray[i]
        personsQueue.Enqueue(p, p.ArrivalTime)

    for i in 1..personsToCarry do
        let p=personsQueue.Dequeue()
        printfn "%0A" p
    *)

    let mutable currentIndex = 0

    let getNextEventClock() =
        if currentIndex=personsToCarry
        then None
        else Some (sortedPersonsArray[currentIndex].ArrivalTime)

    