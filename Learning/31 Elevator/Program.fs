// 31 Elevator
// Learning F#, Elevator simulation
// Main module, simulations tests and examples
//
// 2024-08-13   PV      First version, only 1 cabin

// "Main" module, Run the simulation
// In charge of master clock progression

System.Console.OutputEncoding <- System.Text.Encoding.UTF8
printfn "Elevator simulation in F#\n"

//let rndPersons = UniformUntRandom.createNew 1
//let mutable rmax = 0
//for i in 1..1000000 do
//    rmax <- max rmax (rndPersons.nextValue())
//printfn "%d" rmax

//System.Diagnostics.Debugger.Break()


let testSimulation10PersonsArrivingTogetherWithCabinCapacity6 () =

    let tenPersonsArrivingAtTimeZero = [|
        { Id = PersonId 1; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 2; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 3; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 4; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 5; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 6; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 7; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 8; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 9; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 10; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
    |]

    // Create DataBag
    let b =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, Clock>()
          SimulationElevators =
            { Levels = 6
              NumberOfCabins = 1
              Capacity = 6 }
          SimulationPersons = SimulationPersonsArray tenPersonsArrivingAtTimeZero
          LogDetails = standardLogDetails
          Durations = standardDurations
          }

    let res = runSimulation b

    assert (res.ElevatorsStats.LevelsCovered[6] = 3) // First elevator move over 3 levels (floor 0->3) with full capacity, 6 persons
    assert (res.ElevatorsStats.LevelsCovered[0] = 3) // Then three levels again with an empty cabin, to go back floor 3->0 to take remaining persons
    assert (res.ElevatorsStats.LevelsCovered[4] = 3) // Final travel over 3 levels too, with 4 remaining persons

    //PersonsActor.printPersonStats res.PersonsStats
    //ElevatorsActor.printElevatorStats res.ElevatorsStats
    //printSimulationStats res.SimulationStats

//testSimulation10PersonsArrivingTogetherWithCabinCapacity6 ()


let testWithAPersonArrivingJustWhenCabinDoorsAreAboutToClose () =

    // Person 2 arrives just when the door is about to close
    // Check that person events are processed before elevator events, so both persons are transported together
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 2; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 4; EntryTime = None; ExitTime = None }
    |]

    let b =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, Clock>()
          SimulationElevators =
            { Levels = 6
              NumberOfCabins = 1
              Capacity = 6 }
          SimulationPersons = SimulationPersonsArray personsData 
          LogDetails = standardLogDetails
          Durations = standardDurations
          }

    let res = runSimulation b

    assert (res.ElevatorsStats.LevelsCovered[2] = 3) // Make sure that the two persons traveled together on 3 levels

    //PersonsActor.printPersonStats res.PersonsStats
    //ElevatorsActor.printElevatorStats res.ElevatorsStats
    //printSimulationStats res.SimulationStats

//testWithAPersonArrivingJustWhenCabinDoorsAreAboutToClose ()


// Just a random simulation
let testARandomSimulation () =
    let b =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, Clock>()
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
          SimulationPersons = SimulationRandomGeneration(1000, 36000, 1, Ground50Levels50) 
          LogDetails = { standardLogDetails with ShowInitialPersons=false }
          Durations = standardDurations
        }

    printSimulationParameters b
    let res = runSimulation b
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    printSimulationStats res.SimulationStats

testARandomSimulation ()


let testContinuousSimulation () =
    let refDataBag =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, Clock>()
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
          SimulationPersons = SimulationRandomGeneration(10, 800, 1, Ground50Levels50) 
          LogDetails = standardLogDetails
          Durations = standardDurations
        }

    printfn "\nContinuous random simulation of variable number of persons over 800s"
    for np in 0..5..130 do
        let b = {refDataBag with SimulationPersons = SimulationRandomGeneration(np, 800, 1, Ground50Levels50) }
        let res = runSimulation b

        printf "p=%3d: wait = %5.1f " np res.PersonsStats.AvgWaitForElevator
        let s = int(res.PersonsStats.AvgWaitForElevator/350.0*80.0+0.5)
        printfn "%*c" s '*'

//testContinuousSimulation ()


let testDoorsClosingWhenAPersonArrives () =
    // Person 2 arrives just when the door is closing, and person 3 1s later
    // Check that the closing door sequence is interrupted (ClosingDoors event is deleted) while both persons are maintained in the list
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 2; EntryFloor = Floor 3; ExitFloor = Floor 0; ArrivalTime = Clock 27; EntryTime = None; ExitTime = None }
        { Id = PersonId 3; EntryFloor = Floor 3; ExitFloor = Floor 0; ArrivalTime = Clock 28; EntryTime = None; ExitTime = None }
    |]

    let b =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, Clock>()
          SimulationElevators =
            { Levels = 6
              NumberOfCabins = 1
              Capacity = 6 }
          SimulationPersons = SimulationPersonsArray personsData 
          LogDetails = { 
              ShowLog = false
              ShowEvents = false
              ShowInitialPersons = false
              ShowDetailedPersonStats = false
              ShowDetailedElevatorStatRecords = false }
          Durations = { 
              AccelerationDuration = 2
              OneLevelFullSpeed = 2
              FullSpeedBeforeDecisionDuration = 1
              OpeningDoorsDuration = 4                  // Need a long closing door for this test
              MoveInDuration = 2 }
          }

    let res = runSimulation b

    assert (res.SimulationStats.SimulationDuration = 58)
    let tp2 = res.TransportedPersons.Find (fun p -> p.Id = PersonId 2)
    let tp3 = res.TransportedPersons.Find (fun p -> p.Id = PersonId 3)
    assert (tp2.EntryTime = Some(Clock 28))
    assert (tp3.EntryTime = Some(Clock 30))

    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    printSimulationStats res.SimulationStats

//testDoorsClosingWhenAPersonArrives ()


printfn "\nDone."
