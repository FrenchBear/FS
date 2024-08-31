// 31 Elevator
// Learning F#, Elevator simulation
// Main module, simulations tests and examples
//
// 2024-08-13   PV      First version
//
// Various tests



let testSimple1 () =
    printfn "\n---------------------------------------\nTest Simple #1\n"

    let persone = [|
        { Id = PersonId 1; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock 10; EntryClock = None; ExitClock = None }
    |]

    // Create DataBag
    let b =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          SimulationElevators =
            { Levels = 6
              NumberOfCabins = 1
              Capacity = 6 }
          SimulationPersons = SimulationPersonsArray persone
          LogDetails = { standardLogDetails with ShowLog = true }
          //LogDetails = { 
          //    ShowLog = true
          //    ShowEvents = false
          //    ShowInitialPersons = false
          //    ShowDetailedPersonsStats = false
          //    ShowDetailedElevatorStatRecords = false }
          Durations = standardDurations
          }

    let res = runSimulation b

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats



let testSimulation10PersonsArrivingTogetherWithCabinCapacity6 () =
    printfn "\n---------------------------------------\nTest 10 persons arriving together with cabin capacity 6\n"

    let tenPersonsArrivingAtTimeZero = [|
        { Id = PersonId 1; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 2; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 3; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 4; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 5; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 6; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 7; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 8; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 9; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 10; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
    |]

    // Create DataBag
    let b =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          SimulationElevators =
            { Levels = 6
              NumberOfCabins = 1
              Capacity = 6 }
          SimulationPersons = SimulationPersonsArray tenPersonsArrivingAtTimeZero
          LogDetails = { standardLogDetails with ShowLog = true }
          Durations = standardDurations
          }

    let res = runSimulation b

    assert (res.ElevatorsStats.LevelsCovered[6] = 3) // First elevator move over 3 levels (Floor.Zero->3) with full capacity, 6 persons
    assert (res.ElevatorsStats.LevelsCovered[0] = 3) // Then three levels again with an empty cabin, to go back floor 3->0 to take remaining persons
    assert (res.ElevatorsStats.LevelsCovered[4] = 3) // Final travel over 3 levels too, with 4 remaining persons

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats



let testPersonArrivingJustWhenCabinDoorsAreAboutToClose () =
    printfn "\n---------------------------------------\nTest Person arriving just when cabin doors are about to close\n"

    // Person 2 arrives just when the door is about to close
    // Check that person events are processed before elevator events, so both persons are transported together
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 2; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock 4; EntryClock = None; ExitClock = None }
    |]

    let b =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          SimulationElevators =
            { Levels = 6
              NumberOfCabins = 1
              Capacity = 6 }
          SimulationPersons = SimulationPersonsArray personsData 
          LogDetails = { standardLogDetails with ShowLog = true }
          Durations = standardDurations
          }

    let res = runSimulation b

    assert (res.ElevatorsStats.LevelsCovered[2] = 3) // Make sure that the two persons traveled together on 3 levels

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats


let testDoorsClosingWhenAPersonArrives () =
    printfn "\n---------------------------------------\nTest Doors closing when a person arrives\n"

    // Person 2 arrives just when the door is closing, and person 3 1s later
    // Check that the closing door sequence is interrupted (ClosingDoors event is deleted) while both persons are maintained in the list
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 2; EntryFloor = Floor 3; ExitFloor = Floor.Zero; ArrivalClock = Clock 27; EntryClock = None; ExitClock = None }
        { Id = PersonId 3; EntryFloor = Floor 3; ExitFloor = Floor.Zero; ArrivalClock = Clock 28; EntryClock = None; ExitClock = None }
    |]

    let b =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          SimulationElevators =
            { Levels = 6
              NumberOfCabins = 1
              Capacity = 6 }
          SimulationPersons = SimulationPersonsArray personsData 
          LogDetails = { standardLogDetails with ShowLog = true }
          Durations = { 
              AccelerationDuration = 2
              OneLevelFullSpeed = 2
              FullSpeedBeforeDecisionDuration = 1
              OpeningDoorsDuration = 4                  // Need a long closing door for this test
              MoveInDuration = 2 
              MotorDelayDuration = 1 }
          }

    let res = runSimulation b

    assert (res.SimulationStats.SimulationDuration = 60)
    let tp2 = res.TransportedPersons.Find (fun p -> p.Id = PersonId 2)
    let tp3 = res.TransportedPersons.Find (fun p -> p.Id = PersonId 3)
    assert (tp2.EntryClock = Some(Clock 28))
    assert (tp3.EntryClock = Some(Clock 30))

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats


let testPersonsGoingUpAndDownFromSameFloor () =
    printfn "\n---------------------------------------\nTest Persons going up and down from same floor\n"

    // All persons arrive on 2nd floor, Persons 1 and 3 go to Floor.Zero, Persons 2 and 4 go to Floor 4
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor 2; ExitFloor = Floor.Zero; ArrivalClock = Clock 10; EntryClock = None; ExitClock = None }
        { Id = PersonId 2; EntryFloor = Floor 2; ExitFloor = Floor 4; ArrivalClock = Clock 11; EntryClock = None; ExitClock = None }
        { Id = PersonId 3; EntryFloor = Floor 2; ExitFloor = Floor.Zero; ArrivalClock = Clock 12; EntryClock = None; ExitClock = None }
        { Id = PersonId 4; EntryFloor = Floor 2; ExitFloor = Floor 4; ArrivalClock = Clock 13; EntryClock = None; ExitClock = None }
    |]

    let b =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          SimulationElevators =
            { Levels = 6
              NumberOfCabins = 1
              Capacity = 6 }
          SimulationPersons = SimulationPersonsArray personsData 
          LogDetails = { standardLogDetails with ShowLog = true }
          //LogDetails = { 
          //    ShowLog = true
          //    ShowEvents = false
          //    ShowInitialPersons = false
          //    ShowDetailedPersonStats = true
          //    ShowDetailedElevatorStatRecords = false }
          Durations = standardDurations
          }

    let res = runSimulation b

    assert (res.SimulationStats.SimulationDuration = 81)
    let tp1 = res.TransportedPersons.Find (fun p -> p.Id = PersonId 1)
    let tp2 = res.TransportedPersons.Find (fun p -> p.Id = PersonId 2)
    let tp3 = res.TransportedPersons.Find (fun p -> p.Id = PersonId 3)
    let tp4 = res.TransportedPersons.Find (fun p -> p.Id = PersonId 4)

    assert (tp1.EntryClock = Some(Clock 21))
    assert (tp3.EntryClock = Some(Clock 23))
    assert (tp1.ExitClock = Some(Clock 41))
    assert (tp3.ExitClock = Some(Clock 43))

    assert (tp2.EntryClock = Some(Clock 57))
    assert (tp4.EntryClock = Some(Clock 59))
    assert (tp2.ExitClock = Some(Clock 77))
    assert (tp4.ExitClock = Some(Clock 79))

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats


// Just a random long simulation
let testARandomSimulation () =
    printfn "\n---------------------------------------\nTest a random simulation\n"

    let b =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
          SimulationPersons = SimulationRandomGeneration(1000, 36000, 1, FullRandom) 
          LogDetails = { standardLogDetails with ShowDetailedPersonsStats = false }
          //LogDetails = { 
          //    ShowLog = true
          //    ShowEvents = true
          //    ShowInitialPersons = true
          //    ShowDetailedPersonStats = true
          //    ShowDetailedElevatorStatRecords = true }
          Durations = standardDurations
        }

    let res = runSimulation b
    
    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats


let testContinuousSimulation () =
    printfn "\n---------------------------------------\nTest continuous simulation\n"

    let refDataBag =
        { EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
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


System.Console.OutputEncoding <- System.Text.Encoding.UTF8
printfn "Elevator simulation in F#\n"

//testSimple1 ()
//testSimulation10PersonsArrivingTogetherWithCabinCapacity6 ()
//testPersonArrivingJustWhenCabinDoorsAreAboutToClose ()
//testDoorsClosingWhenAPersonArrives ()
//testPersonsGoingUpAndDownFromSameFloor ()
testARandomSimulation ()
//testContinuousSimulation ()

printfn "\nDone."
