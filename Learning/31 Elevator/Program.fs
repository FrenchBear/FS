// 31 Elevator
// Learning F#, Elevator simulation
// Main module, simulations tests and examples
//
// 2024-08-13   PV      First version
//
// Various tests


let simulationSimple1 () =
    let persone = [|
        { Id = PersonId 1; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock 10; EntryClock = None; ExitClock = None }
    |]

    // Create DataBag
    let b =
        { SimulationDescription = { Title = "Simple #1"; Description = "Juste une personne à transporter" }
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
          SimulationPersons = SimulationPersonsArray persone
          LogDetails = { standardLogDetails with ShowLog = true; ShowEvents = true }
          //LogDetails = { 
          //    ShowLog = true
          //    ShowEvents = false
          //    ShowInitialPersons = false
          //    ShowDetailedPersonsStats = false
          //    ShowDetailedElevatorStatRecords = false }
          Durations = standardDurations

          EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          Journal = new System.Collections.Generic.List<JournalRecord>()
        }

    runSimulation b

let testSimple1 () =
    printfn "\n---------------------------------------\nTest Simple #1\n"

    let res = simulationSimple1 ()

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats

// ============================================================================================================

let simulationSimple2 () =
    let persone = [|
        { Id = PersonId 1; EntryFloor = Floor 1; ExitFloor = Floor 4; ArrivalClock = Clock 4; EntryClock = None; ExitClock = None }
        { Id = PersonId 2; EntryFloor = Floor 1; ExitFloor = Floor 0; ArrivalClock = Clock 42; EntryClock = None; ExitClock = None }
        { Id = PersonId 3; EntryFloor = Floor 5; ExitFloor = Floor 0; ArrivalClock = Clock 47; EntryClock = None; ExitClock = None }
    |]

    // Create DataBag
    let b =
        { SimulationDescription = { Title = "Simple #2"; Description = "Trois personnes en trois voyages" }
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
          SimulationPersons = SimulationPersonsArray persone
          LogDetails = { standardLogDetails with ShowLog = true; ShowEvents = true; ShowJournal = true }
          //LogDetails = { 
          //    ShowLog = true
          //    ShowEvents = false
          //    ShowInitialPersons = false
          //    ShowDetailedPersonsStats = false
          //    ShowDetailedElevatorStatRecords = false }
          Durations = standardDurations

          EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          Journal = new System.Collections.Generic.List<JournalRecord>()
        }

    runSimulation b

let testSimple2 () =
    printfn "\n---------------------------------------\nTest Simple #2\n"

    let res = simulationSimple2 ()

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats

// ============================================================================================================

let simulation10PersonsArrivingTogetherWithCabinCapacity6 () =
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
        { SimulationDescription = { Title = "Personnes=10, Capacité=6"; Description = "Test 10 persons arriving together with cabin capacity 6" }
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
          SimulationPersons = SimulationPersonsArray tenPersonsArrivingAtTimeZero
          LogDetails = { standardLogDetails with ShowLog = true }
          Durations = standardDurations

          EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          Journal = new System.Collections.Generic.List<JournalRecord>()
        }

    runSimulation b

let test10PersonsArrivingTogetherWithCabinCapacity6 () =
    printfn "\n---------------------------------------\nTest 10 persons arriving together with cabin capacity 6\n"

    let res = simulation10PersonsArrivingTogetherWithCabinCapacity6 ()

    assert (res.ElevatorsStats.LevelsCovered[6] = 3) // First elevator move over 3 levels (Floor.Zero->3) with full capacity, 6 persons
    assert (res.ElevatorsStats.LevelsCovered[0] = 3) // Then three levels again with an empty cabin, to go back floor 3->0 to take remaining persons
    assert (res.ElevatorsStats.LevelsCovered[4] = 3) // Final travel over 3 levels too, with 4 remaining persons

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats

// ============================================================================================================

let simulationPersonArrivingJustWhenCabinDoorsAreAboutToClose () =
    // Person 2 arrives just when the door is about to close
    // Check that person events are processed before elevator events, so both persons are transported together
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 2; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock 4; EntryClock = None; ExitClock = None }
    |]

    let b =
        { SimulationDescription = { Title = "Arrivée quand les portes vont se fermer"; Description = "Person arriving just when cabin doors are about to close" }
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
          SimulationPersons = SimulationPersonsArray personsData 
          LogDetails = { standardLogDetails with ShowLog = true }
          Durations = standardDurations

          EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          Journal = new System.Collections.Generic.List<JournalRecord>()
        }

    runSimulation b

let testPersonArrivingJustWhenCabinDoorsAreAboutToClose () =
    printfn "\n---------------------------------------\nTest Person arriving just when cabin doors are about to close\n"

    let res = simulationPersonArrivingJustWhenCabinDoorsAreAboutToClose ()

    assert (res.ElevatorsStats.LevelsCovered[2] = 3) // Make sure that the two persons traveled together on 3 levels

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats

// ============================================================================================================

let simulationPersonArrivingJustWhenCabinDoorsHaveFinishedClosing () =
    // Person 2 arrives just when the door have closed, door should reopen and let the person move in
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 2; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock 6; EntryClock = None; ExitClock = None }
    |]

    let b =
        { SimulationDescription = { Title = "Arrivée quand les portes se sont fermées"; Description = "Person arriving just when cabin have finished closing" }
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
          SimulationPersons = SimulationPersonsArray personsData 
          LogDetails = { standardLogDetails with ShowLog = true }
          Durations = standardDurations

          EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          Journal = new System.Collections.Generic.List<JournalRecord>()
        }

    runSimulation b

let testPersonArrivingJustWhenCabinDoorsHaveFinishedClosing () =
    printfn "\n---------------------------------------\nTest Person arriving just when cabin have finished closing\n"

    let res = simulationPersonArrivingJustWhenCabinDoorsHaveFinishedClosing ()

    assert (res.ElevatorsStats.LevelsCovered[2] = 3) // Make sure that the two persons traveled together on 3 levels

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats

// ============================================================================================================

let simulationDoorsClosingWhenAPersonArrives () =
    // Person 2 arrives just when the door is closing, and person 3 1s later
    // Check that the closing door sequence is interrupted (ClosingDoors event is deleted) while both persons are maintained in the list
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock.Zero; EntryClock = None; ExitClock = None }
        { Id = PersonId 2; EntryFloor = Floor 3; ExitFloor = Floor.Zero; ArrivalClock = Clock 27; EntryClock = None; ExitClock = None }
        { Id = PersonId 3; EntryFloor = Floor 3; ExitFloor = Floor.Zero; ArrivalClock = Clock 28; EntryClock = None; ExitClock = None }
    |]

    let b =
        { SimulationDescription = { Title = "Arrivée quand les portes se ferment"; Description = "Doors closing when a person arrives" }
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
          SimulationPersons = SimulationPersonsArray personsData 
          LogDetails = { standardLogDetails with ShowLog = true }
          Durations = { 
              AccelerationDuration = 2
              OneLevelFullSpeed = 2
              FullSpeedBeforeDecisionDuration = 1
              OpeningDoorsDuration = 4                  // Need a long closing door for this test
              MoveInDuration = 2 
              MotorDelayDuration = 1 }

          EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          Journal = new System.Collections.Generic.List<JournalRecord>()
        }

    runSimulation b

let testDoorsClosingWhenAPersonArrives () =
    printfn "\n---------------------------------------\nTest Doors closing when a person arrives\n"

    let res = simulationDoorsClosingWhenAPersonArrives ()

    assert (res.SimulationStats.SimulationDuration = 60)
    let tp2 = res.TransportedPersons |> Array.find (fun p -> p.Id = PersonId 2)
    let tp3 = res.TransportedPersons |> Array.find (fun p -> p.Id = PersonId 3)
    assert (tp2.EntryClock = Some(Clock 28))
    assert (tp3.EntryClock = Some(Clock 30))

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats

// ============================================================================================================

let simulationPersonsGoingUpAndDownFromSameFloor () =
    // All persons arrive on 2nd floor, Persons 1 and 3 go to Floor.Zero, Persons 2 and 4 go to Floor 4
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor 2; ExitFloor = Floor.Zero; ArrivalClock = Clock 10; EntryClock = None; ExitClock = None }
        { Id = PersonId 2; EntryFloor = Floor 2; ExitFloor = Floor 4; ArrivalClock = Clock 11; EntryClock = None; ExitClock = None }
        { Id = PersonId 3; EntryFloor = Floor 2; ExitFloor = Floor.Zero; ArrivalClock = Clock 12; EntryClock = None; ExitClock = None }
        { Id = PersonId 4; EntryFloor = Floor 2; ExitFloor = Floor 4; ArrivalClock = Clock 13; EntryClock = None; ExitClock = None }
    |]

    let b =
        { SimulationDescription = { Title = "Personnes montant et descendant du même palier"; Description = "Persons going up and down from same floor" }
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
          SimulationPersons = SimulationPersonsArray personsData 
          LogDetails = { standardLogDetails with ShowLog = true }
          //LogDetails = { 
          //    ShowLog = true
          //    ShowEvents = false
          //    ShowInitialPersons = false
          //    ShowDetailedPersonStats = true
          //    ShowDetailedElevatorStatRecords = false }
          Durations = standardDurations

          EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          Journal = new System.Collections.Generic.List<JournalRecord>()
        }

    runSimulation b

let testPersonsGoingUpAndDownFromSameFloor () =
    printfn "\n---------------------------------------\nTest Persons going up and down from same floor\n"

    let res = simulationPersonsGoingUpAndDownFromSameFloor ()

    assert (res.SimulationStats.SimulationDuration = 81)

    let tp1 = res.TransportedPersons |> Array.find (fun p -> p.Id = PersonId 1)
    let tp2 = res.TransportedPersons |> Array.find (fun p -> p.Id = PersonId 2)
    let tp3 = res.TransportedPersons |> Array.find (fun p -> p.Id = PersonId 3)
    let tp4 = res.TransportedPersons |> Array.find (fun p -> p.Id = PersonId 4)

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

// ============================================================================================================

// Note that the result of this simulation is not satisfying
// When empty cabin is moving down after three first persons moved out on floor 3, two persons remain waiting to go up,
// one person on floor 0->3, and 1 person on floor 1->4
// But with current business logic, empty cabin stops at floor 1 with direction=NoDirection, person on floor 1 enters,
// cabin direction->up, and person on floor 0 will have to wait for the cabin going floor 1->4->0 before moving in
// In this case, it could be better not to stop on floor 1 and continue down to 0, but more thinking is needed

let simulationUselessStop () =
    // With a cabin capacity of 3, at atime 10, 4 persons are going up Floor 0->3 and 1 from 1->4
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock 10; EntryClock = None; ExitClock = None }
        { Id = PersonId 2; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock 10; EntryClock = None; ExitClock = None }
        { Id = PersonId 3; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock 10; EntryClock = None; ExitClock = None }
        { Id = PersonId 4; EntryFloor = Floor.Zero; ExitFloor = Floor 3; ArrivalClock = Clock 10; EntryClock = None; ExitClock = None }
        { Id = PersonId 5; EntryFloor = Floor 1; ExitFloor = Floor 4; ArrivalClock = Clock 10; EntryClock = None; ExitClock = None }
    |]

    let b =
        { SimulationDescription = { Title = "Ascenseur s'arrête mais cabine pleine"; Description = "Capacity 3, but 4 persons go up, and 1 next stop" }
          SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 3 }
          SimulationPersons = SimulationPersonsArray personsData 
          LogDetails = { standardLogDetails with ShowLog = true; ShowEvents = true; ShowJournal = true }
          //LogDetails = { 
          //    ShowLog = true
          //    ShowEvents = false
          //    ShowInitialPersons = false
          //    ShowDetailedPersonStats = true
          //    ShowDetailedElevatorStatRecords = false }
          Durations = standardDurations

          EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          Journal = new System.Collections.Generic.List<JournalRecord>()
        }

    runSimulation b

let testUselessStop () =
    printfn "\n---------------------------------------\nTest Useless stop\n"

    let res = simulationUselessStop ()

    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats

// ============================================================================================================

// Just a random long simulation
let simulationRandom () =
    let b =
        { SimulationDescription = { Title = "Simulation aléatoire"; Description = "A random large simulation" }
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

          EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
          Journal = new System.Collections.Generic.List<JournalRecord>()
        }

    runSimulation b

let testRandom () =
    printfn "\n---------------------------------------\nTest a random simulation\n"

    let res = simulationRandom ()
    
    Simulation.PrintSimulationData(res.SimulationData);
    PersonsActor.PrintTransportedPersons(res.TransportedPersons);
    PersonsActor.printPersonStats res.PersonsStats
    ElevatorsActor.printElevatorStats res.ElevatorsStats
    Simulation.printSimulationStats res.SimulationStats

// ============================================================================================================

let testContinuousSimulation () =
    printfn "\n---------------------------------------\nTest continuous simulation\n"

    printfn "\nContinuous random simulation of variable number of persons over 800s"
    for np in 0..5..130 do
        let b =
            { SimulationDescription = { Title = $"Simulation continue à {np} personnes"; Description = "Continuous simulation" }
              SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
              SimulationPersons = SimulationRandomGeneration(np, 800, 1, Ground50Levels50)
              LogDetails = standardLogDetails
              Durations = standardDurations

              EventsQueue = new System.Collections.Generic.PriorityQueue<CommonEvent, ClockPriority>()
              Journal = new System.Collections.Generic.List<JournalRecord>()
            }

        let res = runSimulation b

        printf "p=%3d: wait = %5.1f " np res.PersonsStats.AvgWaitForElevator
        let s = int(res.PersonsStats.AvgWaitForElevator/350.0*80.0+0.5)
        printfn "%*c" s '*'


// ============================================================================================================
// ============================================================================================================

System.Console.OutputEncoding <- System.Text.Encoding.UTF8
printfn "Elevator simulation in F#\n"

//testSimple1 ()
//testSimple2 ()
//test10PersonsArrivingTogetherWithCabinCapacity6 ()
//testPersonArrivingJustWhenCabinDoorsAreAboutToClose ()
//testPersonArrivingJustWhenCabinDoorsHaveFinishedClosing ()
//testDoorsClosingWhenAPersonArrives ()
//testPersonsGoingUpAndDownFromSameFloor ()
testUselessStop()
//testRandom ()
//testContinuousSimulation ()

printfn "\nDone."
