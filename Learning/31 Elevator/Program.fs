// 31 Elevator
// Learning F#, Elevator simulation
// Main module, simulations tests and examples
//
// 2024-08-13   PV      First version, only 1 cabin

// "Main" module, Run the simulation
// In charge of master clock progression

System.Console.OutputEncoding <- System.Text.Encoding.UTF8
printfn "Elevator simulation in F#\n"


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
        { SimulationElevators =
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

testSimulation10PersonsArrivingTogetherWithCabinCapacity6 ()


let testWithAPersonArrivingJustWhenCabinDoorsAreAboutToClose () =

    // Person 2 arrives just when the door is about to close
    // Check that person events are processed before elevator events, so both persons are transported together
    let personsData = [|
        { Id = PersonId 1; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 0; EntryTime = None; ExitTime = None }
        { Id = PersonId 2; EntryFloor = Floor 0; ExitFloor = Floor 3; ArrivalTime = Clock 4; EntryTime = None; ExitTime = None }
    |]

    let b =
        { SimulationElevators =
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

testWithAPersonArrivingJustWhenCabinDoorsAreAboutToClose ()



// Create DataBag
let b =
    { SimulationElevators = { Levels = 6; NumberOfCabins = 1; Capacity = 6 }
      SimulationPersons = SimulationRandomGeneration(10, 800, 1, Ground50Levels50) 
      LogDetails = standardLogDetails
      Durations = standardDurations
    }

printSimulationParameters b
let res = runSimulation b
PersonsActor.printPersonStats res.PersonsStats
ElevatorsActor.printElevatorStats res.ElevatorsStats
printSimulationStats res.SimulationStats

printfn "\nDone."
