// 31 Elevator
// Elevator simulation in F#
// Simple console logging
//
// 2014-08-15   PV

[<AutoOpen>]
module Logging

let logMessage b clk msg =
    if b.LogDetails.ShowLog then
        let (Clock iClk) = clk
        printfn $"clk: {iClk, 4}  {msg}"

let logCabinUpdate b clk before after =
    let lst = new System.Collections.Generic.List<string>()

    if before.Floor <> after.Floor then
        lst.Add($"Floor {before.Floor}→{after.Floor}")

    if before.MotorStatus <> after.MotorStatus then
        lst.Add($"Motor {before.MotorStatus}→{after.MotorStatus}")

    if before.DoorStatus <> after.DoorStatus then
        lst.Add($"Door {before.DoorStatus}→{after.DoorStatus}")

    if before.Direction <> after.Direction then
        lst.Add($"Direction {before.Direction}→{after.Direction}")

    if before.PowerStatus <> after.PowerStatus then
        lst.Add($"Power {before.PowerStatus}→{after.PowerStatus}")


    let lstStopRequested = new System.Collections.Generic.List<string>()

    for i in 0 .. b.SimulationElevators.Levels - 1 do
        let b = before.getStopRequested (Floor i)
        let a = after.getStopRequested (Floor i)
        if a<>b then
            lstStopRequested.Add($"StopRequested[{i}]: {b}→{a}")

    if not (lstStopRequested.Count = 0) then
        lst.Add(System.String.Join(", ", lstStopRequested))


    let lstPersons = new System.Collections.Generic.List<string>()

    if List.length before.Persons <> List.length after.Persons then
        lstPersons.Add($"Persons count {List.length before.Persons}→{List.length after.Persons}")

    for pb in before.Persons do
        let ixOpt = List.tryFindIndex (fun pa -> pa.Id = pb.Id) after.Persons

        if ixOpt.IsNone then
            let (PersonId pid) = pb.Id
            lstPersons.Add($"Person {pid} out")

    for pa in after.Persons do
        let ixOpt = List.tryFindIndex (fun pb -> pb.Id = pa.Id) before.Persons

        if ixOpt.IsNone then
            let (PersonId pid) = pa.Id
            lstPersons.Add($"Person {pid} in")

    if not (lstPersons.Count = 0) then
        lst.Add(System.String.Join(", ", lstPersons))

    if not (lst.Count = 0) then
        logMessage b clk ("Cabin:   " + System.String.Join(", ", lst))


let logLandingUpdate b clk floor before after =
    let lst = new System.Collections.Generic.List<string>()

    if before.CallUp <> after.CallUp then
        lst.Add($"CallUp {before.CallUp}→{after.CallUp}")
    if before.CallDown <> after.CallDown then
        lst.Add($"CallDown {before.CallDown}→{after.CallDown}")

    let lstPersons = new System.Collections.Generic.List<string>()

    if List.length before.Persons <> List.length after.Persons then
        lstPersons.Add($"Persons count {List.length before.Persons}→{List.length after.Persons}")

    for pb in before.Persons do
        let ixOpt = List.tryFindIndex (fun pa -> pa.Id = pb.Id) after.Persons

        if ixOpt.IsNone then
            let (PersonId pid) = pb.Id
            lstPersons.Add($"Person {pid}→in cabin")

    for pa in after.Persons do
        let ixOpt = List.tryFindIndex (fun pb -> pb.Id = pa.Id) before.Persons

        if ixOpt.IsNone then
            let (PersonId pid) = pa.Id
            lstPersons.Add($"Person {pid} arrived →{pa.ExitFloor}")

    if not (lstPersons.Count = 0) then
        lst.Add(System.String.Join(", ", lstPersons))

    if not (lst.Count = 0) then
        let (Floor iFloor) = floor
        logMessage b clk ($"Landing: Landing {iFloor} " + System.String.Join(", ", lst))


let logPersonArrival b clk p =
    let (PersonId person) = p.Id
    let (Floor entry) = p.EntryFloor
    let (Floor exit) = p.ExitFloor
    logMessage b clk $"Person:  Person {person} Arrival Floor {entry}→Floor {exit}"

let logPersonExit b clk p =
    let (PersonId pid) = p.Id
    let (Clock arrivalIClk) = p.ArrivalClock
    let (Floor entry) = p.EntryFloor
    let (Floor exit) = p.ExitFloor
    let (Clock entryIClk) = p.EntryClock.Value
    let waitingCabin = entryIClk - arrivalIClk
    let (Clock exitIClk) = p.ExitClock.Value
    let totalTransportationTime = exitIClk - arrivalIClk

    logMessage b clk
        $"Person:  Person {pid} Exit, Arrival Floor {entry}@{arrivalIClk}, Waited {waitingCabin}, Entered@{entryIClk}, Exit Floor {exit}@{exitIClk}, Total {totalTransportationTime}"

