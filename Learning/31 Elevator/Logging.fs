// 31 Elevator
// Elevator simulation in F#
//
// 2014-08-15   PV

[<AutoOpen>]
module Logging

let logMessage clk msg =
    if showLog then
        let (Clock iClk) = clk
        printfn $"clk: {iClk, 4}  {msg}"

let logCabinUpdate clk before after =
    let lst = new System.Collections.Generic.List<string>()

    if before.Floor <> after.Floor then
        lst.Add($"Floor {before.Floor}→{after.Floor}")

    if before.Motor <> after.Motor then
        lst.Add($"Motor {before.Motor}→{after.Motor}")

    if before.Door <> after.Door then
        lst.Add($"Door {before.Door}→{after.Door}")

    if before.Direction <> after.Direction then
        lst.Add($"Direction {before.Direction}→{after.Direction}")

    if before.Cabin <> after.Cabin then
        lst.Add($"Cabin {before.Cabin}→{after.Cabin}")


    let lstStopRequested = new System.Collections.Generic.List<string>()

    for i in 0 .. levels - 1 do
        if before._StopRequested[i] <> after._StopRequested[i] then
            lstStopRequested.Add($"StopRequested[{i}]: {before._StopRequested[i]}→{after._StopRequested[i]}")

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
        logMessage clk (System.String.Join(", ", lst))

let logPersonArrival clk p =
    let (PersonId person) = p.Id
    let (Floor entry) = p.EntryFloor
    let (Floor exit) = p.ExitFloor
    logMessage clk $"Person {person} Arrival Floor {entry}→Floor {exit}"

let logPersonExit clk p =
    let (PersonId pid) = p.Id
    let (Clock arrivalClk) = p.ArrivalTime
    let (Floor entry) = p.EntryFloor
    let (Floor exit) = p.ExitFloor
    let (Clock entryClk) = p.EntryTime.Value
    let waitingCabin = entryClk - arrivalClk
    let (Clock exitClk) = p.ExitTime.Value
    let totalTransportationTime = exitClk - arrivalClk

    logMessage
        clk
        $"Person {pid} Exit, Arrival Floor {entry}@{arrivalClk}, Waited {waitingCabin}, Entered@{entryClk}, Exit Floor {exit}@{exitClk}, Total {totalTransportationTime}"

