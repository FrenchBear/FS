// 08 CLParser
// Learning F#, Parsing command line
//
// 2024-06-25   PV

open CommandLineV1
open CommandLineV2
open CommandLineV3

// ==============================
// Test V1

let printOptions1 (opt:CommandLineOptions1) =
    printf "MyApp1 "
    if opt.verbose then printf "/V "
    if opt.subdirectories then printf "/S "
    match opt.orderby with
    | "N" -> printf "OrderByName"
    | "S" -> printf "OrderBySize"
    | _ -> ()
    printf "\n"

// test this version
parseCommandLine1 ["/v"] |> printOptions1
parseCommandLine1 ["/v"; "/s"] |> printOptions1
parseCommandLine1 ["/o"; "S"] |> printOptions1

// error handling
parseCommandLine1 ["/v"; "xyz"] |> ignore
parseCommandLine1 ["/o"; "xyz"] |> ignore

printfn ""


// ==============================
// Test V2

let printOptions2 (opt:CommandLineOptions2) =
    printf "MyApp2 "
    match opt.verbose with
    | CommandLineV2.VerboseOption.VerboseOutput -> printf "/V "
    | _ -> ()
    if opt.subdirectories=CommandLineV2.SubdirectoryOption.IncludeSubdirectories then printf "/S "
    match opt.orderby with
    | CommandLineV2.OrderByOption.OrderByName -> printf "OrderByName"
    | CommandLineV2.OrderByOption.OrderBySize -> printf "OrderBySize"
    printf "\n"
    

parseCommandLine2 ["/v"] |> printOptions2
parseCommandLine2 ["/v"; "/s"] |> printOptions2
parseCommandLine2 ["/o"; "S"] |> printOptions2

// error handling
parseCommandLine2 ["/v"; "xyz"] |> ignore
parseCommandLine2 ["/o"; "xyz"] |> ignore

printfn ""


// ==============================
// Test V3

let printOptions3 (opt:FoldState) =
    printf "MyApp3 "
    match opt.options.verbose with
    | CommandLineV3.VerboseOption.VerboseOutput -> printf "/V "
    | _ -> ()
    if opt.options.subdirectories=CommandLineV3.SubdirectoryOption.IncludeSubdirectories then printf "/S "
    match opt.options.orderby with
    | CommandLineV3.OrderByOption.OrderByName -> printf "OrderByName "
    | CommandLineV3.OrderByOption.OrderBySize -> printf "OrderBySize "

    printfn "parseMode=%s" (match opt.parseMode with
                            | ParseMode.TopLevel -> "TopLevel"
                            | ParseMode.OrderBy -> "OrderBy")

parseCommandLine3 ["/v"] |> printOptions3
parseCommandLine3 ["/v"; "/s"] |> printOptions3
parseCommandLine3 ["/o"; "S"] |> printOptions3
// error handling
parseCommandLine3 ["/v"; "xyz"] |> ignore
parseCommandLine3 ["/o"; "xyz"] |> ignore
