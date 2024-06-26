// Another version of command line parser
//
// 2024-06-26   PV

module CommandLineV2 

type OrderByOption = OrderBySize | OrderByName
type SubdirectoryOption = IncludeSubdirectories | ExcludeSubdirectories
type VerboseOption = VerboseOutput | TerseOutput

type CommandLineOptions2 = {
    verbose: VerboseOption;
    subdirectories: SubdirectoryOption;
    orderby: OrderByOption
}

// create the "helper" recursive function
let rec parseCommandLineRec args optionsSoFar =
    match args with
    // empty list means we're done.
    | [] ->
        optionsSoFar

    // match verbose flag
    | "/v"::xs ->
        let newOptionsSoFar = { optionsSoFar with verbose=VerboseOutput}
        parseCommandLineRec xs newOptionsSoFar

    // match subdirectories flag
    | "/s"::xs ->
        let newOptionsSoFar = { optionsSoFar with subdirectories=IncludeSubdirectories}
        parseCommandLineRec xs newOptionsSoFar

    // match sort order flag
    | "/o"::xs ->
        //start a submatch on the next arg
        match xs with
        | "S"::xss ->
            let newOptionsSoFar = { optionsSoFar with orderby=OrderBySize}
            parseCommandLineRec xss newOptionsSoFar
        | "N"::xss ->
            let newOptionsSoFar = { optionsSoFar with orderby=OrderByName}
            parseCommandLineRec xss newOptionsSoFar
        // handle unrecognized option and keep looping
        | _ ->
            printfn "OrderBy needs a second argument"
            parseCommandLineRec xs optionsSoFar

    // handle unrecognized option and keep looping
    | x::xs ->
        printfn "Option '%s' is unrecognized" x
        parseCommandLineRec xs optionsSoFar

// create the "public" parse function
let parseCommandLine2 args =
    // create the defaults
    let defaultOptions = {
        verbose = TerseOutput;
        subdirectories = ExcludeSubdirectories;
        orderby = OrderByName
        }

    // call the recursive one with the initial options
    parseCommandLineRec args defaultOptions

