module CommandLineV1

// constants used later
let OrderByName = "N"
let OrderBySize = "S"

// set up a type to represent the options
type CommandLineOptions1 = {
    verbose: bool;
    subdirectories: bool;
    orderby: string;
}

let rec parseCommandLineRec args optionsSoFar =
    match args with
    // empty list means we're done.
    | [] ->
        optionsSoFar

    // match verbose flag
    | "/v"::xs ->
        let newOptionsSoFar = { optionsSoFar with verbose=true}
        parseCommandLineRec xs newOptionsSoFar

    // match subdirectories flag
    | "/s"::xs ->
        let newOptionsSoFar = { optionsSoFar with subdirectories=true}
        parseCommandLineRec xs newOptionsSoFar

    // match orderBy by flag
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
            eprintfn "OrderBy needs a second argument"
            parseCommandLineRec xs optionsSoFar

    // handle unrecognized option and keep looping
    | x::xs ->
        eprintfn "Option '%s' is unrecognized" x
        parseCommandLineRec xs optionsSoFar

// create the "public" parse function
let parseCommandLine1 args =
    // create the defaults
    let defaultOptions = {
        verbose = false;
        subdirectories = false;
        orderby = OrderByName
        }

    // call the recursive one with the initial options
    parseCommandLineRec args defaultOptions
