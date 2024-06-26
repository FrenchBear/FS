// Another version of command line parser
//
// 2024-06-26   PV

module CommandLineV3

type OrderByOption = OrderBySize | OrderByName
type SubdirectoryOption = IncludeSubdirectories | ExcludeSubdirectories
type VerboseOption = VerboseOutput | TerseOutput

type CommandLineOptions = {
    verbose: VerboseOption;
    subdirectories: SubdirectoryOption;
    orderby: OrderByOption }

type ParseMode = TopLevel | OrderBy

type FoldState = {
    options: CommandLineOptions ;
    parseMode: ParseMode;
    }

// parse the top-level arguments
// return a new FoldState
let parseTopLevel arg optionsSoFar =
    match arg with
    // match verbose flag
    | "/v" ->
        let newOptionsSoFar = {optionsSoFar with verbose=VerboseOutput}
        {options=newOptionsSoFar; parseMode=TopLevel}
    // match subdirectories flag
    | "/s"->
        let newOptionsSoFar = { optionsSoFar with subdirectories=IncludeSubdirectories}
        {options=newOptionsSoFar; parseMode=TopLevel}
    // match sort order flag
    | "/o" ->
        {options=optionsSoFar; parseMode=OrderBy}
    // handle unrecognized option and keep looping
    | x ->
        printfn "Option '%s' is unrecognized" x
        {options=optionsSoFar; parseMode=TopLevel}
// parse the orderBy arguments
// return a new FoldState
let parseOrderBy arg optionsSoFar =
    match arg with
    | "S" ->
        let newOptionsSoFar = { optionsSoFar with orderby=OrderBySize}
        {options=newOptionsSoFar; parseMode=TopLevel}
    | "N" ->
        let newOptionsSoFar = { optionsSoFar with orderby=OrderByName}
        {options=newOptionsSoFar; parseMode=TopLevel}
    // handle unrecognized option and keep looping
    | _ ->
        printfn "OrderBy needs a second argument"
        {options=optionsSoFar; parseMode=TopLevel}
// create a helper fold function
let foldFunction state element  =
    match state with
    | {options=optionsSoFar; parseMode=TopLevel} ->
        // return new state
        parseTopLevel element optionsSoFar
    | {options=optionsSoFar; parseMode=OrderBy} ->
        // return new state
        parseOrderBy element optionsSoFar
// create the "public" parse function
let parseCommandLine3 args =
    let defaultOptions = {
        verbose = TerseOutput;
        subdirectories = ExcludeSubdirectories;
        orderby = OrderByName }
    let initialFoldState = {options=defaultOptions; parseMode=TopLevel}
    // call fold with the initial state
    args |> List.fold foldFunction initialFoldState

