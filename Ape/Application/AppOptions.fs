module AppOptions

open System

type AppOptions = {
    cfgName:        string
    encoding:       string option
    strictEncoding: string option
    edit:           bool
    view:           bool
    extract:        bool
    filePath:       string option
}

type private ParsedArgs = {
    cfgName:        string option
    encoding:       string option
    strictEncoding: string option
    edit:           bool
    view:           bool
    extract:        bool
    filePath:       string option
}

let INVALID_COMMAND_LINE_ARGUMENTS = "Invalid command line arguments"

let private joinArgs (args: string list) =
    String.Join (' ', args)

[<TailCall>]
let rec private parseArgs args acc : Result<ParsedArgs, string> =
    match args with
    | [] ->
        Ok acc

    | "-c"   :: cfgName        :: rest when Option.isNone acc.cfgName  ->
        parseArgs rest {
            acc with cfgName   = Some cfgName
        }

    | "-enc" :: encoding       :: rest when Option.isNone acc.encoding ->
        parseArgs rest {
            acc with encoding  = Some encoding
        }

    | "-se"  :: strictEncpding :: rest when Option.isNone acc.strictEncoding ->
        parseArgs rest {
            acc with strictEncoding = Some strictEncpding
        }

    | "-e"   :: filePath       :: rest when Option.isNone acc.filePath ->
        parseArgs rest {
            acc with filePath  = Some filePath
                     edit      = true
        }

    | "-v"   :: filePath       :: rest when Option.isNone acc.filePath ->
        parseArgs rest {
            acc with filePath  = Some filePath
                     view      = true
        }

    | "-x"   :: filePath       :: rest when Option.isNone acc.filePath ->
        parseArgs rest {
            acc with filePath  = Some filePath
                     extract   = true
        }

    | filePath                 :: []   when Option.isNone acc.filePath ->
        Ok {
            acc with filePath  = Some filePath
                     edit      = true
        }

    | _  ->
        Error $"{INVALID_COMMAND_LINE_ARGUMENTS}: '{joinArgs args}'"

/// Returns application options derived from given command line arguments.
/// Valid arguments are "-c cfgName", "-e filePath", "-v filePath", "-x filePath" and "filePath".
/// Arguments "-v filePath" and "filePath" are mutually exclusive.
let getAppOptions args : Result<AppOptions, string> =
    let parsedArgs = parseArgs args {
        cfgName         = None
        encoding        = None
        strictEncoding  = None
        edit            = false
        view            = false
        extract         = false
        filePath        = None
    }

    match parsedArgs with
    | Ok x    ->
        Ok {
            cfgName        = x.cfgName |> Option.defaultValue ExecutionCommon.defaultCfgName
            encoding       = x.encoding
            strictEncoding = x.strictEncoding
            edit           = x.edit
            view           = x.view
            extract        = x.extract
            filePath       = x.filePath
        }
    | Error e ->
        Error e
