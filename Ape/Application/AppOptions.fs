module AppOptions

open System

type AppOptions = {
    cfgName:        string
    encoding:       string option
    strictEncoding: string option
    filePath:       string option
    isReadOnly:     string option
}

type private ParsedArgs = {
    cfgName:        string option
    encoding:       string option
    strictEncoding: string option
    filePath:       string option
    isReadOnly:     string option
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
            acc with cfgName    = Some cfgName
        }

    | "-enc" :: encoding       :: rest when Option.isNone acc.encoding ->
        parseArgs rest {
            acc with encoding   = Some encoding
        }

    | "-se"  :: strictEncpding :: rest when Option.isNone acc.strictEncoding ->
        parseArgs rest {
            acc with strictEncoding = Some strictEncpding
        }

    | "-v"   :: filePath       :: rest when Option.isNone acc.filePath ->
        parseArgs rest {
            acc with filePath   = Some filePath
                     isReadOnly = Some "true"
        }

    | "-e"   :: filePath       :: rest when Option.isNone acc.filePath ->
        parseArgs rest {
            acc with filePath   = Some filePath
                     isReadOnly = Some "false"
        }

    | filePath                 :: []   when Option.isNone acc.filePath ->
        Ok {
            acc with filePath   = Some filePath
                     isReadOnly = Some "false"
        }

    | _  ->
        Error $"{INVALID_COMMAND_LINE_ARGUMENTS}: '{joinArgs args}'"

/// Returns application options derived from given command line arguments.
/// Valid arguments are "-c cfgName", "-v filePath" and "filePath".
/// Arguments "-v filePath" and "filePath" are mutually exclusive.
let getAppOptions args : Result<AppOptions, string> =
    let parsedArgs = parseArgs args {
        cfgName        = None
        encoding       = None
        strictEncoding = None
        filePath       = None
        isReadOnly     = None
    }

    match parsedArgs with
    | Ok x    ->
        Ok {
            cfgName        = x.cfgName |> Option.defaultValue ExecutionCommon.defaultCfgName
            encoding       = x.encoding
            strictEncoding = x.strictEncoding
            filePath       = x.filePath
            isReadOnly     = x.isReadOnly
        }
    | Error e ->
        Error e
