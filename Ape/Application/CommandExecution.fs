module rec CommandExecution

open System

open CommandArgs
open CommandsList
open ExecutionCommon
open UserMessages

// commands mapping

type private CommandSpec = {
    argsMapSpec: ArgsMapSpec
    executeFun:  ExecuteFun
}

type private CommandsMap =
    Map<string, CommandSpec>

let private getCommandSpecs
    (name, abbr, argsMapSpec: ArgsMapSpec, executeFun: ExecuteFun, _completeFunsList) =

    seq {
        let commandSpec = {
            argsMapSpec = argsMapSpec
            executeFun  = executeFun
        }

        if name <> "" then yield (name, commandSpec)
        if abbr <> "" then yield (abbr, commandSpec)
    }

let private commandsMap : CommandsMap =
    commandsList
    |> Seq.collect getCommandSpecs
    |> Map

let private commandsMapCfg : CommandsMap =
    commandsListCfg
    |> Seq.collect getCommandSpecs
    |> Map

// execution

// Returns true if the application is to be exited.
let private executeCommandAux
    (commandsMap: CommandsMap) userMessages textArea registers (promptLine: string) =

    let promptLine' = promptLine.TrimStart ()
    let lineParts = promptLine'.Split (null, 2)

    let command, args =
        match lineParts.Length with
        | 1 -> lineParts[0], None
        | 2 -> lineParts[0], Some lineParts[1]
        | _ -> invalidOp ""

    match commandsMap.TryFind command with
    | Some commandSpec ->
        let mandatoryCount, names = commandSpec.argsMapSpec
        let executeFun            = commandSpec.executeFun

        let argsMapResult = getArgsMapRight args mandatoryCount names

        match argsMapResult with
        | Ok argsMap ->
            let context = {
                userMessages      = userMessages
                textArea          = textArea
                registers         = registers
                executeCfgFileFun = executeCfgFile
            }
            executeFun context argsMap
        | Error e ->
            userMessages.RegisterMessage (makeErrorMessage e)
            false

    | None ->
        userMessages.RegisterMessage (
            formatMessage ERROR_UNKNOWN_COMMAND command
        )
        false

/// Executes editor command in promptLine.
/// Returns true if the application is to be exited.
let executeCommand userMessages textArea registers (promptLine: string) =
    executeCommandAux commandsMap userMessages textArea registers promptLine

/// Executes editor configuration command in promptLine.
/// Returns true if the application is to be exited.
let executeCfgCommand userMessages textArea registers (promptLine: string) =
    executeCommandAux commandsMapCfg userMessages textArea registers promptLine

let private executeFileAux userMessages textArea registers filePath =
    use stream = new IO.StreamReader (
        filePath, Text.Encoding.UTF8, true, FileUtils.getReadOptions ()
    )

    let mutable toContinue = true

    while toContinue && not stream.EndOfStream do
        let line = stream.ReadLine ()

        let toIgnore = String.IsNullOrEmpty line || line.StartsWith "#"

        if not toIgnore then
            executeCfgCommand userMessages textArea registers line
                |> ignore
            if userMessages.HasErrorMessage then
                toContinue <- false

/// Executes configuration commands from given file.
let executeCfgFile userMessages textArea registers cfgName =
    try
        executeFileAux userMessages textArea registers
            (getCfgFilePath cfgName)
    with
    | :? System.IO.DirectoryNotFoundException as ex ->
        userMessages.RegisterMessage (
            UserMessages.makeErrorMessage ex.Message
        )
    | :? System.IO.FileNotFoundException as ex ->
        userMessages.RegisterMessage (
            UserMessages.makeErrorMessage ex.Message
        )
