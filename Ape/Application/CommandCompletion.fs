module CommandCompletion

open CommandArgs
open CommandsList
open CompletionCommon
open CompletionUtils
open DataTypes
open Position
open StringInCompl

// commands names

let private getCommandName
    (name, _abbr, _argsMapSpec, _executeFun, _completeFun) =

    seq {
        if name <> "" then yield name
    }

let private getCommandNames () =
    commandsList
    |> Seq.collect getCommandName
    |> Seq.sort
    |> Seq.toArray

let private commandNames = getCommandNames ()

// commands mapping

type private ArgSpec = {
    argName:     string
    completeFun: CompleteFun
}

type private CommandSpec = {
    mandatoryCount: int
    argSpecs:       ArgSpec array
}

type private CommandsMap =
    Map<string, CommandSpec>

let private getCommandSpecs
    (name, abbr, argsMapSpec: ArgsMapSpec, _executeFun, completeFuns: CompleteFun list) =

    seq {
        let mandatoryCount, argNames = argsMapSpec

        let n = completeFuns.Length

        if n <> 0 && n <> argNames.Length then
            invalidOp "Incorrect completeFuns length"

        let argSpecs =
            Seq.zip argNames completeFuns
            |> Seq.map (
                fun (argName, completeFun) -> {
                    argName     = argName
                    completeFun = completeFun
                }
            )
            |> Seq.toArray

        let commandSpec = {
            mandatoryCount = mandatoryCount
            argSpecs       = argSpecs
        }

        if name <> "" then yield (name, commandSpec)
        if abbr <> "" then yield (abbr, commandSpec)
    }

let private commandsMap : CommandsMap =
    commandsList
    |> Seq.collect getCommandSpecs
    |> Map

// command completion

/// Returns string in completion and possible completions for given command in completion.
let private getCommandCompletions (commandInCompl: string) =
    let completions =
        commandNames
        |> keepCommandsStartingWith commandInCompl
        |> ResizeArray

    let stringInCompl = {
        quoteType = NotQuoted
        orig      = commandInCompl
        unescaped = commandInCompl
    }

    (stringInCompl, completions)

// args completion

/// Returns all possible combinations of argsMap and completeFun for given complete arguments.
let private getCallsToComplete
    (argsCompl: string array) mandatoryCount (argSpecs: ArgSpec array) =

    let specsCount = argSpecs.Length
    let complCount = argsCompl.Length
    let firstMandatory = specsCount - mandatoryCount
    // - 1 for the argument in completion
    let maxFirst = specsCount - complCount - 1
    // Include all mandatory arguments.
    let maxFirst = min maxFirst firstMandatory

    seq {
        for first = 0 to maxFirst do
            let argsMap =
                seq {
                    for i = 0 to specsCount - 1 do
                        // Is it a completed argument ?
                        if i >= first && i < first + complCount then
                            yield (argSpecs[i].argName, Some argsCompl[i - first])
                        else
                            yield (argSpecs[i].argName, None)
                } |> Map

            // completeFun for the argument in completion
            let completeFun =
                argSpecs[first + complCount].completeFun

            yield (argsMap, completeFun)
    }

/// Returns possible completions for given complete arguments and argument in completion.
let private collectCompletions context argsCompl argInCompl mandatoryCount argSpecs =
    let calls = getCallsToComplete argsCompl mandatoryCount argSpecs

    seq {
        for argsMap, completeFun in calls do
            yield! (completeFun context argsMap argInCompl)
    }

/// Returns string in completion and possible completions for given command and args.
let private getArgsCompletions (command: string) (args: string) =
    match commandsMap.TryFind command with
    | Some commandSpec ->
        let mandatoryCount = commandSpec.mandatoryCount
        let argSpecs       = commandSpec.argSpecs

        let maxCount = argSpecs.Length

        let argsForComplResult = getArgsForCompl args maxCount

        match argsForComplResult with
        | Ok (argsCompl, argInCompl) ->
            // empty context
            let context = ()

            let completions =
                collectCompletions context argsCompl argInCompl mandatoryCount argSpecs
                |> ResizeArray

            (argInCompl, completions)
        | Error _e ->
            (noStringInCompl, noCompletions)

    | None ->
        (noStringInCompl, noCompletions)

// general

/// Returns true if cursor is at a position where completion is possible.
let private isCompletable (lineStr: string) (cursorChar: int) =
    cursorChar = lineStr.Length || lineStr[cursorChar] = ' '

let private getCompletionsAux (lineStr: string) (cursorChar: int) =
    let lineLeft = lineStr.Substring(0, cursorChar).TrimStart()

    let lineParts = lineLeft.Split (null, 2)

    let command, args =
        match lineParts.Length with
        | 1 -> lineParts[0], None
        | 2 -> lineParts[0], Some lineParts[1]
        | _ -> invalidOp ""

    match args with
    | Some args -> getArgsCompletions command args
    | None      -> getCommandCompletions command

/// Returns string in completion and possible completions for given items to complete.
let getCompletions
    (itemsToComplete: (Chars * Position) seq) =

    let chars, cursor = itemsToComplete |> Seq.head

    let lineStr = charsToString chars

    if isCompletable lineStr cursor.char then
        Ok (getCompletionsAux lineStr cursor.char)
    else
        Error "Cursor(s) at wrong position(s)."
