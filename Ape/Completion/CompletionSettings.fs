module CompletionSettings

open CommandArgs
open CompletionCommon
open CompletionUtils

// set, unset

let check_setUnset (argsMap: ArgsMap) =
    let scope = argsMap["scope"]
    let name  = argsMap["name" ]

    let has0 = scope |> Option.map Settings.parseScope
    let has1 = name  |> Option.map Settings.parseName

    match has0, has1 with
    | Some (Error _), _
    | _, Some (Error _)
        -> false
    | _
        -> true

let complete_setUnset_scope _context (argsMap: ArgsMap) (argInCompl: string) =
    if check_setUnset argsMap then
        Settings.scopeStrings
        |> keepStartingWith argInCompl
    else
        noCompletions

let complete_setUnset_name _context (argsMap: ArgsMap) (argInCompl: string) =
    if check_setUnset argsMap then
        Settings.nameStrings
        |> keepStartingWith argInCompl
    else
        noCompletions

let complete_set_value _context (argsMap: ArgsMap) (argInCompl: string) =
    if check_setUnset argsMap then
        let nameString = argsMap["name"] |> Option.get
        let nameResult = Settings.parseName nameString
        let name       = nameResult |> Utils.resultGet

        match name with
        | Settings.Name.colorScheme ->
            ColorUtils.schemesArray
            |> keepStartingWith argInCompl

        | Settings.Name.encoding    ->
            getSuggestedEncodings argInCompl

        | _ ->
            match Settings.specsMap[name] with
            | Settings.Bools   _ ->
                seq { "false"; "true" }
                |> keepStartingWith argInCompl
            | Settings.FileFormats _ ->
                FileUtils.fileFormatsArray
                |> keepStartingWith argInCompl
            | Settings.Ints    _ ->
                seq { ForList "#int" }
            | Settings.Strings _ ->
                seq { ForList "#str" }
                
    else
        noCompletions

let complete_set: CompleteFun list = [
    complete_setUnset_scope
    complete_setUnset_name
    complete_set_value
]

let complete_unset: CompleteFun list = [
    complete_setUnset_scope
    complete_setUnset_name
]

// get

let complete_get_name _context (_argsMap: ArgsMap) (argInCompl: string) =
    Settings.nameStrings
    |> keepStartingWith argInCompl

let complete_get: CompleteFun list = [
    complete_get_name
]
