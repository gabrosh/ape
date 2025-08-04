module ExecutionSettings

open CommandArgs
open ExecutionCommon
open UserMessages

let private applySettings context =
    context.textArea.ApplySettings ()

let private executeSettingsCommand context (argsMap: ArgsMap) f =
    let scopeString = argsMap["scope"]
    let nameString  = argsMap["name" ] |> Option.get

    let scopeResult =
        match scopeString with
        | Some x ->
            Settings.parseScope x |> Result.map Some
        | None   ->
            Ok None

    let nameResult =
        Settings.parseName nameString

    match scopeResult, nameResult with
    | Ok scope, Ok name ->
        f scope name
    | Error e, _
    | _, Error e ->
        context.userMessages.RegisterMessage (makeErrorMessage e)

// set

let argsMapSpec_set = (2, [| "scope"; "name"; "value" |])

let execute_set context (argsMap: ArgsMap) =
    let settings = context.textArea.CurrentSettings

    executeSettingsCommand context argsMap (
        fun scope name ->
            let value = argsMap["value"] |> Option.get

            let result = Settings.setValue settings scope name value

            match result with
            | Ok ()   ->
                applySettings context
            | Error e ->
                context.userMessages.RegisterMessage (makeErrorMessage e)
    )

    false

// unset

let argsMapSpec_unset = (1, [| "scope"; "name" |])

let execute_unset context (argsMap: ArgsMap) =
    let settings = context.textArea.CurrentSettings

    executeSettingsCommand context argsMap (
        fun scope name ->
            let result = Settings.unsetValue settings scope name

            match result with
            | Ok ()   ->
                applySettings context
            | Error e ->
                context.userMessages.RegisterMessage (makeErrorMessage e)
    )

    false

// get

let argsMapSpec_get = (1, [| "name" |])

let execute_get context (argsMap: ArgsMap) =
    let settings = context.textArea.CurrentSettings

    let nameString = argsMap["name"] |> Option.get

    match Settings.parseName nameString with
    | Ok name ->
        let repr = Settings.getSettingRepr settings name
        context.userMessages.RegisterMessage (makeInfoMessage repr)
    | Error e ->
        context.userMessages.RegisterMessage (makeErrorMessage e)

    false
