module ExecutionKeyMappings

open CommandArgs
open ExecutionCommon
open UserMessages

let private executeKeyMappingsCommand context (argsMap: ArgsMap) f =
    let scopeString = argsMap["scope"]
    let modeString  = argsMap["mode" ]
    let keyString   = argsMap["key"  ] |> Option.get

    let scopeResult =
        match scopeString with
        | Some x ->
            KeyMappings.parseScope x |> Result.map Some
        | None   ->
            Ok None

    let modeResult =
        match modeString with
        | Some x ->
            KeyMappings.parseMode x
        | None   ->
            Ok KeyMappings.Mode.normal

    let keyResult =
        KeysStrings.parseKeyPrefixAndKeyString keyString

    match scopeResult, modeResult, keyResult with
    | Ok scope, Ok mode, Ok (prefixKey, key) ->
        f scope (mode, prefixKey, key)
    | Error e, _, _
    | _, Error e, _
    | _, _, Error e ->
        context.userMessages.RegisterMessage (makeErrorMessage e)

// map

let argsMapSpec_map = (2, [| "scope"; "mode"; "key"; "keySeq" |])

let execute_map context (argsMap: ArgsMap) =
    let keyMappings = context.textArea.CurrentKeyMappings

    executeKeyMappingsCommand context argsMap (
        fun scope modeKeyTriple ->
            let keysString = argsMap["keySeq"] |> Option.get

            let keySeq = KeysStrings.parseKeysString keysString

            match keySeq with
            | Ok keySeq ->
                match KeyMappings.mapKey keyMappings scope modeKeyTriple keySeq with
                | Ok ()   ->
                    ()
                | Error e ->
                    context.userMessages.RegisterMessage (makeErrorMessage e)
            | Error e   ->
                context.userMessages.RegisterMessage (makeErrorMessage e)
    )

    false

// unmap

let argsMapSpec_unmap = (1, [| "scope"; "mode"; "key" |])

let execute_unmap context (argsMap: ArgsMap) =
    let keyMappings = context.textArea.CurrentKeyMappings

    executeKeyMappingsCommand context argsMap (
        fun scope modeKeyTriple ->
            match KeyMappings.unmapKey keyMappings scope modeKeyTriple with
            | Ok ()   ->
                ()
            | Error e ->
                context.userMessages.RegisterMessage (makeErrorMessage e)
    )

    false
