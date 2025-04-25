module CompletionKeyMappings

open CommandArgs
open CompletionCommon
open CompletionUtils

// map, unmap

let check_mapUnmap (argsMap: ArgsMap) =
    let scope = argsMap["scope"]
    let mode  = argsMap["mode" ]
    let key   = argsMap["key"  ]

    let has0 = scope |> Option.map KeyMappings.parseScope
    let has1 = mode  |> Option.map KeyMappings.parseMode
    let has2 = key   |> Option.map KeysStrings.parseKeyPrefixAndKeyString

    match has0, has1, has2 with
    | Some (Error _), _, _
    | _, Some (Error _), _
    | _, _, Some (Error _)
        -> false
    | _
        -> true

let complete_mapUnmap_scope _context (argsMap: ArgsMap) (argInCompl: string) =
    if check_mapUnmap argsMap then
        KeyMappings.scopeStrings
        |> keepStartingWith argInCompl
    else
        noCompletions

let complete_mapUnmap_mode _context (argsMap: ArgsMap) (argInCompl: string) =
    if check_mapUnmap argsMap then
        KeyMappings.modeStrings
        |> keepStartingWith argInCompl
    else
        noCompletions

let complete_mapUnmap_key _context (argsMap: ArgsMap) (_argInCompl: string) =
    if check_mapUnmap argsMap then
        seq { ListOnly "#key" }
    else
        noCompletions

let complete_map_keySeq _context (argsMap: ArgsMap) (_argInCompl: string) =
    if check_mapUnmap argsMap then
        seq { ListOnly "#keySeq"}
    else
        noCompletions

let complete_map: CompleteFun list = [
    complete_mapUnmap_scope
    complete_mapUnmap_mode
    complete_mapUnmap_key
    complete_map_keySeq
]

let complete_unmap: CompleteFun list = [
    complete_mapUnmap_scope
    complete_mapUnmap_mode
    complete_mapUnmap_key
]
