module CompletionBasic

open System

open CommandArgs
open CompletionCommon
open CompletionUtils
open ExecutionCommon

// execCfg

let complete_execCfg_cfgName _context (_argsMap: ArgsMap) (argInCompl: string) =
    IO.Directory.EnumerateFiles (
        cfgFilesDir, "*" + cfgFileExt
    )
    |> Seq.map (
        fun filePath -> IO.Path.GetFileNameWithoutExtension filePath
    )
    |> keepStartingWith argInCompl

let complete_execCfg: CompleteFun list = [
    complete_execCfg_cfgName
]

// write, write!

let complete_writeAux_filePath _context (_argsMap: ArgsMap) (_argInCompl: string) =
    seq { ListOnly "#filePath" }

let complete_writeAux: CompleteFun list = [
    complete_writeAux_filePath
]

let complete_write     = complete_writeAux
let complete_writeBang = complete_writeAux

// edit, edit!, view, view!

let check_editView (argsMap: ArgsMap) =
    let strictEncoding = argsMap["strictEncoding"]
    let encoding       = argsMap["encoding"      ]

    let has0 = strictEncoding |> Option.map Parsing.parseBool
    let has1 = encoding       |> Option.map Parsing.parseEncoding

    match has0, has1 with
    | Some (Error _), _
    | _, Some (Error _)
        -> false
    | _
        -> true

let complete_editView_strictEncoding _context (argsMap: ArgsMap) (argInCompl: string) =
    if check_editView argsMap then
        seq { "false"; "true" }
        |> keepStartingWith argInCompl
    else
        noCompletions

let complete_editView_encoding _context (argsMap: ArgsMap) (argInCompl: string) =
    if check_editView argsMap then
        getSuggestedEncodings argInCompl
    else
        noCompletions

let complete_editView_filePath _context (argsMap: ArgsMap) (_argInCompl: string) =
    if check_editView argsMap then
        seq { ListOnly "#filePath" }
    else
        noCompletions

let complete_editView: CompleteFun list = [
    complete_editView_strictEncoding
    complete_editView_encoding
    complete_editView_filePath
]

let complete_edit     = complete_editView
let complete_editBang = complete_editView
let complete_view     = complete_editView
let complete_viewBang = complete_editView

// extract

let complete_extract_filePath _context (_argsMap: ArgsMap) (_argInCompl: string) =
    seq { ListOnly "#filePath" }

let complete_extract: CompleteFun list = [
    complete_extract_filePath
]
