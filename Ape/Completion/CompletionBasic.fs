module CompletionBasic

open System

open CommandArgs
open CompletionCommon
open CompletionUtils
open ExecutionCommon
open FilePathUtils

let private getPathForEnumerate (dirNameIsEmpty: bool) (dirName: string) =
    if dirNameIsEmpty then
        "."
    else
        dirName

let private transformEnumerateEntry (dirNameIsEmpty: bool) (entry: string) =
    if dirNameIsEmpty then
        entry.Substring 2
    else
        entry

/// Returns the filePath completion taking into account all files and directories
/// matching given pattern.
let private getFilePathCompletions (argInCompl: string) =
    let fileName = IO.Path.GetFileName argInCompl

    let pattern =
        if fileName.Contains "*" || fileName.Contains "?" then
            argInCompl
        else
            argInCompl + "*"
    
    let dirName' = IO.Path.GetDirectoryName pattern
    // When argInCompl starts with \\, dirName' = null - it's a root directory.
    let dirName = if dirName' = null then pattern else dirName'
    let dirNameIsEmpty = (dirName = "")

    let path = getPathForEnumerate dirNameIsEmpty dirName
    let searchPattern = IO.Path.GetFileName pattern

    try
        let filePaths =
            IO.Directory.EnumerateFileSystemEntries (path, searchPattern)
            |> Seq.map (transformEnumerateEntry dirNameIsEmpty)
            |> Seq.toArray

        let n = filePaths.Length

        if n = 0 then
            seq { ForList "#filePath" }
        elif n = 1 then
            let completed = filePaths
            seq { Both ($"#filePath:{n}", completed) }
        else
            let commonPrefix = getCommonPrefix filePaths

            if equalsWithPlatformCase commonPrefix argInCompl then
                let completed = filePaths
                seq { Both ($"#filePath:{n}", completed) }
            else
                let completed = Array.append [| commonPrefix |] filePaths
                seq { Both ($"#filePath:+{n}", completed) }
    with
        | :? IO.IOException ->
            seq { ForList "#filePath" }

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

let complete_writeAux_filePath _context (_argsMap: ArgsMap) (argInCompl: string) =
    getFilePathCompletions argInCompl

let complete_writeAux: CompleteFun list = [
    complete_writeAux_filePath
]

let complete_write     = complete_writeAux
let complete_writeBang = complete_writeAux

// edit, edit!, view, view!, extract, extract!

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

let complete_editViewExtract_strictEncoding _context (argsMap: ArgsMap) (argInCompl: string) =
    if check_editView argsMap then
        seq { "false"; "true" }
        |> keepStartingWith argInCompl
    else
        noCompletions

let complete_editViewExtract_encoding _context (argsMap: ArgsMap) (argInCompl: string) =
    if check_editView argsMap then
        getSuggestedEncodings argInCompl
    else
        noCompletions

let complete_editViewExtract_filePath _context (argsMap: ArgsMap) (argInCompl: string) =
    if check_editView argsMap then
        getFilePathCompletions argInCompl
    else
        noCompletions

let complete_editViewExtract: CompleteFun list = [
    complete_editViewExtract_strictEncoding
    complete_editViewExtract_encoding
    complete_editViewExtract_filePath
]

let complete_edit        = complete_editViewExtract
let complete_editBang    = complete_editViewExtract
let complete_view        = complete_editViewExtract
let complete_viewBang    = complete_editViewExtract
let complete_extract     = complete_editViewExtract
let complete_extractBang = complete_editViewExtract

// bufferName

let complete_bufferName_bufferName _context (_argsMap: ArgsMap) (_argInCompl: string) =
    seq { ForList "#bufferName" }

let complete_bufferName: CompleteFun list = [
    complete_bufferName_bufferName
]
