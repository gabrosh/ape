module ExecutionBasic

open System

open CommandArgs
open ExecutionCommon
open UserMessages

// execCfg

let argsMapSpec_execCfg = (1, [| "cfgName" |])

let execute_execCfg context (argsMap: ArgsMap) =
    let cfgName = argsMap["cfgName"] |> Option.get

    let userMessages = context.userMessages
    let textArea     = context.textArea
    let registers    = context.registers

    context.executeCfgFileFun userMessages textArea registers cfgName

    false

// help

let argsMapSpec_help: ArgsMapSpec = (0, [| |])

let execute_help context (_argsMap: ArgsMap) =
    let filePath = IO.Path.Combine (
        AppContext.BaseDirectory, helpFileName
    )

    if context.textArea.HasBufferWithFilePath filePath then
        context.textArea.ToBufferWithFilePath filePath
        context.userMessages.RegisterMessage (
            formatMessage INFO_FILE_ALREADY_OPENED filePath
        )
    else
        context.textArea.ViewFile filePath None (Some "true")

    false

// quit, quit!

let argsMapSpec_quitAux: ArgsMapSpec = (0, [| |])

let command_quitAux quite context (_argsMap: ArgsMap) =
    if not quite && context.textArea.IsAnyBufferChanged () then
        context.userMessages.RegisterMessage WARNING_NO_WRITE_SINCE_LAST_CHANGE
        context.textArea.ToFirstChangedBuffer ()
        false
    else
        true

let argsMapSpec_quit     = argsMapSpec_quitAux
let argsMapSpec_quitBang = argsMapSpec_quitAux

let execute_quit     = command_quitAux false
let execute_quitBang = command_quitAux true

// write, write!

let argsMapSpec_writeAux = (0, [| "filePath" |])

let private command_writeAux quite context (argsMap: ArgsMap) =
    let filePath = argsMap["filePath"]

    match filePath with
    | Some filePath ->
        if context.textArea.HasBufferWithFilePath filePath then
            context.userMessages.RegisterMessage (
                formatMessage INFO_FILE_ALREADY_OPENED filePath
            )
        elif not quite && FileUtils.fileExists filePath then
            context.userMessages.RegisterMessage (
                formatMessage WARNING_FILE_ALREADY_EXISTS filePath
            )
        else
            context.textArea.WriteFileAs filePath
    | None ->
        if not quite && not context.textArea.IsBufferChanged then
            context.userMessages.RegisterMessage WARNING_NO_CHANGE_SINCE_LAST_WRITE
        else
            context.textArea.WriteFile ()

    false

let argsMapSpec_write     = argsMapSpec_writeAux
let argsMapSpec_writeBang = argsMapSpec_writeAux

let execute_write     = command_writeAux false
let execute_writeBang = command_writeAux true

// writeQuit

let argsMapSpec_writeQuit: ArgsMapSpec = (0, [| |])

let execute_writeQuit context (_argsMap: ArgsMap) =
    while context.textArea.IsAnyBufferChanged () do
        context.textArea.ToFirstChangedBuffer ()
        context.textArea.WriteFile ()
    true

// edit, edit!

let argsMapSpec_editAux = (1, [| "strictEncoding"; "encoding"; "filePath" |])

let private command_editAux quite context (argsMap: ArgsMap) =
    let strictEncoding = argsMap["strictEncoding"]
    let encoding       = argsMap["encoding"      ]
    let filePath       = argsMap["filePath"      ] |> Option.get

    if context.textArea.HasBufferWithFilePath filePath then
        context.textArea.ToBufferWithFilePath filePath
        if not quite then
            context.userMessages.RegisterMessage (
                formatMessage INFO_FILE_ALREADY_OPENED filePath
            )
    else
        context.textArea.EditFile filePath encoding strictEncoding quite

    false

let argsMapSpec_edit     = argsMapSpec_editAux
let argsMapSpec_editBang = argsMapSpec_editAux

let execute_edit     = command_editAux false
let execute_editBang = command_editAux true

// view

let argsMapSpec_view = (1, [| "strictEncoding"; "encoding"; "filePath" |])

let execute_view context (argsMap: ArgsMap) =
    let strictEncoding = argsMap["strictEncoding"]
    let encoding       = argsMap["encoding"      ]
    let filePath       = argsMap["filePath"      ] |> Option.get

    if context.textArea.HasBufferWithFilePath filePath then
        context.textArea.ToBufferWithFilePath filePath
        context.userMessages.RegisterMessage (
            formatMessage INFO_FILE_ALREADY_OPENED filePath
        )
    else
        context.textArea.ViewFile filePath encoding strictEncoding

    false

// reload, reload!

let argsMapSpec_reloadAux: ArgsMapSpec = (0, [| |])

let command_reloadAux quite context (_argsMap: ArgsMap) =
    if not quite && context.textArea.IsBufferChanged then
        context.userMessages.RegisterMessage WARNING_NO_WRITE_SINCE_LAST_CHANGE
    else
        context.textArea.ReloadFile ()

    false

let argsMapSpec_reload     = argsMapSpec_reloadAux
let argsMapSpec_reloadBang = argsMapSpec_reloadAux

let execute_reload     = command_reloadAux false
let execute_reloadBang = command_reloadAux true

// extract

let argsMapSpec_extract = (0, [| "filePath" |])

let execute_extract context (argsMap: ArgsMap) =
    let filePath =
        argsMap["filePath"] |> Option.defaultWith (
            fun () -> context.textArea.FilePath + ".ex"
        )

    if context.textArea.HasBufferWithFilePath filePath then
        context.textArea.ToBufferWithFilePath filePath
        context.userMessages.RegisterMessage (
            formatMessage INFO_FILE_ALREADY_OPENED filePath
        )
    else
        context.textArea.Extract filePath

    false

// bufferDelete, bufferDelete!

let argsMapSpec_bufferDeleteAux: ArgsMapSpec = (0, [| |])

let command_bufferDeleteAux quite context (_argsMap: ArgsMap) =
    if not quite && context.textArea.IsBufferChanged then
        context.userMessages.RegisterMessage WARNING_NO_WRITE_SINCE_LAST_CHANGE
    else
        context.textArea.DeleteBuffer ()

    false

let argsMapSpec_bufferDelete     = argsMapSpec_bufferDeleteAux
let argsMapSpec_bufferDeleteBang = argsMapSpec_bufferDeleteAux

let execute_bufferDelete     = command_bufferDeleteAux false
let execute_bufferDeleteBang = command_bufferDeleteAux true

// bufferNext

let argsMapSpec_bufferNext: ArgsMapSpec = (0, [| |])

let execute_bufferNext context (_argsMap: ArgsMap) =
    context.textArea.ToNextBuffer ()
    false

// bufferPrev

let argsMapSpec_bufferPrev: ArgsMapSpec = (0, [| |])

let execute_bufferPrev context (_argsMap: ArgsMap) =
    context.textArea.ToPrevBuffer ()
    false
