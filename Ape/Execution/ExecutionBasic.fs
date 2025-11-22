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

    if context.textArea.HasBufferWithBufferName filePath then
        context.textArea.ToBufferWithBufferName filePath
        context.userMessages.RegisterMessage (
            formatMessage WARNING_BUFFER_ALREADY_OPENED filePath
        )
    else
        context.textArea.ViewFile filePath None (Some "true") false

    false

// quit, quit!

let argsMapSpec_quitAux: ArgsMapSpec = (0, [| |])

let execute_quitAux quite context (_argsMap: ArgsMap) =
    if not quite && context.textArea.IsAnyBufferChanged () then
        context.textArea.ToFirstChangedBuffer ()
        context.userMessages.RegisterMessage
            WARNING_NO_WRITE_SINCE_LAST_CHANGE
        false
    else
        true

let argsMapSpec_quit     = argsMapSpec_quitAux
let argsMapSpec_quitBang = argsMapSpec_quitAux

let execute_quit     = execute_quitAux false
let execute_quitBang = execute_quitAux true

// write, write!

let argsMapSpec_writeAux = (0, [| "filePath" |])

let private execute_writeAux quite context (argsMap: ArgsMap) =
    let filePath = argsMap["filePath"]

    match filePath with
    | Some filePath ->
        if context.textArea.HasBufferWithBufferName filePath then
            context.textArea.ToBufferWithBufferName filePath
            context.userMessages.RegisterMessage (
                formatMessage WARNING_BUFFER_ALREADY_OPENED filePath
            )
        elif not quite && FileUtils.fileExists filePath then
            context.userMessages.RegisterMessage (
                formatMessage WARNING_FILE_ALREADY_EXISTS filePath
            )
        else
            context.textArea.WriteFileAs filePath
    | None ->
        if   not quite
          && not context.textArea.IsBufferChanged
        then
            context.userMessages.RegisterMessage
                WARNING_NO_CHANGE_SINCE_LAST_WRITE
        else
            context.textArea.WriteFile ()

    false

let argsMapSpec_write     = argsMapSpec_writeAux
let argsMapSpec_writeBang = argsMapSpec_writeAux

let execute_write     = execute_writeAux false
let execute_writeBang = execute_writeAux true

// writeQuit

let argsMapSpec_writeQuit: ArgsMapSpec = (0, [| |])

let execute_writeQuit context (_argsMap: ArgsMap) =
    // Break the loop also if writing a file caused an error.
    let mutable hasErrorMessage = false

    while not hasErrorMessage && context.textArea.IsAnyBufferChanged () do
        context.textArea.ToFirstChangedBuffer ()
        context.textArea.WriteFile ()
        hasErrorMessage <- context.userMessages.HasErrorMessage

    // Quit only if writing the files went without an error.
    not hasErrorMessage

// edit, edit!

let argsMapSpec_editAux = (1, [| "strictEncoding"; "encoding"; "filePath" |])

let private execute_editAux quite context (argsMap: ArgsMap) =
    let strictEncoding = argsMap["strictEncoding"]
    let encoding       = argsMap["encoding"      ]
    let filePath       = argsMap["filePath"      ] |> Option.get

    if context.textArea.HasBufferWithBufferName filePath then
        context.textArea.ToBufferWithBufferName filePath
        if not quite then
            context.userMessages.RegisterMessage (
                formatMessage WARNING_BUFFER_ALREADY_OPENED filePath
            )
    else
        context.textArea.EditFile filePath encoding strictEncoding quite

    false

let argsMapSpec_edit     = argsMapSpec_editAux
let argsMapSpec_editBang = argsMapSpec_editAux

let execute_edit     = execute_editAux false
let execute_editBang = execute_editAux true

// view

let argsMapSpec_viewAux = (1, [| "strictEncoding"; "encoding"; "filePath" |])

let private execute_viewAux quite context (argsMap: ArgsMap) =
    let strictEncoding = argsMap["strictEncoding"]
    let encoding       = argsMap["encoding"      ]
    let filePath       = argsMap["filePath"      ] |> Option.get

    if context.textArea.HasBufferWithBufferName filePath then
        context.textArea.ToBufferWithBufferName filePath
        if not quite then
            context.userMessages.RegisterMessage (
                formatMessage WARNING_BUFFER_ALREADY_OPENED filePath
            )
    else
        context.textArea.ViewFile filePath encoding strictEncoding quite

    false

let argsMapSpec_view     = argsMapSpec_viewAux
let argsMapSpec_viewBang = argsMapSpec_viewAux

let execute_view     = execute_viewAux false
let execute_viewBang = execute_viewAux true

// extract

let argsMapSpec_extractAux = (1, [| "strictEncoding"; "encoding"; "filePath" |])

let private execute_extractAux quite context (argsMap: ArgsMap) =
    let strictEncoding = argsMap["strictEncoding"]
    let encoding       = argsMap["encoding"      ]
    let filePath       = argsMap["filePath"      ] |> Option.get

    if context.textArea.HasBufferWithBufferName filePath then
        context.textArea.ToBufferWithBufferName filePath
        if not quite then
            context.userMessages.RegisterMessage (
                formatMessage WARNING_BUFFER_ALREADY_OPENED filePath
            )
    else
        context.textArea.ExtractFile filePath encoding strictEncoding quite

    false

let argsMapSpec_extract     = argsMapSpec_extractAux
let argsMapSpec_extractBang = argsMapSpec_extractAux

let execute_extract     = execute_extractAux false
let execute_extractBang = execute_extractAux true

// reload, reload!

let argsMapSpec_reloadAux: ArgsMapSpec = (0, [| |])

let execute_reloadAux quite context (_argsMap: ArgsMap) =
    if not quite && context.textArea.IsBufferChanged then
        context.userMessages.RegisterMessage
            WARNING_NO_WRITE_SINCE_LAST_CHANGE
    else
        context.textArea.Reload ()

    false

let argsMapSpec_reload     = argsMapSpec_reloadAux
let argsMapSpec_reloadBang = argsMapSpec_reloadAux

let execute_reload     = execute_reloadAux false
let execute_reloadBang = execute_reloadAux true

// bufferName

let argsMapSpec_bufferName = (1, [| "bufferName" |])

let execute_bufferName context (argsMap: ArgsMap) =
    let bufferName = argsMap["bufferName"] |> Option.get

    if context.textArea.HasBufferWithBufferName bufferName then
        context.textArea.ToBufferWithBufferName bufferName
        context.userMessages.RegisterMessage (
            formatMessage WARNING_BUFFER_ALREADY_OPENED bufferName
        )
    else
        context.textArea.BufferName <- bufferName

    false

// bufferDelete, bufferDelete!

let argsMapSpec_bufferDeleteAux: ArgsMapSpec = (0, [| |])

let execute_bufferDeleteAux quite context (_argsMap: ArgsMap) =
    match context.textArea.GetFirstChildBuffer () with
    | Some childBuffer ->
        context.textArea.ToBuffer childBuffer
        context.userMessages.RegisterMessage
            WARNING_CHILD_BUFFER_STILL_OPENED
    | None       ->
        if not quite && context.textArea.IsBufferChanged then
            context.userMessages.RegisterMessage
                WARNING_NO_WRITE_SINCE_LAST_CHANGE
        else
            context.textArea.DeleteBuffer ()

    false

let argsMapSpec_bufferDelete     = argsMapSpec_bufferDeleteAux
let argsMapSpec_bufferDeleteBang = argsMapSpec_bufferDeleteAux

let execute_bufferDelete     = execute_bufferDeleteAux false
let execute_bufferDeleteBang = execute_bufferDeleteAux true

// bufferNext, bufferPrev

let argsMapSpec_bufferNext: ArgsMapSpec = (0, [| |])
let argsMapSpec_bufferPrev: ArgsMapSpec = (0, [| |])

let execute_bufferNext context (_argsMap: ArgsMap) =
    context.textArea.ToNextBuffer ()
    false

let execute_bufferPrev context (_argsMap: ArgsMap) =
    context.textArea.ToPrevBuffer ()
    false

// bufferFirst, bufferLast

let argsMapSpec_bufferFirst: ArgsMapSpec = (0, [| |])
let argsMapSpec_bufferLast:  ArgsMapSpec = (0, [| |])

let execute_bufferFirst context (_argsMap: ArgsMap) =
    context.textArea.ToFirstBuffer ()
    false

let execute_bufferLast context (_argsMap: ArgsMap) =
    context.textArea.ToLastBuffer ()
    false

// bufferBegin, bufferEnd

let argsMapSpec_bufferBegin: ArgsMapSpec = (0, [| |])
let argsMapSpec_bufferEnd:   ArgsMapSpec = (0, [| |])

let execute_bufferBegin context (_argsMap: ArgsMap) =
    context.textArea.BufferToBegin ()
    false

let execute_bufferEnd context (_argsMap: ArgsMap) =
    context.textArea.BufferToEnd ()
    false
