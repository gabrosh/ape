open System

open Common
open ConsoleKeys
open ConsoleInputSource
open ConsoleRenderer
open ConsoleInterop_Common
open ConsoleInterop_Specific
open Context
open DataTypes
open KeyDispatching
open KeysStrings
open Registers
open UndoRedo
open UserMessages
open WrappedRef

// auxiliary objects

let consoleInputSource = ConsoleInputSource ()
let windowSize         = consoleInputSource.GetWindowSize ()
let consoleContextRef  = WrappedRef (makeConsoleContext windowSize)

let userMessages       = UserMessages ()
let globalSettings     = Settings.makeGlobalSettings ()
let globalKeyMappings  = KeyMappings.makeGlobalKeyMappings ()
let registers          = Registers.Registers ()
let keysRecorder       = KeysRecorder.KeysRecorder registers

// areas and console renderer

let textArea = new TextArea.TextArea (
    consoleContextRef, userMessages, globalSettings, globalKeyMappings, registers,
    IdentCompletion.getCompletions false
)

let mainContextRef = textArea.CurrentMainContextRef

let prompt = new Prompt.Prompt (
    consoleContextRef, mainContextRef, userMessages, registers,
    CommandCompletion.getCompletions,
    IdentCompletion.getCompletions true (
        fun () -> textArea.LinesForCompletion
    )
)
let statusArea = new StatusArea.StatusArea (
    mainContextRef
)
let renderer = new ConsoleRenderer (
    mainContextRef, userMessages, textArea, prompt, statusArea
)

// catching exceptions

/// Performs action and catches most types of exceptions thrown
/// by the action. Returns false if no exception was thrown.
let tryCall (userMessages: UserMessages) action =
    try
        action ()
        false
    with
        | :? System.AccessViolationException
        | :? System.OutOfMemoryException
        | :? System.InsufficientExecutionStackException
        | :? System.InsufficientMemoryException
        | :? System.StackOverflowException ->
            reraise ()
        | ex ->
            userMessages.RegisterException ex
            true

// recording

let mutable isRecording = false

// rendering

let renderAreas mode keyPrefix areasToRender =
    renderer.Render mode keyPrefix isRecording areasToRender

let rerender mode keyPrefix =
    let areasToRender = renderer.GetAreasToRerender mode
    renderer.Render mode keyPrefix isRecording areasToRender

// main application loop

type ApplicationState =
    | Continue of Mode  // continue running in specified mode
    | Exit              // exit the application immediately

let mutable isConsoleOK = true

let applyWindowSize windowSize =
    if windowSize.width > 0 then
        consoleContextRef.Value <-
            makeConsoleContext windowSize

        Console.CursorVisible <- false
        Console.Clear ()
        isConsoleOK <- true
    else
        isConsoleOK <- false

/// Dispatches a single key.
let dispatchKey isToConsole mode keyPrefix key =
    match keyPrefix, key with
    | None, key
        when isKeyPrefix key && isKeyMappingsMode mode ->

        let keyPrefix' = Some key
        if isToConsole then
            rerender mode keyPrefix'
        (Continue mode, keyPrefix')

    | Some _, Key.NoModif InputKey.Escape ->
        let keyPrefix' = None
        if isToConsole then
            rerender mode keyPrefix'
        (Continue mode, keyPrefix')

    | None, key ->
        let result, areasToRender =
            KeyDispatching.dispatchKey
                userMessages textArea prompt registers mode key

        match result with
        | KeyDispatching.Performed mode nextMode ->
            if isToConsole then
                renderAreas nextMode keyPrefix areasToRender
            handleTextAreaUndo textArea mode nextMode
            handlePromptUndo   prompt   mode nextMode
            (Continue nextMode, None)

        | DispatchingResult.NoChange ->
            (Continue mode, None)
        | DispatchingResult.Exit     ->
            (Exit, None)
        | _                          ->
            invalidOp ""

    | _ ->
        (Continue mode, keyPrefix)

/// Dispatches a key sequence recusively.
[<TailCall>]
let rec dispatchKeySequence mode keyPrefix keys recursionLimit recursions =
    match keys with
    | [] ->
        (Continue mode, keyPrefix)

    | key :: keysRest ->
        let keyMappings = textArea.CurrentKeyMappings

        match getKeySequence keyMappings mode keyPrefix key with
        | Some keys' ->
            if recursions < recursionLimit then
                // Replace the first key in keys with keySeq mapped to it.
                dispatchKeySequence
                    mode None (keys' @ keysRest) recursionLimit (recursions + 1)
            else
                userMessages.RegisterMessage (
                    formatMessage ERROR_RECURSION_LIMIT_WAS_REACHED recursionLimit
                )
                (Continue mode, None)

        | None ->
            match dispatchKey false mode keyPrefix key with
            | Continue mode', keyPrefix ->
                if not userMessages.HasErrorOrWarningMessage then
                    dispatchKeySequence
                        mode' keyPrefix keysRest recursionLimit recursions
                else
                    // Stop the iteration prematurely, continue application.
                    (Continue mode', keyPrefix)
            | Exit, _ ->
                // Stop the iteration prematurely, exit application.
                (Exit, None)

/// If given key is mapped to some key sequence, it dispatches
/// this key sequence, otherwise it dispatches given key itself.
let dispatchInputKey mode keyPrefix key =
    let keyMappings = textArea.CurrentKeyMappings

    match getKeySequence keyMappings mode keyPrefix key with
    | Some keys' ->
        let mainContextRef = textArea.CurrentMainContextRef
        let recursionLimit = mainContextRef.Value.recursionLimit

        match dispatchKeySequence mode None keys' recursionLimit 1 with
        | Continue mode', keyPrefix ->
            rerender mode' keyPrefix
            // Continue application.
            (Continue mode', keyPrefix)
        | Exit, _ ->
            // Exit application.
            (Exit, None)

    | None ->
        dispatchKey true mode keyPrefix key

let toToggleRecording keyPrefix key =
    keyPrefix = None && key = Ctrl InputKey.Q

/// Returns true if recording was toggled.
let checkRecording keyPrefix key =
    if toToggleRecording keyPrefix key then
        if isRecording then
            keysRecorder.MoveKeysToRegister (
                SelectedRegister (false, recordingRegisterName)
            )
        isRecording <- not isRecording
        true
    else
        if isRecording then
            keysRecorder.AppendKey key
        false

[<TailCall>]
let rec mainLoop mode keyPrefix =
    let input = consoleInputSource.TakeInput ()

    match input with
    | WindowSizeChanged windowSize ->
        applyWindowSize windowSize
        if isConsoleOK then
            rerender mode keyPrefix
        mainLoop mode keyPrefix

    | KeyboardInputRead key ->
        if isConsoleOK then
            if checkRecording keyPrefix key then
                rerender mode keyPrefix
                mainLoop mode keyPrefix
            else
                let result = dispatchInputKey mode keyPrefix key

                match result with
                | Continue mode, keyPrefix ->
                    mainLoop mode keyPrefix
                | Exit, _ ->
                    ()
        else
            mainLoop mode keyPrefix

[<TailCall>]
let rec runMainLoop () =
    let wasExceptionCaught = tryCall userMessages (
        fun () ->
            let mode = NormalMode NormalMainState
            if isConsoleOK then
                renderAreas mode None toRenderTextAndStatus
            mainLoop mode None
    )

    if wasExceptionCaught then
        textArea.UndoCorruptedState ()
        prompt.UndoCorruptedState ()
        runMainLoop ()
    else
        ()

// main function

[<EntryPoint>]
let main argv =
    consoleInterop.DisableExitOnCtrlC ()
    consoleInterop.SetConsoleOutputMode ()

    Console.InputEncoding  <- Text.Encoding.UTF8
    Console.OutputEncoding <- Text.Encoding.UTF8
    Console.CursorVisible  <- false

    let options = AppOptions.getAppOptions (argv |> Array.toList)

    match options with
    | Ok options ->
        tryCall userMessages (fun () ->
            CommandExecution.executeCfgFile userMessages textArea registers
                options.cfgName
        ) |> ignore
    | Error e    ->
        userMessages.RegisterMessage (makeErrorMessage e)

    let bckFg = Console.ForegroundColor
    let bckBg = Console.BackgroundColor
    let colorScheme = mainContextRef.Value.colorScheme
    Console.Write (getColorsSequence colorScheme.normal)
    Console.Clear ()

    if not userMessages.HasErrorMessage then
        match options with
        | Ok x ->
            match x.filePath with
            // To open a specific file ?
            | Some filePath ->
                tryCall userMessages (fun () ->
                    textArea.EditOrViewFile filePath x.encoding x.strictEncoding x.isReadOnly
                ) |> ignore
            | None           ->
                textArea.SetBufferSettings x.encoding x.strictEncoding x.isReadOnly
        | Error _ ->
            ()

    runMainLoop ()

    (textArea   :> IDisposable).Dispose ()
    (prompt     :> IDisposable).Dispose ()
    (statusArea :> IDisposable).Dispose ()
    (renderer   :> IDisposable).Dispose ()

    Console.ForegroundColor <- bckFg
    Console.BackgroundColor <- bckBg
    Console.Clear ()
    Console.CursorVisible   <- true

    0
