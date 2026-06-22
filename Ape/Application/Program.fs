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
open Settings
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

let isWindowSizeOK windowSize =
       windowSize.width  > 0
    && windowSize.height > 0

let mutable isConsoleOK = isWindowSizeOK windowSize

let applyWindowSize windowSize =
    if isWindowSizeOK windowSize then
        consoleContextRef.Value <-
            makeConsoleContext windowSize

        Console.CursorVisible <- false
        Console.Clear ()
        isConsoleOK <- true
    else
        isConsoleOK <- false

/// Dispatches a single key.
let dispatchKey mode keyPrefix key isToConsole keySequenceSleep =
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
        | Performed mode nextMode ->
            if isToConsole then
                renderAreas nextMode keyPrefix areasToRender
            if keySequenceSleep > 0 then
                System.Threading.Thread.Sleep keySequenceSleep
                
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

/// Dispatches a key sequence recursively.
[<TailCall>]
let rec dispatchKeySequence
    mode keyPrefix keys recursionLimit recursions isToConsole keySequenceSleep =

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
                    isToConsole keySequenceSleep
            else
                userMessages.RegisterMessage (
                    formatMessage ERROR_RECURSION_LIMIT_WAS_REACHED recursionLimit
                )
                (Continue mode, None)

        | None ->
            match dispatchKey mode keyPrefix key isToConsole keySequenceSleep with
            | Continue mode', keyPrefix ->
                if not userMessages.HasErrorOrWarningMessage then
                    dispatchKeySequence
                        mode' keyPrefix keysRest recursionLimit recursions
                        isToConsole keySequenceSleep
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
        let mainContextRef   = textArea.CurrentMainContextRef
        let keySequenceSleep = mainContextRef.Value.keySequenceSleep
        let recursionLimit   = mainContextRef.Value.recursionLimit
        
        // Render every step of the key sequence to the console ?
        let isToConsole = keySequenceSleep > 0

        let result =
            dispatchKeySequence
                mode None keys' recursionLimit 1
                isToConsole keySequenceSleep
        
        match result with
        | Continue mode', keyPrefix ->
            if not isToConsole then
                // The steps of the key sequence were not rendered yet.
                rerender mode' keyPrefix
            // Continue application.
            (Continue mode', keyPrefix)
        | Exit, _ ->
            // Exit application.
            (Exit, None)

    | None ->
        let keySequenceSleep = 0
        
        // Render the single step of the key to the console.
        let isToConsole = true
        
        dispatchKey mode keyPrefix key isToConsole keySequenceSleep

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
            
    | FatalExceptionCaught ex ->
        userMessages.RegisterException ex
        
    | ExceptionCaught ex ->
        userMessages.RegisterException ex
        if isConsoleOK then
            rerender mode keyPrefix
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

let getBoolSetting name =
    let settings = textArea.CurrentSettings

    getValueBool settings name  

let setBoolSettingAsFixed name value =
    let settings = textArea.CurrentSettings
    
    setValueAsFixed settings (Some Scope.``global``) name
        (if value then "true" else "false") |> ignore

let getClipboardTypeSetting name =
    let settings = textArea.CurrentSettings

    getValueClipboardType settings name
    
let setClipboardTypeSettingAsFixed name value =
    let settings = textArea.CurrentSettings
    
    setValueAsFixed settings (Some Scope.``global``) name
        (value.ToString ()) |> ignore

let applySettings () =
    let useKittyKeys = getBoolSetting Name.useKittyKeys
    let useKittyKeys' = consoleInputSource.Initialize useKittyKeys 
    setBoolSettingAsFixed Name.useKittyKeys useKittyKeys'
    
    let clipboardType = getClipboardTypeSetting Name.clipboardType
    consoleInterop.SetClipboardType clipboardType
    setClipboardTypeSettingAsFixed Name.clipboardType clipboardType

let mainAux argv =
    consoleInterop.DisableExitOnCtrlC ()
    consoleInterop.SetConsoleOutputMode ()

    Console.InputEncoding  <- Text.Encoding.UTF8
    Console.OutputEncoding <- Text.Encoding.UTF8
    Console.CursorVisible  <- false

    match ColorUtils.initResult with
    | Ok ()   ->
        ()
    | Error e ->
        userMessages.RegisterMessage (makeErrorMessage e)

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

    //let stopWatch = System.Diagnostics.Stopwatch();
    //stopWatch.Start();

    if not userMessages.HasErrorMessage then
        match options with
        | Ok x ->
            match x.filePath with
            // To open a specific file for editing ?
            | Some filePath when x.edit ->
                tryCall userMessages (fun () ->
                    textArea.EditFile filePath x.encoding x.strictEncoding false
                    textArea.ToFirstBuffer ()
                    textArea.DeleteBuffer ()
                ) |> ignore
            // To open a specific file for viewing ?
            | Some filePath when x.view ->
                tryCall userMessages (fun () ->
                    textArea.ViewFile filePath x.encoding x.strictEncoding true
                    textArea.ToFirstBuffer ()
                    textArea.DeleteBuffer ()
                ) |> ignore
            // To open a specific file as extract ?
            | Some filePath (* when x.extract *) ->
                tryCall userMessages (fun () ->
                    textArea.ExtractFile filePath x.encoding x.strictEncoding false
                    textArea.ToFirstBuffer ()
                    textArea.DeleteBuffer ()
                ) |> ignore
            | None ->
                let result = textArea.SetBufferSettings x.encoding x.strictEncoding (Some "false")
                match result with
                | Error e -> userMessages.RegisterMessage (makeErrorMessage e)
                | Ok ()   -> ()

        | Error _ ->
            ()

    //stopWatch.Stop ()
    //let elapsedMs = stopWatch.ElapsedMilliseconds;
    //UserMessages.logInfo (elapsedMs.ToString ())

    applySettings ()    
    runMainLoop ()

    (textArea   :> IDisposable).Dispose ()
    (prompt     :> IDisposable).Dispose ()
    (statusArea :> IDisposable).Dispose ()
    (renderer   :> IDisposable).Dispose ()

    consoleInputSource.Deinitialize ()

    Console.ForegroundColor <- bckFg
    Console.BackgroundColor <- bckBg
    Console.Clear ()
    Console.CursorVisible   <- true

    0

[<EntryPoint>]
let main argv =
    try
        mainAux argv
    with
        | ex ->
            userMessages.RegisterException ex
            1
