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
open TextAreaFileReader
open TextAreaFileWriter
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

// prompt history editing mechanism

[<Literal>]
let RegexHistoryFileName   = "<regex_history>"
[<Literal>]
let CommandHistoryFileName = "<command_history>"

let PromptHistoryRead filePath =
    match filePath with
    | RegexHistoryFileName   ->
        Some (prompt.RegexHistory.GetAll ())
    | CommandHistoryFileName ->
        Some (prompt.CommandHistory.GetAll ())        
    | _ ->
        None
        
let PromptHistoryWrite filePath lines =
    match filePath with
    | RegexHistoryFileName   ->
        prompt.RegexHistory.SetAll lines
        Some ()
    | CommandHistoryFileName ->
        prompt.CommandHistory.SetAll lines
        Some ()
    | _ ->
        None
    
TextAreaFileReader.FakeRead  <- PromptHistoryRead
TextAreaFileWriter.FakeWrite <- PromptHistoryWrite

// recording

let mutable isRecording = false

// rendering

let render mode keyPrefix areasToRender =
    renderer.Render mode keyPrefix isRecording areasToRender

let renderIndicators mode keyPrefix =
    let areasToRender = renderer.GetAreasToRenderIndicators mode
    renderer.Render mode keyPrefix isRecording areasToRender

let renderALl mode keyPrefix =
    let areasToRender = renderer.GetAreasToRenderAll mode
    renderer.Render mode keyPrefix isRecording areasToRender

// main application loop

/// Result of calling dispatchKey.
type DispatchKeyResult =
    | DispatchKeySucceeded of nextMode: Mode  // dispatchKey succeeded
    | DispatchKeyFailed    of nextMode: Mode  // dispatchKey failed
    | DispatchKeyAppExit                      // exit the application immediately

/// State of the application.
type AppState =
    | AppContinue of nextMode: Mode  // continue the application
    | AppExit                        // exit the application immediately

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

let inline handleKeySequenceSleep keySequenceSleep =
    if keySequenceSleep > 0 then
        Threading.Thread.Sleep keySequenceSleep    

type DispatchKeyInput = {
    mode:             Mode
    keyPrefix:        Key option
    isToConsole:      bool
    keySequenceSleep: int
    recursionLimit:   int
    recursions:       int
}

/// Toggles key prefix according to keyPrefix.
let toggleKeyPrefix keyPrefix (input: DispatchKeyInput) =
    if input.isToConsole then
        renderIndicators input.mode keyPrefix
    handleKeySequenceSleep input.keySequenceSleep

    (DispatchKeySucceeded input.mode, keyPrefix)

/// Dispatches a key.
let dispatchKey key (input: DispatchKeyInput) =
    match input.keyPrefix, key with
    // Set key prefix.
    | None, key
        when isKeyPrefix key && isKeyMappingsMode input.mode ->

        toggleKeyPrefix (Some key) input

    // Cancel key prefix.
    | Some _keyPrefix, Key.NoModif InputKey.Escape ->
        toggleKeyPrefix None input

    // Ignore unmapped key with key prefix.
    | Some _keyPrefix, _key ->
        (DispatchKeySucceeded input.mode, input.keyPrefix)

    // Dispatch key without key prefix.
    | None, key ->
        let result, areasToRender =
            KeyDispatching.dispatchKey
                userMessages textArea prompt registers input.mode key

        match result with
        | Performed input.mode nextMode ->
            let returnValue =
                if userMessages.HasErrorOrWarningMessage then
                    (DispatchKeyFailed nextMode, None)
                else
                    (DispatchKeySucceeded nextMode, None)

            if input.isToConsole then
                render nextMode input.keyPrefix areasToRender
            handleKeySequenceSleep input.keySequenceSleep
                
            handleTextAreaUndo textArea input.mode nextMode
            handlePromptUndo   prompt   input.mode nextMode
            
            returnValue

        | DispatchingResult.NoChange ->
            (DispatchKeySucceeded input.mode, None)
        | DispatchingResult.Exit     ->
            (DispatchKeyAppExit, None)
        | _                          ->
            invalidOp ""

/// Dispatches a single key as opposed to a sequence of keys.
let dispatchSingleKey key (input: DispatchKeyInput) =
    match dispatchKey key input with
    | DispatchKeySucceeded mode', keyPrefix' ->
        (AppContinue mode', keyPrefix')
    | DispatchKeyFailed mode', keyPrefix' ->
        (AppContinue mode', keyPrefix')
    | DispatchKeyAppExit, keyPrefix' ->
        (AppExit, keyPrefix')

/// Dispatches a sequence of keys.
[<TailCall>]
let rec dispatchKeySequence keys (input: DispatchKeyInput) =
    match keys with
    | [] ->
        (AppContinue input.mode, input.keyPrefix)

    | key :: keysRest ->
        let keyMappings = textArea.CurrentKeyMappings

        match getKeySequence keyMappings input.mode input.keyPrefix key with
        | Some keys' ->
            if input.recursions < input.recursionLimit then
                // Replace the first key in keys with keySeq mapped to it.
                dispatchKeySequence (keys' @ keysRest) {
                    input with keyPrefix  = None
                               recursions = input.recursions + 1
                }
            else
                userMessages.RegisterMessage (
                    formatMessage ERROR_RECURSION_LIMIT_WAS_REACHED input.recursionLimit
                )
                
                (AppContinue input.mode, None)

        | None ->
            match dispatchKey key input with
            | DispatchKeySucceeded mode', keyPrefix' ->
                dispatchKeySequence keysRest {
                    input with mode      = mode'
                               keyPrefix = keyPrefix'
                }
            | DispatchKeyFailed mode', keyPrefix' ->
                (AppContinue mode', keyPrefix')                
            | DispatchKeyAppExit, keyPrefix' ->
                (AppExit, keyPrefix')

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
            dispatchKeySequence keys' {
                mode             = mode
                keyPrefix        = None
                isToConsole      = isToConsole
                keySequenceSleep = keySequenceSleep
                recursionLimit   = recursionLimit
                recursions       = 1
            }

        match result with
        | AppContinue mode', keyPrefix' ->
            if not isToConsole then
                // The steps of the key sequence were not rendered yet.
                renderALl mode' keyPrefix'                
        | AppExit, _keyPrefix' ->
            ()
            
        result

    | None ->
        let keySequenceSleep = 0
        
        // Render the single step of the key to the console.
        let isToConsole = true

        dispatchSingleKey key {
            mode             = mode
            keyPrefix        = keyPrefix
            isToConsole      = isToConsole
            keySequenceSleep = keySequenceSleep
            recursionLimit   = 0
            recursions       = 0
        }

let toToggleRecording keyPrefix key =
    keyPrefix = None && key = Ctrl InputKey.Q

/// Handles recording. Returns true if recording was toggled.
let handleRecording keyPrefix key =
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
            renderALl mode keyPrefix
        mainLoop mode keyPrefix

    | KeyboardInputRead key ->
        if isConsoleOK then
            if handleRecording keyPrefix key then
                renderIndicators mode keyPrefix
                mainLoop mode keyPrefix
            else
                let result = dispatchInputKey mode keyPrefix key

                match result with
                | AppContinue mode', keyPrefix' ->
                    mainLoop mode' keyPrefix'
                | AppExit, _keyPrefix' ->
                    ()
        else
            mainLoop mode keyPrefix
            
    | FatalExceptionCaught ex ->
        userMessages.RegisterException ex
        
    | ExceptionCaught ex ->
        userMessages.RegisterException ex
        if isConsoleOK then
            renderALl mode keyPrefix
        mainLoop mode keyPrefix
        
[<TailCall>]
let rec runMainLoop () =
    let wasExceptionCaught = tryCall userMessages (
        fun () ->
            let mode = NormalMode NormalMainState
            let keyPrefix = None
            
            if isConsoleOK then
                renderALl mode keyPrefix
                
            mainLoop mode keyPrefix
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
