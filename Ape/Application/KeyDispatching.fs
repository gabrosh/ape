module KeyDispatching

open System

open Common
open ConsoleKeys
open DataTypes
open Prompt
open PromptExecution
open Registers
open TextArea
open UserMessages

type DispatchingResult =
    | NextMode of Mode  // command performed, change mode
    | KeepMode          // command performed, keep the old mode
    | NoChange          // nothing performed, keep the old mode
    | Exit              // exit the application

let (|Performed|_|) mode result =
    match result with
    | NextMode nextMode -> Some nextMode
    | KeepMode          -> Some mode
    | _                 -> None

type AreaToRender =
    | ToRenderStatus
    | ToRenderText
    | ToRenderTextCompletions
    | ToRenderPrompt
    | ToRenderPromptCompletions

let toRenderNothing           = Set<_> []
let toRenderStatus            = Set<_> [ToRenderStatus]
let toRenderText              = Set<_> [ToRenderText]
let toRenderTextAndStatus     = Set<_> [ToRenderText; ToRenderStatus]
let toRenderTextCompletions   = Set<_> [ToRenderTextCompletions]
let toRenderPrompt            = Set<_> [ToRenderPrompt]
let toRenderPromptAndStatus   = Set<_> [ToRenderPrompt; ToRenderStatus]
let toRenderPromptCompletions = Set<_> [ToRenderPromptCompletions]

// functions related to key mappings

let getKeyMappingsMode mode =
    match mode with
    | NormalMode NormalMainState ->
        Some KeyMappings.Mode.normal

    | InsertMode InsertMainState ->
        Some KeyMappings.Mode.insert

    | PromptNormalMode (_, PromptNormalMainState)   ->
        Some KeyMappings.Mode.promptNormal

    | PromptInsertMode (_, PromptInsertMainState) ->
        Some KeyMappings.Mode.promptInsert

    | _ ->
        None

let isKeyMappingsMode mode =
    getKeyMappingsMode mode |> Option.isSome

let getKeySequence keyMappings mode keyPrefix key =
    match getKeyMappingsMode mode with
    | Some mode' ->
        KeyMappings.getKeySequence keyMappings (mode', keyPrefix, key)
            |> Option.map List.ofArray
    | None       ->
        None

// auxiliary functions for key dispatching

let private (|IsRegisterChar|_|) key =
    match keyToChar key with
    | Some c when isRegisterChar c
        -> Some c
    | _
        -> None

let private inputKeyToChar (inputKey: InputKey) =
    char (LanguagePrimitives.EnumToValue inputKey)

let private parseCharAsDigit c =
    Int32.Parse (c.ToString ())

let private (|IsDigit|_|) key =
    match key with
    | NoModif inputKey ->
        let c = inputKeyToChar inputKey
        if not (System.Char.IsDigit c) then
            None
        else
            let digit = parseCharAsDigit c
            Some digit
    | _ ->
        None

let private (|IsDigitNon0|_|) key =
    match key with
    | NoModif inputKey ->
        let c = inputKeyToChar inputKey
        if not (System.Char.IsDigit c) then
            None
        else
            let digit = parseCharAsDigit c
            if digit = 0 then
                None
            else
                Some digit
    | _ ->
        None

let private addCountDigit count digit =
    let newCount = 10 * count + digit
    if newCount / 10 = count then
        NormalMode (CountState newCount)
    else
        NormalMode (CountState count)

let private deleteCountDigit count =
    let newCount = count / 10
    if newCount <> 0 then
        NormalMode (CountState newCount)
    else
        NormalMode NormalMainState

let private selectRegister count isUpper =
    NormalMode (RegisterState (count, ToSelectRegisterState isUpper))

let private selectedRegister count isUpper c =
    NormalMode (RegisterState (count, SelectedRegisterState (isUpper, c)))

let private findChar count isExtending state =
    NormalMode (FindCharState (count, isExtending , state))

let private enterPrompt (prompt: Prompt) promptType =
    prompt.WhenEntering promptType
    PromptInsertMode (promptType, PromptInsertMainState)

let private promptSelectRegister promptType isUpper =
    PromptNormalMode (promptType, PromptRegisterState (ToSelectRegisterState isUpper))

let private promptSelectedRegister promptType isUpper c =
    PromptNormalMode (promptType, PromptRegisterState (SelectedRegisterState (isUpper, c)))

// key dispatching in various editor modes

let private dispatchNormalMain (textArea: TextArea) (prompt: Prompt) key =
    let mutable areasToRender = toRenderStatus

    let result =
        match key with
        | NoModif     InputKey.Colon        -> areasToRender <- toRenderPrompt
                                               NextMode (enterPrompt prompt CommandPrompt)

        | NoModif     InputKey.Slash        -> areasToRender <- toRenderPrompt
                                               NextMode (enterPrompt prompt (SearchPrompt (true , false)))

        | Alt         InputKey.Slash        -> areasToRender <- toRenderPrompt
                                               NextMode (enterPrompt prompt (SearchPrompt (false, false)))

        | NoModif     InputKey.Question     -> areasToRender <- toRenderPrompt
                                               NextMode (enterPrompt prompt (SearchPrompt (true , true )))

        | Alt         InputKey.Question     -> areasToRender <- toRenderPrompt
                                               NextMode (enterPrompt prompt (SearchPrompt (false, true )))

        | NoModif     InputKey.S            -> areasToRender <- toRenderPrompt
                                               NextMode (enterPrompt prompt SelectPrompt)

        | Shift       InputKey.S            -> areasToRender <- toRenderPrompt
                                               NextMode (enterPrompt prompt KeepPrompt)

        | ShiftAlt    InputKey.S            -> areasToRender <- toRenderPrompt
                                               NextMode (enterPrompt prompt DiscardPrompt)

        | IsDigitNon0 digit                 -> NextMode (addCountDigit 0 digit)

        | OptShift    InputKey.G s          -> NextMode (NormalMode (GoToState s))
        | NoModif     InputKey.V            -> NextMode (NormalMode ViewState)

        | OptShiftAlt InputKey.F s          -> NextMode (findChar 0 s LeftToCharState    )
        | OptShiftAlt InputKey.T s          -> NextMode (findChar 0 s LeftUntilCharState )
        | OptShift    InputKey.F s          -> NextMode (findChar 0 s RightToCharState   )
        | OptShift    InputKey.T s          -> NextMode (findChar 0 s RightUntilCharState)

        | NoModif     InputKey.R            -> NextMode (NormalMode (FillWithCharState 0))

        | NoModif     InputKey.Apostrophe   -> NextMode (selectRegister 0 false)
        | NoModif     InputKey.Quotation    -> NextMode (selectRegister 0 true )

        | _ when textArea.PK_NormalMain key 1
                                            -> areasToRender <- toRenderTextAndStatus
                                               KeepMode

        | _ when textArea.PK_NormalMain_BeforeInsert key
                                            -> areasToRender <- toRenderTextAndStatus
                                               NextMode (InsertMode InsertMainState)

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchNormalCount (textArea: TextArea) key count =
    let mutable areasToRender = toRenderStatus

    let result =
        match key with
        | NoModif     InputKey.Escape       -> NextMode (NormalMode NormalMainState)

        | IsDigit digit                     -> NextMode (addCountDigit count digit)

        | NoModif     InputKey.Backspace    -> NextMode (deleteCountDigit count)

        | NoModif     InputKey.V
            when count <= 1                 -> NextMode (NormalMode ViewState)

        | OptShiftAlt InputKey.F s          -> NextMode (findChar count s LeftToCharState    )
        | OptShiftAlt InputKey.T s          -> NextMode (findChar count s LeftUntilCharState )
        | OptShift    InputKey.F s          -> NextMode (findChar count s RightToCharState   )
        | OptShift    InputKey.T s          -> NextMode (findChar count s RightUntilCharState)

        | NoModif     InputKey.Apostrophe   -> NextMode (selectRegister count false)
        | NoModif     InputKey.Quotation    -> NextMode (selectRegister count true )

        | NoModif     InputKey.R
            when count <= 1                 -> NextMode (NormalMode (FillWithCharState count))

        | _ when textArea.PK_NormalCount key count
                                            -> areasToRender <- toRenderTextAndStatus
                                               NextMode (NormalMode NormalMainState)

        | _ when textArea.PK_NormalMain key count
                                            -> areasToRender <- toRenderTextAndStatus
                                               NextMode (NormalMode NormalMainState)

        | _ when count <= 1 &&
                 textArea.PK_NormalMain_BeforeInsert key
                                            -> areasToRender <- toRenderTextAndStatus
                                               NextMode (InsertMode InsertMainState)

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchNormalRegister (textArea: TextArea) key count state =
    let mutable areasToRender = toRenderStatus

    let result =
        match state with

        | ToSelectRegisterState isUpper
         -> match key with
            | NoModif InputKey.Escape       -> NextMode (NormalMode NormalMainState)

            | IsRegisterChar c              -> NextMode (selectedRegister count isUpper c)

            | _                             -> areasToRender <- toRenderNothing
                                               NoChange

        | SelectedRegisterState (isUpper, c)
         -> match key with
            | NoModif InputKey.Escape       -> NextMode (NormalMode NormalMainState)
            | NoModif InputKey.Backspace    -> NextMode (selectRegister count isUpper)

            | _ when textArea.PK_NormalRegister key (max 1 count) (SelectedRegister (isUpper, c))
                                            -> areasToRender <- toRenderTextAndStatus
                                               NextMode (NormalMode NormalMainState)

            | _ when count <= 1 &&
                     textArea.PK_NormalRegister_BeforeInsert key (SelectedRegister (isUpper, c))
                                            -> areasToRender <- toRenderTextAndStatus
                                               NextMode (InsertMode InsertMainState)

            | _                             -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchNormalFindChar (textArea: TextArea) key count isExtending state =
    let mutable areasToRender = toRenderStatus

    let result =
        match key with
        | NoModif InputKey.Escape           -> NextMode (NormalMode NormalMainState)

        | CanBeInserted c                   -> areasToRender <- toRenderTextAndStatus
                                               textArea.PK_NormalFindChar
                                                   c (max 1 count) isExtending state
                                               NextMode (NormalMode NormalMainState)

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchNormalFillWithChar (textArea: TextArea) key =
    let mutable areasToRender = toRenderStatus

    let result =
        match key with
        | NoModif InputKey.Escape           -> NextMode (NormalMode NormalMainState)

        | CanBeInserted c                   -> areasToRender <- toRenderTextAndStatus
                                               textArea.PK_NormalFillWithChar c
                                               NextMode (NormalMode NormalMainState)

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchNormalGoTo (textArea: TextArea) key isExtending =
    let mutable areasToRender = toRenderStatus

    let result =
        match key with
        | NoModif InputKey.Escape           -> NextMode (NormalMode NormalMainState)

        | _ when textArea.PK_NormalGoTo key isExtending
                                            -> areasToRender <- toRenderTextAndStatus
                                               NextMode (NormalMode NormalMainState)

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchNormalView (textArea: TextArea) key =
    let mutable areasToRender = toRenderStatus

    let result =
        match key with
        | NoModif InputKey.Escape           -> NextMode (NormalMode NormalMainState)

        | _ when textArea.PK_NormalView key -> areasToRender <- toRenderTextAndStatus
                                               NextMode (NormalMode NormalMainState)

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchInsertMain (textArea: TextArea) key =
    let mutable areasToRender = toRenderTextAndStatus

    let result =
        match key with
        | NoModif      InputKey.Escape      -> areasToRender <- toRenderStatus
                                               textArea.ClearCompletions ()
                                               NextMode (NormalMode NormalMainState)

        | OptShiftCtrl InputKey.R s         -> areasToRender <- toRenderStatus
                                               textArea.ClearCompletions ()
                                               NextMode (InsertMode (InsertPasteState s))

        | _ when textArea.PK_Insert key     -> textArea.ClearCompletions ()
                                               KeepMode

        | _ when textArea.PK_Completion key -> let isInCompletion' = textArea.IsInCompletion ()
                                               areasToRender <-
                                                   if isInCompletion' then
                                                       toRenderText + toRenderTextCompletions
                                                   else
                                                       toRenderTextAndStatus
                                               KeepMode

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchInsertPaste (textArea: TextArea) key isUpper =
    let mutable areasToRender = toRenderStatus

    let result =
        match key with
        | NoModif InputKey.Escape           -> NextMode (InsertMode InsertMainState)

        | CanBeInserted c                   -> areasToRender <- toRenderTextAndStatus
                                               textArea.PK_InsertPaste isUpper c
                                               NextMode (InsertMode InsertMainState)

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchPromptNormalMain
    (userMessages: UserMessages)
    (textArea: TextArea) (prompt: Prompt) (registers: Registers)
    promptType key =

    let mutable areasToRender = toRenderPrompt

    let result =
        match key with
        | OptShift InputKey.Escape s        -> areasToRender <- toRenderStatus
                                               prompt.WhenLeaving s
                                               NextMode (NormalMode NormalMainState)

        | OptShift InputKey.Enter s         -> areasToRender <- toRenderTextAndStatus
                                               let promptLine = charsToString prompt.Line
                                               let isCurrentFromHistory = prompt.IsCurrentFromHistory
                                               prompt.WhenLeaving s
                                               let toExit =
                                                   executePrompt userMessages textArea registers promptType
                                                                 promptLine isCurrentFromHistory s
                                               if toExit then Exit else NextMode (NormalMode NormalMainState)

        | OptShift InputKey.G s             -> NextMode (PromptNormalMode (promptType, PromptGoToState s))

        | NoModif  InputKey.Apostrophe      -> NextMode (promptSelectRegister promptType false)
        | NoModif  InputKey.Quotation       -> NextMode (promptSelectRegister promptType true )

        | _ when prompt.PK_NormalMain key   -> KeepMode
        | _ when prompt.PK_History key      -> KeepMode

        | _ when prompt.PK_NormalMain_BeforeInsert key
                                            -> NextMode (PromptInsertMode (promptType, PromptInsertMainState))

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchPromptNormalRegister (prompt: Prompt) promptType key state =
    let mutable areasToRender = toRenderPrompt

    let result =
        match state with

        | ToSelectRegisterState isUpper
         -> match key with
            | NoModif InputKey.Escape       -> NextMode (PromptNormalMode (promptType, PromptNormalMainState))

            | IsRegisterChar c              -> NextMode (promptSelectedRegister promptType isUpper c)

            | _                             -> areasToRender <- toRenderNothing
                                               NoChange

        | SelectedRegisterState (isUpper, c)
         -> match key with
            | NoModif InputKey.Escape       -> NextMode (PromptNormalMode (promptType, PromptNormalMainState))
            | NoModif InputKey.Backspace    -> NextMode (promptSelectRegister promptType isUpper)

            | _ when prompt.PK_NormalRegister key (SelectedRegister (isUpper, c))
                                            -> NextMode (PromptNormalMode (promptType, PromptNormalMainState))

            | _ when prompt.PK_NormalRegister_BeforeInsert key (SelectedRegister (isUpper, c))
                                            -> NextMode (PromptInsertMode (promptType, PromptInsertMainState))

            | _                             -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchPromptNormalGoTo (prompt: Prompt) promptType key isExtending =
    let mutable areasToRender = toRenderPrompt

    let result =
        match key with
        | NoModif InputKey.Escape           -> NextMode (PromptNormalMode (promptType, PromptNormalMainState))

        | _ when prompt.PK_NormalGoTo key isExtending
                                            -> NextMode (PromptNormalMode (promptType, PromptNormalMainState))

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchPromptInsertMain
    (userMessages: UserMessages)
    (textArea: TextArea) (prompt: Prompt) (registers: Registers)
    promptType key =

    let mutable areasToRender = toRenderPromptAndStatus

    let result =
        match key with
        | Alt          InputKey.R           -> prompt.ClearCompletions ()
                                               NextMode (PromptNormalMode (promptType, PromptNormalMainState))

        | OptShiftCtrl InputKey.R s         -> prompt.ClearCompletions ()
                                               NextMode (PromptInsertMode (promptType, (PromptInsertPasteState s)))

        | OptShift     InputKey.Escape s    -> areasToRender <- toRenderStatus
                                               prompt.ClearCompletions ()
                                               prompt.WhenLeaving s                                             
                                               NextMode (NormalMode NormalMainState)

        | OptShift     InputKey.Enter s     -> areasToRender <- toRenderTextAndStatus
                                               let promptLine = charsToString prompt.Line
                                               let isCurrentFromHistory = prompt.IsCurrentFromHistory
                                               prompt.ClearCompletions ()
                                               prompt.WhenLeaving s
                                               let toExit =
                                                   executePrompt userMessages textArea registers promptType
                                                                 promptLine isCurrentFromHistory s
                                               if toExit then Exit else NextMode (NormalMode NormalMainState)

        | _ when prompt.PK_Insert key       -> prompt.ClearCompletions ()
                                               KeepMode

        | _ when prompt.PK_History key      -> prompt.ClearCompletions ()
                                               KeepMode

        | _ when prompt.PK_Completion key   -> let isInCompletion' = prompt.IsInCompletion ()
                                               areasToRender <-
                                                   if isInCompletion' then
                                                       toRenderPrompt + toRenderPromptCompletions
                                                   else
                                                       toRenderPromptAndStatus
                                               KeepMode

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

let private dispatchPromptInsertPaste (prompt: Prompt) promptType key isUpper =

    let mutable areasToRender = toRenderPrompt

    let result =
        match key with
        | NoModif InputKey.Escape           -> NextMode (PromptInsertMode (promptType, PromptInsertMainState))

        | CanBeInserted c                   -> prompt.PK_InsertPaste isUpper c
                                               NextMode (PromptInsertMode (promptType, PromptInsertMainState))

        | _                                 -> areasToRender <- toRenderNothing
                                               NoChange

    (result, areasToRender)

/// Dispatches single key to textArea or prompt according to mode.
let dispatchKey userMessages textArea prompt registers mode key =
    match mode with
    | NormalMode NormalMainState
        -> dispatchNormalMain           textArea prompt key

    | NormalMode (CountState count)
        -> dispatchNormalCount          textArea key count

    | NormalMode (FindCharState (count, isExtending, state))
        -> dispatchNormalFindChar       textArea key count isExtending state

    | NormalMode (FillWithCharState _count)
        -> dispatchNormalFillWithChar   textArea key

    | NormalMode (RegisterState (count, state))
        -> dispatchNormalRegister       textArea key count state

    | NormalMode (GoToState isExtending)
        -> dispatchNormalGoTo           textArea key isExtending

    | NormalMode ViewState
        -> dispatchNormalView           textArea key

    | InsertMode InsertMainState
        -> dispatchInsertMain           textArea key

    | InsertMode (InsertPasteState isUpper)
        -> dispatchInsertPaste          textArea key isUpper

    | PromptNormalMode (promptType, PromptNormalMainState)
        -> dispatchPromptNormalMain     userMessages textArea prompt registers promptType key

    | PromptNormalMode (promptType, PromptRegisterState state)
        -> dispatchPromptNormalRegister prompt promptType key state

    | PromptNormalMode (promptType, PromptGoToState isExtending)
        -> dispatchPromptNormalGoTo     prompt promptType key isExtending

    | PromptInsertMode (promptType, PromptInsertMainState)
        -> dispatchPromptInsertMain     userMessages textArea prompt registers promptType key

    | PromptInsertMode (promptType, PromptInsertPasteState isUpper)
        -> dispatchPromptInsertPaste    prompt promptType key isUpper
