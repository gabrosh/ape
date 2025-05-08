module TextArea

open System

open BuffersRegistry
open Commands.InCommands
open Common
open CompletionItems
open CompletionUtils
open ConsoleKeys
open Context
open DataTypes
open Settings
open TextAreaBuffer
open TextAreaBufferExtract
open UserMessages
open WrappedRef

let (|CanBeInserted|_|) key =
    match keyToChar key with
    | Some c when Utils.canBeDisplayed c || c = Utils.charTab ->
        Some c
    | _ ->
        None

let private performSome (command: InCommand option) f =
    match command with
    | Some command ->
        f command
        true
    | None ->
        false

/// Represents a single text area, which holds the text area buffers corresponding
/// to all opened files. One of the buffers is the current buffer. This class
/// transforms input keys to the corresponding commands and delegates the commands
/// to the current buffer. It also provides several methods related to rendering,
/// Undo/Redo, regular expression matching and buffer operations.

[<Sealed>]
type TextArea (
    myContextRef:        IWrappedRef<ConsoleContext>,
    myUserMessages:      UserMessages,
    inGlobalSettings:    Settings.Settings,
    inGlobalKeyMappings: KeyMappings.KeyMappings,
    inRegisters:         Registers.Registers,
    inGetIdentCompletionsFun: (unit -> Lines) -> GetCompletionsFun
) as this_
  =
    let myBuffers = new BuffersRegistry (
        myContextRef, myUserMessages, inGlobalSettings, inGlobalKeyMappings, inRegisters
    )

    let mutable myBuffer = myBuffers.CurrentBuffer

    let myMainContextRef2 = new WrappedRef2<MainContext> (
        myBuffers.CurrentMainContextRef
    )

    let publishMainContext () =
        myMainContextRef2.WrappedRef <- myBuffers.CurrentMainContextRef

    let applyBufferSwitch () =
        myBuffer <- myBuffers.CurrentBuffer
        myBuffers.UpdateCurrentMainContext ()
        publishMainContext ()

    let handleContextChanged () = myBuffers.UpdateCurrentMainContext ()
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    let myIdentCompletions = new CompletionItems (
        myContextRef, myUserMessages,
        inGetIdentCompletionsFun (fun () -> this_.Lines)
    )

    let myCompletions : ICompletionItems = myIdentCompletions

    member _.CurrentSettings        = myBuffers.CurrentSettings
    member _.CurrentKeyMappings     = myBuffers.CurrentKeyMappings
    member _.CurrentMainContextRef  = myMainContextRef2 :> IWrappedRef<MainContext>

    member _.FilePath               = myBuffer.FilePath
    member _.Lines                  = myBuffer.Lines
    member _.LinesForCompletion     = myBuffer.LinesForCompletion
    member _.IsReadOnly             = myBuffer.IsReadOnly
    member _.IsBufferChanged        = myBuffer.IsBufferChanged
    member _.HasUndoToRegister      = myBuffer.HasUndoToRegister
    member _.HasUndoLinesToRegister = myBuffer.HasUndoLinesToRegister

    member _.ApplySettings () =
        myBuffers.UpdateCurrentMainContext ()

    // Insert

    member _.PK_Insert key =
        let command =
            match key with

            | NoModif InputKey.LeftArrow  -> Some (CommonCommand       CursorLeft)
            | NoModif InputKey.RightArrow -> Some (CommonCommand       CursorRight)
            | NoModif InputKey.UpArrow    -> Some (WrapLinesDepCommand CursorHardUp)
            | NoModif InputKey.DownArrow  -> Some (WrapLinesDepCommand CursorHardDown)
            | Ctrl    InputKey.UpArrow    -> Some (WrapLinesDepCommand CursorSoftUp)
            | Ctrl    InputKey.DownArrow  -> Some (WrapLinesDepCommand CursorSoftDown)
            | NoModif InputKey.PageUp     -> Some (WrapLinesDepCommand ScrollPageUp)
            | NoModif InputKey.PageDown   -> Some (WrapLinesDepCommand ScrollPageDown)

            | Ctrl    InputKey.LeftArrow  -> Some (CommonCommand       CursorLeftAtWordStart)
            | Ctrl    InputKey.RightArrow -> Some (CommonCommand       CursorRightAtWordStart)

            | NoModif InputKey.Home       -> Some (CommonCommand       CursorHardLineStart)
            | NoModif InputKey.End        -> Some (CommonCommand       CursorHardLineEnd)
            | Ctrl    InputKey.Home       -> Some (WrapLinesDepCommand CursorSoftFileStart)
            | Ctrl    InputKey.End        -> Some (WrapLinesDepCommand CursorSoftFileEnd)

            | Ctrl    InputKey.Z          -> Some (CommonCommand       CursorToPairChar)

            | Ctrl    InputKey.Enter      -> Some (ModifyingCommand    InsertNewLine)
            | NoModif InputKey.Enter      -> Some (ModifyingCommand    InsertNewLineIndent)
            | NoModif InputKey.Delete     -> Some (ModifyingCommand    DeleteChar)
            | NoModif InputKey.Backspace  -> Some (ModifyingCommand    DeletePrevChar)

            | CanBeInserted c             -> Some (ModifyingCommand    (InsertChar c))

            | _                           -> None

        performSome command (
            fun command -> myBuffer.PerformCommand false false command 1
        )

    // InsertPaste

    member _.PK_InsertPaste isUpper c =
        let command = ModifyingCommand (PasteBefore (SelectedRegister (isUpper, c)))
        myBuffer.PerformCommand false false command 1

    // NormalMain

    member private _.PK_NormalMain_Repeatable key count =
        let F = false

        let isExtending, command =
            match key with

            | OptShift     InputKey.LeftArrow  s -> s , Some (CommonCommand       CursorLeft)
            | OptShift     InputKey.RightArrow s -> s , Some (CommonCommand       CursorRight)
            | OptShift     InputKey.UpArrow    s -> s , Some (WrapLinesDepCommand CursorHardUp)
            | OptShift     InputKey.DownArrow  s -> s , Some (WrapLinesDepCommand CursorHardDown)
            | OptShiftCtrl InputKey.UpArrow    s -> s , Some (WrapLinesDepCommand CursorSoftUp)
            | OptShiftCtrl InputKey.DownArrow  s -> s , Some (WrapLinesDepCommand CursorSoftDown)
            | OptShift     InputKey.PageUp     s -> s , Some (WrapLinesDepCommand ScrollPageUp)
            | OptShift     InputKey.PageDown   s -> s , Some (WrapLinesDepCommand ScrollPageDown)

            | OptShiftCtrl InputKey.LeftArrow  s -> s , Some (CommonCommand       CursorLeftAtWordStart)
            | OptShiftCtrl InputKey.RightArrow s -> s , Some (CommonCommand       CursorRightAtWordStart)

            | OptShift     InputKey.H          s -> s , Some (CommonCommand       CursorLeft)
            | OptShift     InputKey.L          s -> s , Some (CommonCommand       CursorRight)
            | OptShift     InputKey.K          s -> s , Some (WrapLinesDepCommand CursorHardUp)
            | OptShift     InputKey.J          s -> s , Some (WrapLinesDepCommand CursorHardDown)
            | OptShiftCtrl InputKey.K          s -> s , Some (WrapLinesDepCommand CursorSoftUp)
            | OptShiftCtrl InputKey.J          s -> s , Some (WrapLinesDepCommand CursorSoftDown)
            | OptShiftAlt  InputKey.K          s -> s , Some (WrapLinesDepCommand ScrollPageUp)
            | OptShiftAlt  InputKey.J          s -> s , Some (WrapLinesDepCommand ScrollPageDown)

            | OptShift     InputKey.W          s -> s , Some (CommonCommand       CursorRightBeforeWordStart)
            | OptShiftAlt  InputKey.W          s -> s , Some (CommonCommand       CursorLeftAfterWordEnd)
            | OptShift     InputKey.B          s -> s , Some (CommonCommand       CursorRightAtWordStart)
            | OptShiftAlt  InputKey.B          s -> s , Some (CommonCommand       CursorLeftAtWordStart)
            | OptShift     InputKey.E          s -> s , Some (CommonCommand       CursorRightAtWordEnd)
            | OptShiftAlt  InputKey.E          s -> s , Some (CommonCommand       CursorLeftAtWordEnd)

            | OptShift     InputKey.N          s -> s , Some (CommonCommand       (CursorToNextMatch false))
            | OptShiftAlt  InputKey.N          s -> s , Some (CommonCommand       (CursorToPrevMatch false))

            | Alt          InputKey.D            -> F , Some (ModifyingCommand    Delete)
            | NoModif      InputKey.D            -> F , Some (ModifyingCommand    (YankAndDelete DefaultRegister))
            | Shift        InputKey.P            -> F , Some (ModifyingCommand    (PasteBefore DefaultRegister))
            | NoModif      InputKey.P            -> F , Some (ModifyingCommand    (PasteAfter DefaultRegister))
            | NoModif      InputKey.GreaterThan  -> F , Some (TextRangesCommand   Indent)
            | NoModif      InputKey.LessThan     -> F , Some (TextRangesCommand   Unindent)

            | Shift        InputKey.C            -> F , Some (SelectionsCommand   CopyLastDown)
            | ShiftAlt     InputKey.C            -> F , Some (SelectionsCommand   CopyFirstUp)
            | Alt          InputKey.Comma        -> F , Some (SelectionsCommand   RemoveMain)
            | NoModif      InputKey.LeftRound    -> F , Some (SelectionsCommand   RotateUp)
            | NoModif      InputKey.RightRound   -> F , Some (SelectionsCommand   RotateDown)

            | NoModif      InputKey.U            -> F , Some (UndoRedoCommand     Undo)
            | Shift        InputKey.U            -> F , Some (UndoRedoCommand     UndoFast)
            | Alt          InputKey.U            -> F , Some (UndoRedoCommand     Redo)
            | ShiftAlt     InputKey.U            -> F , Some (UndoRedoCommand     RedoFast)

            | _                                  -> F , None

        performSome command (
            fun command -> myBuffer.PerformCommand true isExtending command count
        )

    member private _.PK_NormalMain_NonRepeatable key =
        let F = false

        let isExtending, command =
            match key with

            | OptShift     InputKey.Home      s -> s , Some (CommonCommand       CursorHardLineStart)
            | OptShift     InputKey.End       s -> s , Some (CommonCommand       CursorHardLineEnd)
            | OptShiftCtrl InputKey.Home      s -> s , Some (WrapLinesDepCommand CursorSoftFileStart)
            | OptShiftCtrl InputKey.End       s -> s , Some (WrapLinesDepCommand CursorSoftFileEnd)

            | OptShiftAlt  InputKey.H         s -> s , Some (CommonCommand       CursorHardLineStart)
            | OptShiftAlt  InputKey.L         s -> s , Some (CommonCommand       CursorHardLineEnd)
            | OptShiftCtrl InputKey.H         s -> s , Some (WrapLinesDepCommand CursorSoftLineStart)
            | OptShiftCtrl InputKey.L         s -> s , Some (WrapLinesDepCommand CursorSoftLineEnd)

            | OptShiftCtrl InputKey.Z         s -> s , Some (CommonCommand       CursorToPairChar)

            | Alt          InputKey.Exclamation -> F , Some (CommonCommand       AssertNonWhiteSpace)
            | Alt          InputKey.Dollar      -> F , Some (CommonCommand       ClearInfoMessage)

            | NoModif      InputKey.Y           -> F , Some (ModifyingCommand    (Yank DefaultRegister))
            | Shift        InputKey.R           -> F , Some (ModifyingCommand    (Replace DefaultRegister))
            | NoModif      InputKey.Caret       -> F , Some (ModifyingCommand    AlignSelections)

            | NoModif      InputKey.Tilde       -> F , Some (TextRangesCommand   ToUppercase)
            | Alt          InputKey.Tilde       -> F , Some (TextRangesCommand   InvertCase)
            | NoModif      InputKey.At          -> F , Some (TextRangesCommand   TabsToSpaces)
            | Alt          InputKey.At          -> F , Some (TextRangesCommand   SpacesToTabs)
            | Ctrl         InputKey.X           -> F , Some (TextRangesCommand   RegexEscape)

            | NoModif      InputKey.Percent     -> F , Some (SelectionsCommand   SelectWholeBuffer)
            | Alt          InputKey.Percent     -> F , Some (SelectionsCommand   InvertSelections)
            | NoModif      InputKey.Dollar      -> F , Some (SelectionsCommand   SplitOnLineStarts)
            | NoModif      InputKey.Underscore  -> F , Some (SelectionsCommand   MergeContiguous)
            | NoModif      InputKey.X           -> F , Some (SelectionsCommand   ExpandToFullLines)
            | Alt          InputKey.X           -> F , Some (SelectionsCommand   TrimToFullLines)
            | NoModif      InputKey.Semicolon   -> F , Some (SelectionsCommand   ReduceToCursor)
            | Alt          InputKey.Colon       -> F , Some (SelectionsCommand   ForwardDirection)
            | Alt          InputKey.Semicolon   -> F , Some (SelectionsCommand   FlipDirection)
            | NoModif      InputKey.Comma       -> F , Some (SelectionsCommand   KeepOnlyMain)
            | NoModif      InputKey.Ampersand   -> F , Some (SelectionsCommand   RemoveLessIndented)
            | Alt          InputKey.Ampersand   -> F , Some (SelectionsCommand   RemoveMoreIndented)
            | Alt          InputKey.M           -> F , Some (SelectionsCommand   (Store DefaultRegister))
            | NoModif      InputKey.M           -> F , Some (SelectionsCommand   (Load DefaultRegister))
            | Ctrl         InputKey.M           -> F , Some (SelectionsCommand   (RemoveStored DefaultRegister))

            | NoModif      InputKey.Plus        -> let count = inRegisters.GetSlotsCount DefaultRegister
                                                   let count = count |> Option.defaultValue 1
                                                   F , Some (SelectionsCommand (Multiply count))

            | _                                 -> F , None

        performSome command (
            fun command -> myBuffer.PerformCommand true isExtending command 1
        )

    member this.PK_NormalMain key count =
        if count = 1 then
            (this.PK_NormalMain_Repeatable key count) ||
            (this.PK_NormalMain_NonRepeatable key)
        else
            (this.PK_NormalMain_Repeatable key count)

    member _.PK_NormalMain_BeforeInsert key =
        let command =
            match key with

            | NoModif InputKey.I -> Some (ModifyingCommand EnterInserting)
            | NoModif InputKey.A -> Some (ModifyingCommand EnterAppending)
            | Shift   InputKey.I -> Some (ModifyingCommand EnterInsertingAtSol)
            | Shift   InputKey.A -> Some (ModifyingCommand EnterAppendingAtEol)

            | Alt     InputKey.C -> Some (ModifyingCommand Delete)
            | NoModif InputKey.C -> Some (ModifyingCommand (YankAndDelete DefaultRegister))

            | _                  -> None

        let result = performSome command (
            fun command -> myBuffer.PerformCommand false false command 1
        )

        if result && myBuffer.IsReadOnly then
            myUserMessages.RegisterMessage ERROR_FILE_OPENED_AS_READ_ONLY

        result

    // NormalRegister

    member private _.PK_NormalRegister_Repeatable key count register =
        let command =
            match key with
            | NoModif InputKey.Y when Registers.isUpperRegister register
                                 -> Some (ModifyingCommand (Yank register))

            | NoModif InputKey.D -> Some (ModifyingCommand (YankAndDelete register))
            | Shift   InputKey.P -> Some (ModifyingCommand (PasteBefore register))
            | NoModif InputKey.P -> Some (ModifyingCommand (PasteAfter register))

            | _                  -> None

        performSome command (
            fun command -> myBuffer.PerformCommand true false command count
        )

    member private _.PK_NormalRegister_NonRepeatable key register =
        let command =
            match key with
            | NoModif InputKey.Y    when not (Registers.isUpperRegister register)
                                    -> Some (ModifyingCommand  (Yank register))

            | Shift   InputKey.R    -> Some (ModifyingCommand  (Replace register))

            | Ctrl    InputKey.U    when not (Registers.isUpperRegister register)
                                    -> Some (UndoRedoCommand   (AddName register))

            | Alt     InputKey.M    when not (Registers.isUpperRegister register)
                                    -> Some (SelectionsCommand (Store register))

            | NoModif InputKey.M    when not (Registers.isUpperRegister register)
                                    -> Some (SelectionsCommand (Load register))

            | Ctrl    InputKey.M    when not (Registers.isUpperRegister register)
                                    -> Some (SelectionsCommand (RemoveStored register))

            | NoModif InputKey.Plus -> let count = inRegisters.GetSlotsCount register
                                       let count = count |> Option.defaultValue 1
                                       Some (SelectionsCommand (Multiply count))

            | NoModif InputKey.U    when not (Registers.isUpperRegister register)
                                    -> Some (UndoRedoCommand   (UndoNamed register))

            | Alt     InputKey.U    when not (Registers.isUpperRegister register)
                                    -> Some (UndoRedoCommand   (RedoNamed register))

            | _                     -> None

        performSome command (
            fun command -> myBuffer.PerformCommand true false command 1
        )

    member this.PK_NormalRegister key count register =
        if count = 1 then
            (this.PK_NormalRegister_Repeatable key count register) ||
            (this.PK_NormalRegister_NonRepeatable key register)
        else
            (this.PK_NormalRegister_Repeatable key count register)

    member _.PK_NormalRegister_BeforeInsert key register =
        let command =
            match key with
            | NoModif InputKey.C -> Some (ModifyingCommand (YankAndDelete register))
            | _                  -> None

        let result = performSome command (
            fun command -> myBuffer.PerformCommand false false command 1
        )

        if result && myBuffer.IsReadOnly then
            myUserMessages.RegisterMessage ERROR_FILE_OPENED_AS_READ_ONLY

        result

    // NormalCount, NormalFindChar, NormalFillWithChar, NormalGoTo

    member _.PK_NormalCount key count =
        let F = false

        let isExtending, command =
            match key with
            | OptShift InputKey.G s  -> let line = userPosToPos count
                                        s , Some (WrapLinesDepCommand (CursorHardToLine line))

            | NoModif  InputKey.Plus -> F , Some (SelectionsCommand (Multiply count))

            | _                      -> F , None

        performSome command (
            fun command -> myBuffer.PerformCommand true isExtending command 1
        )

    member _.PK_NormalFindChar c count isExtending state =
        let command =
            match state with
            | LeftToCharState     -> CommonCommand (CursorLeftToChar     c)
            | LeftUntilCharState  -> CommonCommand (CursorLeftUntilChar  c)
            | RightToCharState    -> CommonCommand (CursorRightToChar    c)
            | RightUntilCharState -> CommonCommand (CursorRightUntilChar c)

        myBuffer.PerformCommand true isExtending command count

    member _.PK_NormalFillWithChar c =
        let command = TextRangesCommand (FillWithChar c)
        myBuffer.PerformCommand true false command 1

    member _.PK_NormalGoTo key isExtending =
        let F = false
        let e = isExtending

        let isExtending, command =
            match key with
            | OptShift    InputKey.G s -> e || s , Some (WrapLinesDepCommand CursorSoftFileStart)
            | OptShift    InputKey.E s -> e || s , Some (WrapLinesDepCommand CursorSoftFileEnd)
            | OptShift    InputKey.L s -> e || s , Some (CommonCommand       CursorBeforeEol)
            | OptShiftAlt InputKey.L s -> e || s , Some (CommonCommand       CursorAtEol)
            | _                        -> F      , None

        performSome command (
            fun command -> myBuffer.PerformCommand true isExtending command 1
        )

    member _.PK_NormalView key =
        let command =
            match key with
            | NoModif InputKey.V -> Some (WrapLinesDepCommand CenterVertically)
            | NoModif InputKey.H -> Some (WrapLinesDepCommand CenterHorizontally)
            | _                  -> None

        performSome command (
            fun command -> myBuffer.PerformCommand true false command 1
        )

    // Completion

    member this.PK_Completion key =
        match key with
        | Ctrl InputKey.N ->
            this.GoToNextInCompletion ()
            true
        | Ctrl InputKey.P ->
            this.GoToPreviousInCompletion ()
            true

        | _ -> false

    // rendering

    member _.AdaptDisplayPosition () =
        let command = WrapLinesDepCommand AdaptDisplayPos
        myBuffer.PerformCommand true false command 1

    member _.GetDisplayRows () =
        myBuffer.GetDisplayRows ()

    member _.GetCursorPosForStatusArea () =
        myBuffer.GetCursorPosForStatusArea ()

    // Undo/Redo

    member _.RegisterUndo isStateVolatile =
        myBuffer.RegisterUndo isStateVolatile

    member _.ClearIsLastUndoVolatile () =
        myBuffer.ClearIsLastUndoVolatile ()

    member this.UndoCorruptedState () =
        myBuffer.UndoCorruptedState ()
        this.ClearCompletions ()

    // regular expression matching

    member _.SearchMatching regex isForward isExtending =
        myBuffer.SearchMatching regex isForward isExtending

    member _.ReSearchMatching () =
        myBuffer.ReSearchMatching ()

    member _.ClearSearchMatching () =
        myBuffer.ClearSearchMatching ()

    member _.ExtractMatching regex =
        match myBuffer with
        :? TextAreaBufferExtract as buffer ->
            buffer.ExtractMatching regex
        | _ ->
            myUserMessages.RegisterMessage ERROR_OP_INVALID_ON_NON_EXTRACT_BUFFER

    member _.ReExtractMatching () =
        match myBuffer with
        :? TextAreaBufferExtract as buffer ->
            buffer.ReExtractMatching ()
        | _ ->
            myUserMessages.RegisterMessage ERROR_OP_INVALID_ON_NON_EXTRACT_BUFFER

    member _.ClearExtractMatching () =
        match myBuffer with
        :? TextAreaBufferExtract as buffer ->
            buffer.ClearExtractMatching ()
        | _ ->
            myUserMessages.RegisterMessage ERROR_OP_INVALID_ON_NON_EXTRACT_BUFFER

    member _.SelectMatching regex =
        let command = SelectionsCommand (SelectMatching regex)
        myBuffer.PerformCommand true false command 1

    member _.KeepMatching regex =
        let command = SelectionsCommand (KeepMatching regex)
        myBuffer.PerformCommand true false command 1

    member _.DiscardMatching regex =
        let command = SelectionsCommand (DiscardMatching regex)
        myBuffer.PerformCommand true false command 1

    // buffer operations

    member this.EditFile filePath encoding strictEncoding quite =
        myBuffers.AddTextAreaBuffer filePath

        applyBufferSwitch ()

        let result = this.SetBufferSettingsAux encoding strictEncoding (Some "false")

        match result with
        | Error e -> myUserMessages.RegisterMessage (makeErrorMessage e)
        | Ok ()   -> this.LoadFileAux quite

    member this.ViewFile filePath encoding strictEncoding =
        myBuffers.AddTextAreaBuffer filePath

        applyBufferSwitch ()

        let result = this.SetBufferSettingsAux encoding strictEncoding (Some "true")

        match result with
        | Error e -> myUserMessages.RegisterMessage (makeErrorMessage e)
        | Ok ()   -> this.LoadFileAux false

    member this.EditOrViewFile filePath encoding strictEncoding isReadOnly =
        myBuffer.FilePath <- filePath

        let result = this.SetBufferSettingsAux encoding strictEncoding isReadOnly

        match result with
        | Error e -> myUserMessages.RegisterMessage (makeErrorMessage e)
        | Ok ()   -> this.LoadFileAux false

    member this.ExtractFile fileName =
        match myBuffer with
        :? TextAreaBuffer as buffer ->
            myBuffers.AddTextAreaBufferExtract buffer this.CurrentSettings fileName

            applyBufferSwitch ()

            this.SetBufferAsFixedReadOnly ()
        | _ ->
            myUserMessages.RegisterMessage ERROR_OP_INVALID_ON_EXTRACT_BUFFER

    member private this.LoadFileAux quite =
        let encoding       = getValueString this.CurrentSettings Name.encoding
        let strictEncoding = getValueBool   this.CurrentSettings Name.strictEncoding

        // It's definitely an instance of TextAreaBuffer.
        let buffer = myBuffer :?> TextAreaBuffer

        let fileFormat, endsWithNewLine = buffer.LoadFile encoding strictEncoding quite
        let newLineAtEof = if isSingleEmptyLine this.Lines then true else endsWithNewLine

        let fileFormat   = valueToString (FileFormat fileFormat)
        let newLineAtEof = valueToString (Bool newLineAtEof)

        setValue this.CurrentSettings Scope.buffer Name.fileFormat fileFormat
            |> ignore
        setValue this.CurrentSettings Scope.buffer Name.newLineAtEof newLineAtEof
            |> ignore

    member this.ReloadFile () =
        let encoding       = getValueString this.CurrentSettings Name.encoding
        let strictEncoding = getValueBool   this.CurrentSettings Name.strictEncoding

        let fileFormat, endsWithNewLine = myBuffer.ReloadFile encoding strictEncoding
        let newLineAtEof = if isSingleEmptyLine this.Lines then true else endsWithNewLine

        let fileFormat   = valueToString (FileFormat fileFormat)
        let newLineAtEof = valueToString (Bool newLineAtEof)

        setValue this.CurrentSettings Scope.buffer Name.fileFormat fileFormat
            |> ignore
        setValue this.CurrentSettings Scope.buffer Name.newLineAtEof newLineAtEof
            |> ignore

    member this.WriteFile () =
        let readOnly     = getValueBool       this.CurrentSettings Name.readOnly
        let encoding     = getValueString     this.CurrentSettings Name.encoding
        let fileFormat   = getValueFileFormat this.CurrentSettings Name.fileFormat
        let newLineAtEof = getValueBool       this.CurrentSettings Name.newLineAtEof

        let endWithNewLine =
            if isSingleEmptyLine this.Lines then
                false
            else
                newLineAtEof

        if readOnly then
            myUserMessages.RegisterMessage ERROR_FILE_OPENED_AS_READ_ONLY
        else
            myBuffer.WriteFile encoding fileFormat endWithNewLine

    member this.WriteFileAs filePath =
        let encoding     = getValueString     this.CurrentSettings Name.encoding
        let fileFormat   = getValueFileFormat this.CurrentSettings Name.fileFormat
        let newLineAtEof = getValueBool       this.CurrentSettings Name.newLineAtEof

        let endWithNewLine =
            if isSingleEmptyLine this.Lines then
                false
            else
                newLineAtEof

        myBuffer.FilePath <- filePath

        myBuffer.WriteFile encoding fileFormat endWithNewLine

    member this.SetBufferSettings encoding strictEncoding isReadOnly =
        let result = this.SetBufferSettingsAux encoding strictEncoding isReadOnly

        match result with
        | Error e -> myUserMessages.RegisterMessage (makeErrorMessage e)
        | Ok ()   -> ()

    member private this.SetBufferSettingsAux encoding strictEncoding isReadOnly =
        let results = seq {
            strictEncoding |> Option.map (
                setValue this.CurrentSettings Scope.buffer Name.strictEncoding
            )
        ;
            encoding       |> Option.map (
                setValue this.CurrentSettings Scope.buffer Name.encoding
            )
        ;
            isReadOnly     |> Option.map (
                setValue this.CurrentSettings Scope.buffer Name.readOnly
            )
        }

        let firstError = results |> Seq.tryPick (
            function
                | Some (Error e) -> Some e
                | _              -> None
        )

        match firstError with
        | Some e -> Error e
        | None   -> this.ApplySettings ()
                    Ok ()

    member private this.SetBufferAsFixedReadOnly () =
        let result =
            setValueAsFixed this.CurrentSettings Scope.buffer Name.readOnly "true"

        match result with
        | Ok ()   -> this.ApplySettings ()
        | Error _ -> ()

    member __.DeleteBuffer () =
        myBuffers.DeleteBuffer ()
        applyBufferSwitch ()

    member _.ToNextBuffer () =
        myBuffers.ToNextBuffer ()
        applyBufferSwitch ()

    member _.ToPrevBuffer () =
        myBuffers.ToPrevBuffer ()
        applyBufferSwitch ()

    member _.IsAnyBufferChanged () =
        myBuffers.IsAnyBufferChanged ()

    member _.ToFirstChangedBuffer () =
        myBuffers.ToFirstChanged ()
        applyBufferSwitch ()

    member _.HasBufferWithFilePath filePath =
        myBuffers.HasBufferWithFilePath filePath

    member _.ToBufferWithFilePath filePath =
        myBuffers.ToBufferWithFilePath filePath
        applyBufferSwitch ()

    // completions

    member _.IsInCompletion () =
        myCompletions.IsInCompletion ()

    member private this.GoToNextInCompletion () =
        if not (this.IsInCompletion ()) then
            myCompletions.TrySet (
                seq {
                    for selection in myBuffer.Selections.Items do
                        let cursor = selection.Cursor
                        myBuffer.Lines[cursor.line], cursor
                }
            )

        this.GoToInCompletion (
            fun () -> myCompletions.GetNext ()
        )

    member private this.GoToPreviousInCompletion () =
        this.GoToInCompletion (
            fun () -> myCompletions.GetPrevious ()
        )

    member private this.GoToInCompletion getFun =
        let ret =
            match getFun () with
            | Some completionAction ->
                this.LoadCharsFromCompletion completionAction
            | None ->
                ()

        if not (myCompletions.IsInCompletion ()) then
            myCompletions.Clear ()

        ret

    member private _.LoadCharsFromCompletion (ca: CompletionAction) =
        let command = ModifyingCommand (DeletePrevChars ca.toDelete)
        myBuffer.PerformCommand false false command 1

        let command = ModifyingCommand (InsertChars ca.toInsert)
        myBuffer.PerformCommand false false command 1

    member _.GetCompletionsRow () =
        myCompletions.GetCompletionsRow ()

    // Can be called also outside of prompt.
    member _.ClearCompletions () =
        if myCompletions.IsInCompletion () then
            myCompletions.Clear ()

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable.Dispose ()
            (myMainContextRef2 :> IDisposable).Dispose ()
            (myBuffers :> IDisposable).Dispose ()
