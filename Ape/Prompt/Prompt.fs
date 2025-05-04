module Prompt

open System

open Commands.InCommands
open Common
open CompletionItems
open CompletionUtils
open ConsoleKeys
open Context
open DataTypes
open PromptBuffer
open PromptHistory
open PromptStatus
open UserMessages
open WrappedRef

let (|CanBeInserted|_|) key =
    match keyToChar key with
    | Some c when Utils.canBeDisplayed c ->
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

/// Represents a single prompt area, which holds a single prompt buffer. This
/// class transforms input keys to the corresponding commands and delegates the
/// commands to the buffer. It also provides several methods related to rendering,
/// Undo/Redo, and entering/leaving.

[<Sealed>]
type Prompt (
    inConsoleContextRef: IWrappedRef<ConsoleContext>,
    inMainContextRef:    IWrappedRef<MainContext>,
    inUserMessages:      UserMessages,
    inRegisters:         Registers.Registers,
    inGetCommandCompletionsFun: GetCompletionsFun,
    inGetIdentCompletionsFun:   GetCompletionsFun
) =
    let myExtraContextRef = WrappedRef (
        makeExtraContext (getPromptModeLength CommandPrompt)
    )

    let myBuffer = new PromptBuffer (
        inMainContextRef, myExtraContextRef, inUserMessages, inRegisters
    )

    let myCommandHistory = PromptHistory ()
    let myRegexHistory   = PromptHistory ()

    let mutable myHistory = myCommandHistory

    let myCommandCompletions = new CompletionItems (
        inConsoleContextRef, inUserMessages, inGetCommandCompletionsFun
    )

    let myIdentCompletions = new CompletionItems (
        inConsoleContextRef, inUserMessages, inGetIdentCompletionsFun
    )

    let mutable myCompletions: ICompletionItems option = None

    member _.Line                   = myBuffer.Line
    member _.HasUndoToRegister      = myBuffer.HasUndoToRegister
    member _.HasUndoLinesToRegister = myBuffer.HasUndoLinesToRegister

    member _.IsCurrentFromHistory   = myHistory.IsCurrentFromHistory

    // Insert

    member _.PK_Insert key =
        let command =
            match key with

            | NoModif InputKey.LeftArrow  -> Some (CommonCommand       CursorLeft)
            | NoModif InputKey.RightArrow -> Some (CommonCommand       CursorRight)

            | Ctrl    InputKey.LeftArrow  -> Some (CommonCommand       CursorLeftAtWordStart)
            | Ctrl    InputKey.RightArrow -> Some (CommonCommand       CursorRightAtWordStart)

            | NoModif InputKey.Home       -> Some (CommonCommand       CursorHardLineStart)
            | NoModif InputKey.End        -> Some (CommonCommand       CursorHardLineEnd)
            | Ctrl    InputKey.Home       -> Some (WrapLinesDepCommand CursorSoftFileStart)
            | Ctrl    InputKey.End        -> Some (WrapLinesDepCommand CursorSoftFileEnd)

            | Ctrl    InputKey.Z          -> Some (CommonCommand       CursorToPairChar)

            | NoModif InputKey.Delete     -> Some (ModifyingCommand    DeleteChar)
            | NoModif InputKey.Backspace  -> Some (ModifyingCommand    DeletePrevChar)

            | CanBeInserted c             -> Some (ModifyingCommand    (InsertChar c))

            | _                           -> None

        performSome command (
            fun command -> myBuffer.PerformCommand false false command
        )

    // InsertPaste

    member _.PK_InsertPaste isUpper c =
        let command = ModifyingCommand (PasteBefore (SelectedRegister (isUpper, c)))
        myBuffer.PerformCommand false false command

    // NormalMain

    member private _.PK_NormalMain_Repeatable key =
        let F = false

        let isExtending, command =
            match key with

            | OptShift     InputKey.LeftArrow  s -> s , Some (CommonCommand     CursorLeft)
            | OptShift     InputKey.RightArrow s -> s , Some (CommonCommand     CursorRight)

            | OptShiftCtrl InputKey.LeftArrow  s -> s , Some (CommonCommand     CursorLeftAtWordStart)
            | OptShiftCtrl InputKey.RightArrow s -> s , Some (CommonCommand     CursorRightAtWordStart)

            | OptShift     InputKey.H          s -> s , Some (CommonCommand     CursorLeft)
            | OptShift     InputKey.L          s -> s , Some (CommonCommand     CursorRight)

            | OptShift     InputKey.W          s -> s , Some (CommonCommand     CursorRightBeforeWordStart)
            | OptShiftAlt  InputKey.W          s -> s , Some (CommonCommand     CursorLeftAfterWordEnd)
            | OptShift     InputKey.B          s -> s , Some (CommonCommand     CursorRightAtWordStart)
            | OptShiftAlt  InputKey.B          s -> s , Some (CommonCommand     CursorLeftAtWordStart)
            | OptShift     InputKey.E          s -> s , Some (CommonCommand     CursorRightAtWordEnd)
            | OptShiftAlt  InputKey.E          s -> s , Some (CommonCommand     CursorLeftAtWordEnd)

            | Alt          InputKey.D            -> F , Some (ModifyingCommand  Delete)
            | NoModif      InputKey.D            -> F , Some (ModifyingCommand  (YankAndDelete DefaultRegister))
            | Shift        InputKey.P            -> F , Some (ModifyingCommand  (PasteBefore DefaultRegister))
            | NoModif      InputKey.P            -> F , Some (ModifyingCommand  (PasteAfter DefaultRegister))
            | NoModif      InputKey.GreaterThan  -> F , Some (TextRangesCommand Indent)
            | NoModif      InputKey.LessThan     -> F , Some (TextRangesCommand Unindent)

            | NoModif      InputKey.U            -> F , Some (UndoRedoCommand   Undo)
            | Shift        InputKey.U            -> F , Some (UndoRedoCommand   UndoFast)
            | Alt          InputKey.U            -> F , Some (UndoRedoCommand   Redo)
            | ShiftAlt     InputKey.U            -> F , Some (UndoRedoCommand   RedoFast)

            | _                                  -> F , None

        performSome command (
            fun command -> myBuffer.PerformCommand true isExtending command
        )

    member private _.PK_NormalMain_NonRepeatable key =
        let F = false

        let isExtending, command =
            match key with

            | OptShift     InputKey.Home      s -> s , Some (CommonCommand       CursorHardLineStart)
            | OptShift     InputKey.End       s -> s , Some (CommonCommand       CursorHardLineEnd)

            | OptShiftAlt  InputKey.H         s -> s , Some (CommonCommand       CursorHardLineStart)
            | OptShiftAlt  InputKey.L         s -> s , Some (CommonCommand       CursorHardLineEnd)
            | OptShiftCtrl InputKey.H         s -> s , Some (WrapLinesDepCommand CursorSoftLineStart)
            | OptShiftCtrl InputKey.L         s -> s , Some (WrapLinesDepCommand CursorSoftLineEnd)

            | OptShiftCtrl InputKey.Z         s -> s , Some (CommonCommand       CursorToPairChar)

            | Alt          InputKey.Exclamation -> F , Some (CommonCommand       AssertNonWhiteSpace)
            | Alt          InputKey.Dollar      -> F , Some (CommonCommand       ClearInfoMessage)

            | NoModif      InputKey.Y           -> F , Some (ModifyingCommand    (Yank DefaultRegister))
            | Shift        InputKey.R           -> F , Some (ModifyingCommand    (Replace DefaultRegister))

            | NoModif      InputKey.Tilde       -> F , Some (TextRangesCommand   ToUppercase)
            | Alt          InputKey.Tilde       -> F , Some (TextRangesCommand   InvertCase)
            | Ctrl         InputKey.X           -> F , Some (TextRangesCommand   RegexEscape)

            | NoModif      InputKey.Percent     -> F , Some (SelectionsCommand   SelectWholeBuffer)
            | NoModif      InputKey.X           -> F , Some (SelectionsCommand   ExpandToFullLines)
            | NoModif      InputKey.Semicolon   -> F , Some (SelectionsCommand   ReduceToCursor)
            | Alt          InputKey.Colon       -> F , Some (SelectionsCommand   ForwardDirection)
            | Alt          InputKey.Semicolon   -> F , Some (SelectionsCommand   FlipDirection)

            | _                                 -> F , None

        performSome command (
            fun command -> myBuffer.PerformCommand true isExtending command
        )

    member this.PK_NormalMain key =
        (this.PK_NormalMain_Repeatable key) ||
        (this.PK_NormalMain_NonRepeatable key)

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

        performSome command (
            fun command -> myBuffer.PerformCommand false false command
        )

    // NormalRegister

    member private _.PK_NormalRegister_Repeatable key register =
        let command =
            match key with
            | NoModif InputKey.Y when Registers.isUpperRegister register
                                 -> Some (ModifyingCommand (Yank register))

            | NoModif InputKey.D -> Some (ModifyingCommand (YankAndDelete register))
            | Shift   InputKey.P -> Some (ModifyingCommand (PasteBefore register))
            | NoModif InputKey.P -> Some (ModifyingCommand (PasteAfter register))

            | _                  -> None

        performSome command (
            fun command -> myBuffer.PerformCommand true false command
        )

    member private _.PK_NormalRegister_NonRepeatable key register =
        let command =
            match key with
            | NoModif InputKey.Y when not (Registers.isUpperRegister register)
                                 -> Some (ModifyingCommand (Yank register))

            | Shift   InputKey.R -> Some (ModifyingCommand (Replace register))

            | _                  -> None

        performSome command (
            fun command -> myBuffer.PerformCommand true false command
        )

    member this.PK_NormalRegister key register =
        (this.PK_NormalRegister_Repeatable key register) ||
        (this.PK_NormalRegister_NonRepeatable key register)

    member _.PK_NormalRegister_BeforeInsert key register =
        let command =
            match key with
            | NoModif InputKey.C -> Some (ModifyingCommand (YankAndDelete register))
            | _                  -> None

        performSome command (
            fun command -> myBuffer.PerformCommand false false command
        )

    // NormalGoTo

    member _.PK_NormalGoTo key isExtending =
        let F = false
        let e = isExtending

        let isExtending, command =
            match key with
            | OptShift    InputKey.L s -> e || s , Some (CommonCommand CursorBeforeEol)
            | OptShiftAlt InputKey.L s -> e || s , Some (CommonCommand CursorAtEol)
            | _                        -> F      , None

        performSome command (
            fun command -> myBuffer.PerformCommand true isExtending command
        )

    // History, Completion

    member this.PK_History key =
        match key with
        | NoModif InputKey.UpArrow   -> this.GoToPreviousInHistory ()
                                        true
        | NoModif InputKey.DownArrow -> this.GoToNextInHistory ()
                                        true
        | _ -> false

    member this.PK_Completion key =
        match key with
        | Ctrl    InputKey.N
        | NoModif InputKey.Tab ->
            this.GoToNextInCompletion ()
            true
        | Ctrl    InputKey.P
        | Shift   InputKey.Tab ->
            this.GoToPreviousInCompletion ()
            true

        | _ -> false

    // rendering

    member _.AdaptDisplayPosition () =
        let command = WrapLinesDepCommand AdaptDisplayPos
        myBuffer.PerformCommand true false command

    member _.GetDisplayRows () =
        myBuffer.GetDisplayRows ()

    // Undo/Redo

    member _.RegisterUndo isStateVolatile =
        myBuffer.RegisterUndo isStateVolatile

    member _.ClearIsLastUndoVolatile () =
        myBuffer.ClearIsLastUndoVolatile ()

    member this.UndoCorruptedState () =
        myBuffer.UndoCorruptedState ()
        this.ClearCompletions ()

    // entering/leaving

    member _.WhenEntering promptType =
        myExtraContextRef.Value <-
            makeExtraContext (getPromptModeLength promptType)

        myBuffer.LoadString ""

        myHistory <-
            match promptType with
            | CommandPrompt -> myCommandHistory
            | _             -> myRegexHistory

        myCompletions <-
            match promptType with
            | CommandPrompt -> Some myCommandCompletions
            | SearchPrompt _
            | ExtractPrompt
            | SelectPrompt
            | KeepPrompt
            | DiscardPrompt -> Some myIdentCompletions

    member _.WhenLeaving toOverwrite =
        myHistory.WhenLeaving myBuffer.Line toOverwrite

    // history

    member private this.GoToPreviousInHistory () =
        this.GoToInHistory myHistory.GetPrevious

    member private this.GoToNextInHistory () =
        this.GoToInHistory myHistory.GetNext

    member private _.GoToInHistory getFun =
        match getFun () with
        | Some line -> myBuffer.LoadCharsFromHistory line
        | None      -> ()

    // completions

    member _.IsInCompletion () =
        match myCompletions with
        | Some completions ->
            completions.IsInCompletion ()
        | None ->
            invalidOp ""

    member private this.GoToNextInCompletion () =
        match myCompletions with
        | Some completions ->
            if not (completions.IsInCompletion ()) then
                completions.TrySet (
                    seq {
                        myBuffer.Line, myBuffer.Main.Cursor
                    }
                )
            this.GoToInCompletion completions (
                fun () -> completions.GetNext ()
            )
        | None ->
            invalidOp ""

    member private this.GoToPreviousInCompletion () =
        match myCompletions with
        | Some completions ->
            this.GoToInCompletion completions (
                fun () -> completions.GetPrevious ()
            )
        | None ->
            invalidOp ""

    member private this.GoToInCompletion completions getFun =
        let ret =
            match getFun () with
            | Some completionAction ->
                this.LoadCharsFromCompletion completionAction
            | None ->
                ()

        if not (completions.IsInCompletion ()) then
            completions.Clear ()

        ret

    member private _.LoadCharsFromCompletion (ca: CompletionAction) =
        let command = ModifyingCommand (DeletePrevChars ca.toDelete)
        myBuffer.PerformCommand false false command

        let command = ModifyingCommand (InsertChars ca.toInsert)
        myBuffer.PerformCommand false false command

    member _.GetCompletionsRow () =
        match myCompletions with
        | Some completions ->
            completions.GetCompletionsRow ()
        | None ->
            invalidOp ""

    // Can be called also outside of prompt.
    member _.ClearCompletions () =
        match myCompletions with
        | Some completions ->
            if completions.IsInCompletion () then
                completions.Clear ()
        | None ->
            ()

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            (myBuffer :> IDisposable).Dispose ()

            match myCompletions with
            | Some completions ->
                (completions :> IDisposable).Dispose ()
            | None            ->
                ()
