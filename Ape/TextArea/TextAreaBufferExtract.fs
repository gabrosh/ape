module TextAreaBufferExtract

open System
open System.Collections.Immutable

open Commands.InCommands
open Common
open Context
open DataTypes
open ITextAreaBuffer
open Position
open Selection
open Selections
open SelectionsRegisters
open TextAreaBuffer
open TextAreaDelegator
open TextRangesModifier
open UndoProvider
open UserMessages
open WrappedRef

let private getBufferState
    (lines:         Lines option)
    (selections:    Selections)
    (selsRegisters: SelectionsRegisters)
    (wantedColumns: Helpers.WantedColumns)
    displayPos
  =
    let lines = lines |> Option.map ImmutableArray.CreateRange

    {
        names            = []
        wasSaved         = false
        lines            = lines
        selections       = selections.GetImmutable ()
        mainIndex        = selections.MainIndex
        selsRegisters    = selsRegisters.GetAllImmutable ()
        displayPos       = displayPos
        pendingWCActions = wantedColumns.GetPendingWCActions ()
    }

[<Sealed>]
type TextAreaBufferExtract (
    myParent:          TextAreaBuffer,
    myContextRef:      IWrappedRef<MainContext>,
    myUserMessages:    UserMessages,
    myRegisters:       Registers.Registers,
    inFilePath:        string,
    inExtractOnConstr: bool
) =
    // init mechanism
    let myParentMainForInit = myParent.Main

    let mutable myContext = myContextRef.Value
    let handleContextChanged () = myContext <- myContextRef.Value
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    let myBasicState = {
        displayPos = DisplayPos_Zero; prevCommand = None
    }

    // fields affected by Undo/Redo
    let myLines           = Lines [Chars.Empty]
    let mySelectionsArray = ResizeArray<Selection> [Selection_Zero]
    let mySelections      = Selections mySelectionsArray
    let mySelsModifier    = TextRangesModifier mySelectionsArray
    let mySelsRegisters   = SelectionsRegisters ()
    let myWantedColumns   = Helpers.WantedColumns mySelections

    // reporting a need for Undo registration
    let mutable myHasUndoToRegister = false

    let myMatchRanges = myParent.CreateMatchRangesExtract myLines inExtractOnConstr

    let myUndoProvider = UndoProvider (
        // the same code as in GetInitialUndoState, which can't be called here
        getBufferState (Some myLines) mySelections mySelsRegisters
            myWantedColumns myBasicState.displayPos
    )
    let myDispatcher = new TextAreaDispatcher.TextAreaDispatcher (
        myContextRef,
        myUserMessages, myLines, myRegisters, mySelections, mySelsRegisters,
        myWantedColumns, myMatchRanges, myUndoProvider
    )
    let myRenderer = TextAreaRenderer.TextAreaRenderer (
        myLines, mySelections, myMatchRanges, myDispatcher
    )

    // delegators

    let myBasicDelegator = TextAreaDelegator.TextAreaBasic (
        myBasicState,
        mySelections,
        myWantedColumns, myMatchRanges, myDispatcher
    )
    let myModifyingDelegator = TextAreaDelegator.TextAreaModifying (
        myBasicState,
        myLines, myRegisters,
        mySelections, mySelsModifier, mySelsRegisters,
        myWantedColumns, myDispatcher
    )
    let myTextRangesDelegator = TextAreaDelegator.TextAreaTextRanges (
        myBasicState,
        mySelections, mySelsModifier, mySelsRegisters,
        myWantedColumns, myDispatcher
    )
    let mySelectionsDelegator = TextAreaDelegator.TextAreaSelections (
        myBasicState,
        myWantedColumns, myDispatcher
    )
    let myUndoRedoDelegator = TextAreaDelegator.TextAreaUndoRedo (
        myBasicState,
        myDispatcher
    )

    // public properties

    member val FilePath = inFilePath
        with get, set

    member _.Parent = myParent

    member _.Lines                  = myLines
    member _.Selections             = mySelections
    member _.IsReadOnly             = myContext.readOnly
    member _.IsBufferChanged        = false
    member _.HasUndoToRegister      = myHasUndoToRegister
    member _.HasUndoLinesToRegister = false

    // only for testing purposes
    member _.Main = mySelections.Main

    // private properties

    member private _.DisplayPos
        with get ()    = myBasicState.displayPos
        and  set value = myBasicState.displayPos  <- value

    member private _.PrevCommand
        with get ()    = myBasicState.prevCommand
        and  set value = myBasicState.prevCommand <- value

    // init mechanism

    /// Initializes the instance after its construction.
    member this.Init () =
        mySelections.Main <- myParentMainForInit

        this.WrapExtractAction (
            fun () -> myMatchRanges.Init ()
        )

    // commands

    member this.PerformCommand isNormalMode isExtending command count =
        if this.IsReadOnly && isWriteCommand command then
            myUserMessages.RegisterMessage ERROR_BUFFER_OPENED_AS_READ_ONLY
        else
            this.PerformCommandAux isNormalMode isExtending command count

    member private this.PerformCommandAux isNormalMode isExtending command count =

        match command with
        | CommonCommand (CursorToNextMatch isInitial) ->
            let isInitial' = this.ReSearchIfNeeded isInitial
            let command'   = CommonCommand (CursorToNextMatch isInitial')

            myBasicDelegator.PerformMatchCommand isExtending command' count true

        | CommonCommand (CursorToPrevMatch isInitial) ->
            let isInitial' = this.ReSearchIfNeeded isInitial
            let command'   = CommonCommand (CursorToPrevMatch isInitial')

            myBasicDelegator.PerformMatchCommand isExtending command' count false

        | CommonCommand _ ->
            myBasicDelegator.PerformOnAllSelections isExtending command count

        | WrapLinesDepCommand ScrollPageUp
        | WrapLinesDepCommand ScrollPageDown     ->
            myBasicDelegator.PerformOnMainSelection isExtending command count
        | WrapLinesDepCommand CenterVertically
        | WrapLinesDepCommand CenterHorizontally
        | WrapLinesDepCommand AdaptDisplayPos    ->
            myBasicDelegator.PerformViewCommand     isExtending command count
        | WrapLinesDepCommand _                  ->
            myBasicDelegator.PerformOnAllSelections isExtending command count

        | ModifyingCommand    x ->
            let isLinesModified = ref false

            myModifyingDelegator.PerformModifyingCommand isNormalMode x count isLinesModified

        | TextRangesCommand   x ->
            let isLinesModified = ref false

            myTextRangesDelegator.PerformTextRangesCommand x count isLinesModified

        | SelectionsCommand   x ->
            mySelectionsDelegator.PerformSelectionsCommand x count

        | UndoRedoCommand     x ->
            let isLinesApplied = ref false

            myUndoRedoDelegator.PerformUndoRedoCommand x count isLinesApplied

        mySelections.Sort ()

        myHasUndoToRegister <- myHasUndoToRegister ||
            match command with
            | WrapLinesDepCommand AdaptDisplayPos
            | UndoRedoCommand     _               -> false
            | _                                   -> true

        myDispatcher.ApplyChangedLinesCountIfNeeded ()

        let first, last = mySelections.GetSelectionsSpan ()
        myDispatcher.DisplayRenderer.TrimLinesCacheIfNeeded
            first.line last.line

    // rendering

    member this.GetDisplayRows () =
        myRenderer.GetDisplayRows this.DisplayPos

    member _.GetCursorPosForStatusArea () =
        myRenderer.GetCursorPosForStatusArea ()

    // search matching

    member this.SearchMatching regex isForward isExtending =
        let isInitial = (
               myMatchRanges.LastRegex <> Some regex
            || myMatchRanges.GetMainGroupCount () = 0
        )

        myMatchRanges.Search regex

        if not myUserMessages.HasErrorMessage then
            let command =
                if isForward then
                    CommonCommand (CursorToNextMatch isInitial)
                else
                    CommonCommand (CursorToPrevMatch isInitial)

            this.PerformCommand true isExtending command 1

    member _.ReSearchMatching () =
        myMatchRanges.ReSearch ()

    member _.ClearSearchMatching () =
        myMatchRanges.ClearSearch ()

    member this.ExtractMatching regex =
        this.WrapExtractAction (
            fun () -> myMatchRanges.Extract regex
        )

    member this.ReExtractMatching () =
        this.WrapExtractAction (
            fun () -> myMatchRanges.ReExtract ()
        )

    member this.ClearExtractMatching () =
        this.WrapExtractAction (
            fun () -> myMatchRanges.ClearExtract ()
        )

    member private this.WrapExtractAction action =
        let lineCurrent = mySelections.Main.Cursor.line

        let lineOrig =
            if myMatchRanges.IsClearedExtract then
                lineCurrent
            else
                myMatchRanges.LineExtractToLine lineCurrent

        action ()

        let lineNew =
            if myMatchRanges.IsClearedExtract then
                lineOrig
            else
                myMatchRanges.LineToLineExtract lineOrig

        let cursorNew = { line = lineNew; char = 0 }

        this.ResetState cursorNew

        let command = WrapLinesDepCommand CenterVertically
        this.PerformCommand true false command 1

        this.ResetUndoState ()

    member private this.ReSearchIfNeeded isInitial =
        if isInitial then
            true
        else
            if myMatchRanges.GetMainGroupCount () = 0 then
                this.ReSearchMatching ()
                true
            else
                false

    // Undo/Redo

    member this.RegisterUndo isStateVolatile =
        myUndoProvider.RegisterState
            (this.GetUndoState ()) isStateVolatile

        myHasUndoToRegister <- false

    member _.ClearIsLastUndoVolatile () =
        myUndoProvider.ClearIsLastStateVolatile ()

    member this.UndoCorruptedState () =
        myMatchRanges.ClearSearch ()
        myMatchRanges.ClearExtract ()

        let cursorNew = Position_Zero

        this.ResetState cursorNew
        this.ResetUndoState ()

    member private this.ResetUndoState () =
        myUndoProvider.Reset (this.GetInitialUndoState ())

        myHasUndoToRegister <- false

    member private this.GetInitialUndoState () =
        // the same code as in myUndoProvider initialization
        getBufferState (Some myLines) mySelections mySelsRegisters
            myWantedColumns this.DisplayPos

    member private this.GetUndoState () =
        getBufferState None mySelections mySelsRegisters
            myWantedColumns this.DisplayPos

    // others

    member this.ReloadFile () =
        let cursor      = this.Main.Cursor
        let cursorWC    = this.Main.CursorWC
        let displayLine = this.DisplayPos.line

        this.ResetStateAfterReload cursor cursorWC displayLine
        this.ResetUndoState ()

    member this.WriteFile encoding fileFormat endWithNewLine =
        FileUtils.writeFile this.FilePath encoding fileFormat endWithNewLine myLines

        myUndoProvider.SetCurrentStateAsSaved myContext.maxSavedUndos

    // auxiliary

    member private this.ResetState cursor =
        mySelections.Clear ()
        mySelections.Add {
            Selection_Zero with first = cursor; last = cursor
        }
        myWantedColumns.SetPendingWCActions [
            SetHardWantedColumn; SetSoftWantedColumn
        ]

        mySelsRegisters.Clear ()

        this.DisplayPos <- DisplayPos_Zero

        this.PrevCommand <- None

        myDispatcher.ApplyChangedLinesCountIfNeeded ()
        myDispatcher.DisplayRenderer.ResetLinesCache ()

    member private this.ResetStateAfterReload cursor cursorWC displayLine =
        // Remember any message from failed reload.
        let userMessage = myUserMessages.RetrieveMessage ()

        myMatchRanges.UpdateAfterReload ()

        match userMessage with
        | Some userMessage ->
            // Re-register the message from failed reload.
            myUserMessages.RetrieveMessage () |> ignore
            myUserMessages.RegisterMessage userMessage
        | None ->
            ()

        mySelections.Clear ()
        let newCursor = this.GetValidCursorPos cursor
        mySelections.Add {
            Selection_Zero with first   = newCursor ; last   = newCursor
                                firstWC = cursorWC  ; lastWC = cursorWC
        }

        mySelsRegisters.Clear ()

        let newDisplayLine = this.GetValidDisplayLine displayLine

        this.DisplayPos <- {
            DisplayPos_Zero with line = newDisplayLine
        }

        this.PrevCommand <- None

        myDispatcher.ApplyChangedLinesCountIfNeeded ()
        myDispatcher.DisplayRenderer.ResetLinesCache ()

    member private _.GetValidCursorPos cursor =
        let line  = min (myLines.Count - 1) cursor.line
        let char_ = min myLines[line].Length cursor.char

        { line = line; char = char_ }

    member private _.GetValidDisplayLine displayLine =
        min (myLines.Count - 1) displayLine

    // ITextAreaBuffer

    interface ITextAreaBuffer with

        // properties

        member this.FilePath
            with get ()    = this.FilePath
            and  set value = this.FilePath <- value

        member this.Lines                  = this.Lines
        member this.LinesForCompletion     = myParent.LinesForCompletion
        member this.Selections             = this.Selections
        member this.IsReadOnly             = this.IsReadOnly
        member this.IsBufferChanged        = this.IsBufferChanged
        member this.IsOrigBufferChanged    = myParent.IsBufferChanged
        member this.HasUndoToRegister      = this.HasUndoToRegister
        member this.HasUndoLinesToRegister = this.HasUndoLinesToRegister

        // commands

        member this.PerformCommand isNormalMode isExtending command count =
            this.PerformCommand isNormalMode isExtending command count

        // rendering

        member this.GetDisplayRows ()            = this.GetDisplayRows ()
        member this.GetCursorPosForStatusArea () = this.GetCursorPosForStatusArea ()

        // search matching

        member this.SearchMatching regex isForward isExtending =
            this.SearchMatching regex isForward isExtending

        member this.ReSearchMatching ()    = this.ReSearchMatching ()
        member this.ClearSearchMatching () = this.ClearSearchMatching ()

        // Undo/Redo

        member this.RegisterUndo isStateVolatile = this.RegisterUndo isStateVolatile
        member this.ClearIsLastUndoVolatile ()   = this.ClearIsLastUndoVolatile ()
        member this.UndoCorruptedState ()        = this.UndoCorruptedState ()

        // others

        member this.WriteFile encoding fileFormat endWithNewLine =
            this.WriteFile encoding fileFormat endWithNewLine

        member _.GetFirstChild () =
            None

    // IDisposable

    interface IDisposable with
        member this.Dispose () =
            myParent.UnregisterChild this

            myContextChangedDisposable.Dispose ()
            (myDispatcher :> IDisposable).Dispose ()
