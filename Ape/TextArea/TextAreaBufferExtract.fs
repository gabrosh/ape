module TextAreaBufferExtract

open System
open System.Collections.Immutable

open Commands.InCommands
open Common
open Context
open DataTypes
open ITextAreaBuffer
open MatchRangesExtract
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
    myParent:       TextAreaBuffer,
    myContextRef:   IWrappedRef<MainContext>,
    myUserMessages: UserMessages,
    myRegisters:    Registers.Registers,
    inFilePath:     string
) =
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

    // LoadFile/WriteFile mechanism
    let mutable myIsBufferChanged = false

    // reporting a need for Undo registration
    let mutable myHasUndoToRegister = false

    let myMatchRanges = myParent.CreateMatchRangesExtract MatchRangesExtract myLines

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

    // constructor

    do
        myMatchRanges.Init ()

    // public properties

    member val FilePath = inFilePath
        with get, set

    member _.Lines                  = myLines
    member _.Selections             = mySelections
    member _.IsReadOnly             = true
    member _.IsBufferChanged        = myIsBufferChanged
    member _.HasUndoToRegister      = myHasUndoToRegister
    member _.HasUndoLinesToRegister = false

    // only for testing purposes
    member _.Main = mySelections.Main

    // private properties

    member private _.DisplayPos
        with get ()    = myBasicState.displayPos
        and  set value = myBasicState.displayPos  <- value

    // commands

    member this.PerformCommand isNormalMode isExtending command count =
        if this.IsReadOnly && isWriteCommand command then
            myUserMessages.RegisterMessage ERROR_FILE_OPENED_AS_READ_ONLY
        else
            this.PerformCommandAux isNormalMode isExtending command count

    member private _.PerformCommandAux isNormalMode isExtending command count =

        match command with
        | CommonCommand (CursorToNextMatch _)    ->
            myBasicDelegator.PerformMatchCommand    isExtending command count true
        | CommonCommand (CursorToPrevMatch _)    ->
            myBasicDelegator.PerformMatchCommand    isExtending command count false
        | CommonCommand _                        ->
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

    member this.SearchMatching regex _isForward _isExtending =
        myMatchRanges.Search regex

        this.ResetState ()
        this.ResetUndoState ()

    member this.ReSearchMatching () =
        myMatchRanges.ReSearch ()

        this.ResetState ()
        this.ResetUndoState ()

    member this.ClearMatching () =
        myMatchRanges.Clear ()

        this.ResetState ()
        this.ResetUndoState ()

    // Undo/Redo

    member this.RegisterUndo isStateVolatile =
        myUndoProvider.RegisterState
            (this.GetUndoState ()) isStateVolatile

        myHasUndoToRegister <- false

    member _.ClearIsLastUndoVolatile () =
        myUndoProvider.ClearIsLastStateVolatile ()

    member _.UndoCorruptedState () =
        myDispatcher.UndoRedoPerformer.UndoCorruptedState ()

        //this.ClearMatching()

        myHasUndoToRegister <- false

        myDispatcher.ApplyChangedLinesCountIfNeeded ()

        myDispatcher.DisplayRenderer.ResetLinesCache ()

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

    member this.ReloadFile encoding strictEncoding =
        let result = myParent.ReloadFile encoding strictEncoding

        this.ResetState ()
        this.ResetUndoState ()

        result

    member this.WriteFile encoding fileFormat endWithNewLine =
        FileUtils.writeFile this.FilePath encoding fileFormat endWithNewLine myLines

        myUndoProvider.SetCurrentStateAsSaved myContext.maxSavedUndos

        myIsBufferChanged <- false

    // auxiliary

    member private this.ResetState () =
        mySelections.Clear ()
        mySelections.Add Selection_Zero

        mySelsRegisters.Clear ()

        this.DisplayPos <- DisplayPos_Zero

        myIsBufferChanged <- false

        myDispatcher.DisplayRenderer.ResetLinesCache ()

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

        member this.ReSearchMatching ()  = this.ReSearchMatching ()
        member this.ClearMatching ()     = this.ClearMatching ()

        // Undo/Redo

        member this.RegisterUndo isStateVolatile = this.RegisterUndo isStateVolatile
        member this.ClearIsLastUndoVolatile ()   = this.ClearIsLastUndoVolatile ()
        member this.UndoCorruptedState ()        = this.UndoCorruptedState ()

        // others

        member this.ReloadFile encoding strictEncoding =
            this.ReloadFile encoding strictEncoding

        member this.WriteFile encoding fileFormat endWithNewLine =
            this.WriteFile encoding fileFormat endWithNewLine

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable.Dispose ()
            (myDispatcher :> IDisposable).Dispose ()
