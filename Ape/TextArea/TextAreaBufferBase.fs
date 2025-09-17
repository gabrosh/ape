module TextAreaBufferBase

open System
open System.Collections.Immutable

open Commands.InCommands
open Common
open Context
open DataTypes
open MatchRanges
open Position
open Selection
open Selections
open SelectionsRegisters
open TextAreaDelegator
open TextAreaFileSupport
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

[<AbstractClass>]
type TextAreaBufferBase (
    myContextRef:   IWrappedRef<MainContext>,
    myUserMessages: UserMessages,
    myRegisters:    Registers.Registers,
    inFilePath:     string,
    myLines:        Lines,
    myMatchRanges:  MatchRanges
) =
    let mutable myContext = myContextRef.Value
    let handleContextChanged () = myContext <- myContextRef.Value
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    let mutable myFilePath   = inFilePath
    let mutable myBufferName = inFilePath

    let myBasicState = {
        displayPos = DisplayPos_Zero; prevCommand = None
    }

    let myFileSupport = new TextAreaFileSupport (
        myContextRef, myUserMessages, myLines
    )

    // fields affected by Undo/Redo
    let mySelectionsArray = ResizeArray<Selection> [Selection_Zero]
    let mySelections      = Selections mySelectionsArray
    let mySelsModifier    = TextRangesModifier mySelectionsArray
    let mySelsRegisters   = SelectionsRegisters ()
    let myWantedColumns   = Helpers.WantedColumns mySelections

    let myUndoProvider = new UndoProvider (
        myContextRef, myUserMessages,
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

    member _.FilePath
        with get ()    = myFilePath
        and  set value = myFilePath   <- value
                         myBufferName <- value

    member _.BufferName
        with get ()    = myBufferName
        and  set value = myBufferName <- value

    member _.MatchRanges = myMatchRanges

    // also for testing purposes
    member _.Lines      = myLines
    member _.Selections = mySelections
    member _.Main       = mySelections.Main

    // internal properties

    member internal _.DisplayPos
        with get ()    = myBasicState.displayPos
        and  set value = myBasicState.displayPos  <- value

    member internal _.PrevCommand
        with get ()    = myBasicState.prevCommand
        and  set value = myBasicState.prevCommand <- value

    member internal _.Context       = myContext
    member internal _.FileSupport   = myFileSupport
    member internal _.SelsRegisters = mySelsRegisters
    member internal _.WantedColumns = myWantedColumns
    member internal _.UndoProvider  = myUndoProvider
    member internal _.Dispatcher    = myDispatcher

    member internal _.IsReadOnly = myContext.readOnly

    // LoadFile/Reload/WriteFile mechanism
    member val internal IsBufferChanged = false
        with get, set

    // reporting a need for Undo registration
    member val internal HasUndoToRegister      = false
       with get, set
    member val internal HasUndoLinesToRegister = false
       with get, set

    // abstract members

    abstract member ReSearchIfNeeded:
        isInitial: bool -> bool

    abstract member ReSearchOrClearMatching:
        unit -> unit

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
            let command' = CommonCommand (CursorToNextMatch isInitial')

            myBasicDelegator.PerformMatchCommand isExtending command' count true

        | CommonCommand (CursorToPrevMatch isInitial) ->
            let isInitial' = this.ReSearchIfNeeded isInitial
            let command' = CommonCommand (CursorToPrevMatch isInitial')

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

            if isLinesModified.Value then
                this.ReSearchOrClearMatching ()
                this.IsBufferChanged        <- true
                this.HasUndoLinesToRegister <- true

        | TextRangesCommand   x ->
            let isLinesModified = ref false

            myTextRangesDelegator.PerformTextRangesCommand x count isLinesModified

            if isLinesModified.Value then
                this.ReSearchOrClearMatching ()
                this.IsBufferChanged        <- true
                this.HasUndoLinesToRegister <- true

        | SelectionsCommand   x ->
            mySelectionsDelegator.PerformSelectionsCommand x count

        | UndoRedoCommand     x ->
            let isLinesApplied = ref false

            myUndoRedoDelegator.PerformUndoRedoCommand x count isLinesApplied

            if isLinesApplied.Value then
                this.ReSearchOrClearMatching ()
                this.IsBufferChanged <- not (
                    myUndoProvider.IsCurrentStateSaved
                )

        mySelections.Sort ()

        this.HasUndoToRegister <- this.HasUndoToRegister ||
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

    // Undo/Redo

    member this.RegisterUndo isStateVolatile =
        myUndoProvider.RegisterState
            (this.GetUndoState this.HasUndoLinesToRegister) isStateVolatile

        this.HasUndoToRegister      <- false
        this.HasUndoLinesToRegister <- false

    member _.ClearIsLastUndoVolatile () =
        myUndoProvider.ClearIsLastStateVolatile ()

    member internal this.ResetUndoState () =
        myUndoProvider.Reset (this.GetInitialUndoState ())

        this.HasUndoToRegister      <- false
        this.HasUndoLinesToRegister <- false

    member private this.GetInitialUndoState () =
        // the same code as in myUndoProvider initialization
        getBufferState (Some myLines) mySelections mySelsRegisters
            myWantedColumns this.DisplayPos

    member private this.GetUndoState withLines =
        let lines =
            if withLines then
                Some myLines
            else
                None

        getBufferState lines mySelections mySelsRegisters
            myWantedColumns this.DisplayPos

    // others

    member this.WriteFile filePath encoding fileFormat endWithNewLine =
        let result =
            myFileSupport.WriteFile filePath encoding fileFormat endWithNewLine myLines

        match result with
        | Ok ()   ->
            myUndoProvider.SetCurrentStateAsSaved ()
            myUndoProvider.RemoveOldStates myContext.maxSavedUndos
            this.IsBufferChanged <- false
            Ok ()

        | Error e ->
            Error e

    // auxiliary

    member internal _.GetValidCursorPos (cursor: Position) =
        let line  = min (myLines.Count - 1) cursor.line
        let char_ = min myLines[line].Length cursor.char

        { line = line; char = char_ }

    member internal _.GetValidDisplayLine displayLine =
        min (myLines.Count - 1) displayLine

    // IDisposable

    abstract Dispose: unit -> unit

    default _.Dispose () =
        myContextChangedDisposable.Dispose ()
        (myFileSupport  :> IDisposable).Dispose ()
        (myUndoProvider :> IDisposable).Dispose ()
        (myDispatcher   :> IDisposable).Dispose ()

    interface IDisposable with
        member this.Dispose () =
            this.Dispose ()
