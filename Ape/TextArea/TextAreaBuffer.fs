module TextAreaBuffer

open System
open System.Collections.Immutable

open Commands.InCommands
open Common
open Context
open DataTypes
open ITextAreaBuffer
open MatchRanges
open MatchRangesExtract
open Position
open Selection
open Selections
open SelectionsRegisters
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
type TextAreaBuffer (
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

    // LoadFile/ReloadFile/WriteFile mechanism
    let mutable myIsBufferChanged = false

    // ReloadFile mechanism
    let mutable myReloadFileParams: FileUtils.ReloadFileParams option = None

    // reporting a need for Undo registration
    let mutable myHasUndoToRegister      = false
    let mutable myHasUndoLinesToRegister = false

    // registered children (extract buffers)
    let myChildren = ResizeArray<ITextAreaBuffer> ()

    let myMatchRanges = MatchRanges (myUserMessages, myLines)

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

    member _.Lines                  = myLines
    member _.LinesForCompletion     = myLines
    member _.Selections             = mySelections
    member _.IsReadOnly             = myContext.readOnly
    member _.IsBufferChanged        = myIsBufferChanged
    member _.HasUndoToRegister      = myHasUndoToRegister
    member _.HasUndoLinesToRegister = myHasUndoLinesToRegister

    // only for testing purposes
    member _.Main = mySelections.Main

    // private properties

    member private _.DisplayPos
        with get ()    = myBasicState.displayPos
        and  set value = myBasicState.displayPos  <- value

    member private _.PrevCommand
        with get ()    = myBasicState.prevCommand
        and  set value = myBasicState.prevCommand <- value

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
                this.ReSearchOrClearMatching()
                myIsBufferChanged        <- true
                myHasUndoLinesToRegister <- true

        | TextRangesCommand   x ->
            let isLinesModified = ref false

            myTextRangesDelegator.PerformTextRangesCommand x count isLinesModified

            if isLinesModified.Value then
                this.ReSearchOrClearMatching()
                myIsBufferChanged        <- true
                myHasUndoLinesToRegister <- true

        | SelectionsCommand   x ->
            mySelectionsDelegator.PerformSelectionsCommand x count

        | UndoRedoCommand     x ->
            let isLinesApplied = ref false

            myUndoRedoDelegator.PerformUndoRedoCommand x count isLinesApplied

            if isLinesApplied.Value then
                this.ReSearchOrClearMatching()
                myIsBufferChanged <- not (
                    myUndoProvider.IsCurrentStateSaved
                )

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

    member _.CreateMatchRangesExtract linesExtract extractOnConstr =
        myMatchRanges.CreateExtract MatchRangesExtract linesExtract extractOnConstr

    member _.RegisterChild buffer =
        myChildren.Add buffer

    member _.UnregisterChild buffer =
        myChildren.Remove buffer
            |> ignore

    member private this.ReSearchIfNeeded isInitial =
        if isInitial then
            true
        else
            if myMatchRanges.GetMainGroupCount () = 0 then
                this.ReSearchMatching ()
                true
            else
                false

    member private _.ReSearchOrClearMatching () =
        if myContext.reSearchMatching then
            if not myMatchRanges.IsCleared then
                myMatchRanges.ReSearch ()
        else
            myMatchRanges.ClearSearch ()

    // Undo/Redo

    member this.RegisterUndo isStateVolatile =
        myUndoProvider.RegisterState
            (this.GetUndoState myHasUndoLinesToRegister) isStateVolatile

        myHasUndoToRegister      <- false
        myHasUndoLinesToRegister <- false

    member _.ClearIsLastUndoVolatile () =
        myUndoProvider.ClearIsLastStateVolatile ()

    member this.UndoCorruptedState () =
        myDispatcher.UndoRedoPerformer.UndoCorruptedState ()

        myHasUndoToRegister      <- false
        myHasUndoLinesToRegister <- false

        this.ClearSearchMatching()

        myDispatcher.ApplyChangedLinesCountIfNeeded ()
        myDispatcher.DisplayRenderer.ResetLinesCache ()

    member private this.ResetUndoState () =
        myUndoProvider.Reset (this.GetInitialUndoState ())

        myHasUndoToRegister      <- false
        myHasUndoLinesToRegister <- false

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

    member this.LoadStrings (lines: string seq) =
        myLines.Clear ()

        try
            this.LoadStringsAux lines myLines
        finally
            this.AssureNonZeroLinesCount ()
            this.ResetState ()
            this.ResetUndoState ()

    member this.LoadFile encoding strictEncoding quite =
        myLines.Clear ()

        let reloadFileParams = None

        try
            let fileFormat, endsWithNewLine, reloadFileParams' =
                this.LoadFileAux encoding strictEncoding quite reloadFileParams

            TestMines.checkMine (nameof this.LoadFile)

            myReloadFileParams <- reloadFileParams'

            (fileFormat, endsWithNewLine)
        finally
            this.AssureNonZeroLinesCount ()
            this.ResetState ()
            this.ResetUndoState ()

    member this.ReloadFile encoding strictEncoding =
        let cursor      = this.Main.Cursor
        let cursorWC    = this.Main.CursorWC
        let displayLine = this.DisplayPos.line

        if not myContext.reloadAsLogFile then
            myLines.Clear ()

        let reloadFileParams =
            if myContext.reloadAsLogFile then
                myReloadFileParams
            else
                None

        try
            let fileFormat, endsWithNewLine, reloadFileParams' =
                this.LoadFileAux encoding strictEncoding false reloadFileParams

            TestMines.checkMine (nameof this.ReloadFile)

            myReloadFileParams <- reloadFileParams'

            (fileFormat, endsWithNewLine)
        finally
            this.AssureNonZeroLinesCount ()
            this.ResetStateAfterReload cursor cursorWC displayLine
            this.ResetUndoState ()

    member this.WriteFile encoding fileFormat endWithNewLine =
        FileUtils.writeFile this.FilePath encoding fileFormat endWithNewLine myLines

        myUndoProvider.SetCurrentStateAsSaved myContext.maxSavedUndos

        myIsBufferChanged <- false

    // others - private

    member private _.LoadStringsAux lines result =
        for line in lines do
            result.Add (stringToChars line)

    member private this.LoadFileAux encoding strictEncoding quite reloadFileParams =
        try
            FileUtils.readFile this.FilePath encoding strictEncoding reloadFileParams myLines
        with
        | :? System.IO.DirectoryNotFoundException as ex ->
            if not quite then
                myUserMessages.RegisterMessage (
                    UserMessages.makeWarningMessage ex.Message
                )
            (FileUtils.FileFormat.dos, true, None)
        | :? System.IO.FileNotFoundException as ex ->
            if not quite then
                myUserMessages.RegisterMessage (
                    UserMessages.makeInfoMessage ex.Message
                )
            (FileUtils.FileFormat.dos, true, None)

    member private _.AssureNonZeroLinesCount () =
        if myLines.Count = 0 then
            myLines.Add Chars.Empty

    // auxiliary

    member private this.ResetState () =
        mySelections.Clear ()
        mySelections.Add Selection_Zero
        myWantedColumns.SetPendingWCActions []

        mySelsRegisters.Clear ()

        myMatchRanges.ClearSearch ()

        this.DisplayPos <- DisplayPos_Zero

        this.PrevCommand <- None

        myDispatcher.ApplyChangedLinesCountIfNeeded ()
        myDispatcher.DisplayRenderer.ResetLinesCache ()

        myIsBufferChanged <- false

    member private this.ResetStateAfterReload cursor cursorWC displayLine =
        // Remember any message from failed reload.
        let userMessage = myUserMessages.RetrieveMessage ()

        myMatchRanges.UpdateAfterReload ()

        match userMessage with
        | Some userMessage ->
            myUserMessages.RetrieveMessage () |> ignore
            // Re-register the message from failed reload.
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

        myIsBufferChanged <- false

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
        member this.LinesForCompletion     = this.LinesForCompletion
        member this.Selections             = this.Selections
        member this.IsReadOnly             = this.IsReadOnly
        member this.IsBufferChanged        = this.IsBufferChanged
        member this.IsOrigBufferChanged    = this.IsBufferChanged
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

        member this.ReloadFile encoding strictEncoding =
            this.ReloadFile encoding strictEncoding

        member this.WriteFile encoding fileFormat endWithNewLine =
            this.WriteFile encoding fileFormat endWithNewLine

        member _.GetFirstChild () =
            if myChildren.Count <> 0 then
                Some myChildren[0]
            else
                None

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable.Dispose ()
            (myDispatcher :> IDisposable).Dispose ()
