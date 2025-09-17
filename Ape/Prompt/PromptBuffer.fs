module PromptBuffer

open System
open System.Collections.Immutable

open Commands.InCommands
open Common
open Context
open DataTypes
open PromptDelegators
open Selection
open Selections
open SelectionsRegisters
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
        selsRegisters    = selsRegisters.GetAllImmutable()
        displayPos       = displayPos
        pendingWCActions = wantedColumns.GetPendingWCActions ()
    }

[<Sealed>]
type PromptBuffer (
    inContextRef:      IWrappedRef<MainContext>,
    inExtraContextRef: IWrappedRef<ExtraContext>,
    inUserMessages:    UserMessages,
    myRegisters:       Registers.Registers
) =
    let myBasicState = {
        displayPos = DisplayPos_Zero
    }

    // fields affected by Undo/Redo
    let myLines           = Lines [Chars.Empty]
    let mySelectionsArray = ResizeArray<Selection> [Selection_Zero]
    let mySelections      = Selections mySelectionsArray
    let mySelsModifier    = TextRangesModifier mySelectionsArray
    let mySelsRegisters   = SelectionsRegisters ()
    let myWantedColumns   = Helpers.WantedColumns mySelections

    // reporting a need for Undo registration
    let mutable myHasUndoToRegister      = false
    let mutable myHasUndoLinesToRegister = false

    let myMatchRanges = MatchRanges.MatchRanges (inUserMessages, myLines)

    let myUndoProvider = new UndoProvider (
        inContextRef, inUserMessages,
        // the same code as in GetInitialUndoState, which can't be called here
        getBufferState (Some myLines) mySelections mySelsRegisters
            myWantedColumns myBasicState.displayPos
    )
    let myDispatcher = new PromptDispatcher.PromptDispatcher (
        inContextRef, inExtraContextRef,
        inUserMessages, myLines, myRegisters, mySelections, mySelsRegisters,
        myWantedColumns, myMatchRanges, myUndoProvider
    )
    let myRenderer = PromptRenderer.PromptRenderer (
        mySelections, myDispatcher
    )

    // delegators

    let myBasicDelegator = PromptDelegators.PromptBasic (
        myBasicState,
        mySelections,
        myDispatcher
    )
    let myModifyingDelegator = PromptDelegators.PromptModifying (
        inUserMessages,
        myBasicState,
        myLines,
        myRegisters,
        mySelections, mySelsModifier,
        myDispatcher
    )
    let myTextRangesDelegator = PromptDelegators.PromptTextRanges (
        myBasicState,
        myLines,
        mySelections, mySelsModifier,
        myDispatcher
    )
    let mySelectionsDelegator = PromptDelegators.PromptSelections (
        myBasicState,
        myDispatcher
    )
    let myUndoRedoDelegator = PromptDelegators.PromptUndoRedo (
        myBasicState,
        myDispatcher
    )

    // public properties

    member _.Line = myLines[0]
    member _.Main = mySelections.Main

    member _.HasUndoToRegister      = myHasUndoToRegister
    member _.HasUndoLinesToRegister = myHasUndoLinesToRegister

    // only for testing purposes
    member _.Lines      = myLines
    member _.Selections = mySelections

    // private properties

    member private _.DisplayPos
        with get ()    = myBasicState.displayPos
        and  set value = myBasicState.displayPos <- value

    // commands

    member _.PerformCommand isNormalMode isExtending command =

        match command with
        | CommonCommand       _ ->
            myBasicDelegator.PerformOnSelection isExtending command

        | WrapLinesDepCommand _ ->
            myBasicDelegator.PerformOnSelection isExtending command

        | ModifyingCommand    x ->
            let isLinesModified = ref false

            myModifyingDelegator.PerformModifyingCommand isNormalMode x isLinesModified

            if isLinesModified.Value then
                myHasUndoLinesToRegister <- true

        | TextRangesCommand   x ->
            let isLinesModified = ref false

            myTextRangesDelegator.PerformTextRangesCommand x isLinesModified

            if isLinesModified.Value then
                myHasUndoLinesToRegister <- true

        | SelectionsCommand   x ->
            mySelectionsDelegator.PerformSelectionsCommand x

        | UndoRedoCommand     x ->
            myUndoRedoDelegator.PerformUndoRedoCommand x

        myHasUndoToRegister <- myHasUndoToRegister ||
            match command with
            | WrapLinesDepCommand AdaptDisplayPos
            | UndoRedoCommand     _               -> false
            | _                                   -> true

    // rendering

    member this.GetDisplayRows () =
        myRenderer.GetDisplayRows this.DisplayPos

    // Undo/Redo

    member this.RegisterUndo isStateVolatile =
        let withLines = myHasUndoLinesToRegister

        myUndoProvider.RegisterState (this.GetUndoState withLines) isStateVolatile

        myHasUndoToRegister      <- false
        myHasUndoLinesToRegister <- false

    member _.ClearIsLastUndoVolatile () =
        myUndoProvider.ClearIsLastStateVolatile ()

    member _.UndoCorruptedState () =
        myDispatcher.UndoRedoPerformer.UndoCorruptedState ()

        myHasUndoToRegister      <- false
        myHasUndoLinesToRegister <- false

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

    member this.LoadString (line: string) =
        myLines.Clear ()
        this.LoadStringAux line
        this.ResetState ()

        this.ResetUndoState ()

    member this.LoadCharsFromHistory (chars: Chars) =
        myLines.Clear ()
        this.LoadCharsAux chars
        this.ResetState ()

        let command = (CommonCommand CursorAtEol)
        this.PerformCommand false false command

        this.ResetUndoState ()

    // others - private

    member private _.LoadStringAux (line: string) =
        myLines.Add (stringToChars line)

    member private _.LoadCharsAux (chars: Chars) =
        myLines.Add chars

    // auxiliary

    member private this.ResetState () =
        mySelections.Clear ()
        mySelections.Add Selection_Zero
        myWantedColumns.SetPendingWCActions []

        mySelsRegisters.Clear ()

        this.DisplayPos <- DisplayPos_Zero

        myDispatcher.DisplayRenderer.ResetLinesCache ()

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            (myUndoProvider :> IDisposable).Dispose ()
            (myDispatcher   :> IDisposable).Dispose ()
