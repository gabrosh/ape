module UndoRedoPerformer

open Commands.InCommands
open Common
open DataTypes
open SelectionsRegisters

type UndoRedoCommandInState = {
    displayPos: DisplayPos
}

type UndoRedoCommandOutState = {
    displayPos:     DisplayPos
    isLinesApplied: bool
}

/// UndoRedoPerformer is performer for Undo/Redo commands.

type UndoRedoPerformer (
    myLines:         Lines,
    mySelections:    Selections.Selections,
    mySelsRegisters: SelectionsRegisters,
    myWantedColumns: Helpers.WantedColumns,
    myUndoProvider:  UndoProvider.UndoProvider
) =
    // set by PerformCommand
    let mutable myDisplayPos     = DisplayPos_Invalid
    let mutable myIsLinesApplied = false

    // commands

    /// Performs command on given state of the performer.
    /// Returns new state of the performer.
    member this.PerformCommand (state: UndoRedoCommandInState) command =
        myDisplayPos     <- state.displayPos
        myIsLinesApplied <- false

        match command with
        | Undo        -> this.Undo      ()
        | UndoFast    -> this.UndoFast  ()
        | Redo        -> this.Redo      ()
        | RedoFast    -> this.RedoFast  ()
        | AddName   r -> this.AddName   r
        | UndoNamed r -> this.UndoNamed r
        | RedoNamed r -> this.RedoNamed r

        let outState: UndoRedoCommandOutState = {
            displayPos     = myDisplayPos
            isLinesApplied = myIsLinesApplied
        }

        outState

    // Undo, Redo, ...

    member private this.Undo () =
        match myUndoProvider.GetUndoState () with
        | Some state -> this.ApplyState state
        | None       -> ()

    member private this.UndoFast () =
        match myUndoProvider.GetUndoStateFast () with
        | Some state -> this.ApplyState state
        | None       -> ()

    member private this.Redo () =
        match myUndoProvider.GetRedoState () with
        | Some state -> this.ApplyState state
        | None       -> ()

    member private this.RedoFast () =
        match myUndoProvider.GetRedoStateFast () with
        | Some state -> this.ApplyState state
        | None       -> ()

    member private _.AddName register =
        let name = getRegisterName register

        myUndoProvider.AddNameToCurrentState name

    member private this.UndoNamed register =
        let name = getRegisterName register

        match myUndoProvider.GetNamedStateBackward name with
        | Some state -> this.ApplyState state
        | None       -> ()

    member private this.RedoNamed register =
        let name = getRegisterName register

        match myUndoProvider.GetNamedStateForward name with
        | Some state -> this.ApplyState state
        | None       -> ()

    // UndoCorruptedState

    member this.UndoCorruptedState () =
        let state = myUndoProvider.GetCurrentState ()
        this.ApplyState state

    // auxiliary

    member private _.ApplyState state =
        match state.lines with
        | Some lines ->
            myLines.Clear ()
            myLines.AddRange lines
            myIsLinesApplied <- true
        | None ->
            ()

        mySelections.SetFrom state.selections state.mainIndex
        mySelsRegisters.SetAllFrom state.selsRegisters
        myWantedColumns.SetPendingWCActions state.pendingWCActions
        myDisplayPos <- state.displayPos
