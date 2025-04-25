namespace TextAreaDelegator

open Commands.InCommands
open UndoRedoPerformer

type TextAreaUndoRedo (
    inBasicState: BasicState,
    myDispatcher: TextAreaDispatcher.TextAreaDispatcher
) =
    inherit TextAreaDelegator (inBasicState)

    // Undo/Redo commands

    member this.PerformUndoRedoCommand command count (isLinesApplied: bool ref) =
        this.LoadBasicState ()

        myDispatcher.DisplayRenderer.DisallowLinesCache ()

        for _ = 1 to count do
            this.DelegateUndoRedoCommand command isLinesApplied

            this.PrevCommand <- Some (UndoRedoCommand command)

        myDispatcher.DisplayRenderer.ResetLinesCache ()

        this.StoreBasicState ()

    // delegating

    member private this.DelegateUndoRedoCommand command isLinesApplied =
        let state: UndoRedoCommandInState = {
            displayPos = this.DisplayPos
        }

        let outState =
            myDispatcher.UndoRedoPerformer.PerformCommand state command

        this.DisplayPos <- outState.displayPos

        if outState.isLinesApplied then
            isLinesApplied.Value <- true
