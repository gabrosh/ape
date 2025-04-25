namespace PromptDelegators

open UndoRedoPerformer

type PromptUndoRedo (
    inBasicState: BasicState,
    myDispatcher: PromptDispatcher.PromptDispatcher
) =
    inherit PromptDelegator (inBasicState)

    // Undo/Redo commands

    member this.PerformUndoRedoCommand command =
        this.LoadBasicState ()

        myDispatcher.DisplayRenderer.DisallowLinesCache ()
        this.DelegateUndoRedoCommand command
        myDispatcher.DisplayRenderer.ResetLinesCache ()

        this.StoreBasicState ()

    // delagating

    member private this.DelegateUndoRedoCommand command =
        let state: UndoRedoCommandInState = {
            displayPos = this.DisplayPos
        }

        let outState =
            myDispatcher.UndoRedoPerformer.PerformCommand state command

        this.DisplayPos <- outState.displayPos
