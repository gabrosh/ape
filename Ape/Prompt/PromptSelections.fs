namespace PromptDelegators

type PromptSelections (
    inBasicState: BasicState,
    myDispatcher: PromptDispatcher.PromptDispatcher
) =
    inherit PromptDelegator (inBasicState)

    // selections commands

    member this.PerformSelectionsCommand command =
        this.LoadBasicState ()

        this.DelegateSelectionsCommand command

        this.StoreBasicState ()

    // delegating

    member private _.DelegateSelectionsCommand command =
        myDispatcher.SelectionsPerformer.PerformCommand
            myDispatcher.DisplayRenderer command
