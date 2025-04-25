namespace TextAreaDelegator

open Commands.InCommands

type TextAreaSelections (
    inBasicState:    BasicState,
    myWantedColumns: Helpers.WantedColumns,
    myDispatcher:    TextAreaDispatcher.TextAreaDispatcher
) =
    inherit TextAreaDelegator (inBasicState)

    // selections commands

    member this.PerformSelectionsCommand command count =
        this.LoadBasicState ()

        let inputs, outputs =
            myDispatcher.SelectionsPerformer.GetWantedColumnsActions command

        for _ = 1 to count do
            myWantedColumns.PerformInputWCActions myDispatcher.GetCharWantedColumns inputs

            this.DelegateSelectionsCommand command

            myWantedColumns.RegisterOutputWCActions outputs

            this.PrevCommand <- Some (SelectionsCommand command)

        this.StoreBasicState ()

    // delagating

    member private _.DelegateSelectionsCommand command =
        myDispatcher.SelectionsPerformer.PerformCommand
            myDispatcher.DisplayRenderer command
