namespace TextAreaDelegator

open Commands.InCommands
open Commands.OutCommands
open Position
open Selection
open TextRange
open TextRangesModifier

open TextRangesPerformer

type TextAreaTextRanges (
    inBasicState:    BasicState,
    mySelections:    Selections.Selections,
    mySelsModifier:  TextRangesModifier<Selection>,
    mySelsRegisters: SelectionsRegisters.SelectionsRegisters,
    myWantedColumns: Helpers.WantedColumns,
    myDispatcher:    TextAreaDispatcher.TextAreaDispatcher
) =
    inherit TextAreaDelegator (inBasicState)

    // text ranges commands

    member this.PerformTextRangesCommand command count (isLinesModified: bool ref) =
        this.LoadBasicState ()

        let inputs, outputs =
            myDispatcher.TextRangesPerformer.GetWantedColumnsActions command

        // Repeatable command must not request any wanted columns inputs.
        if count > 1 && not (List.isEmpty inputs) then
            invalidOp ""

        let dpToken =
            myDispatcher.DisplayRenderer.GetDisplayPosToken
                (TextRangesCommand command) this.DisplayPos

        myWantedColumns.PerformInputWCActions myDispatcher.GetCharWantedColumns inputs

        let delegateFun = this.DelegateTextRangesCommand isLinesModified dpToken
        this.SwitchToTextRangesCommand command count delegateFun

        myWantedColumns.RegisterOutputWCActions outputs

        // Modify display position only if it's not at the start of file.
        if not (this.DisplayPos.line = 0 && this.DisplayPos.lineRow = 0) then
            this.DisplayPos <-
                myDispatcher.DisplayRenderer.GetModifiedDisplayPos dpToken

        this.StoreBasicState ()

    member private this.SwitchToTextRangesCommand command count delegateFun =
        myDispatcher.DisplayRenderer.DisallowLinesCache ()

        for _ = 1 to count do
            match command with
            | Indent
            | Unindent -> this.PerformLinesTextRangesCommand delegateFun command
            | _        -> this.PerformCharsTextRangesCommand delegateFun command

            this.PrevCommand <- Some (TextRangesCommand command)

        myDispatcher.DisplayRenderer.ResetLinesCache ()

    member private this.PerformCharsTextRangesCommand delegateFun command =
        let textRanges = mySelections.GetSelectedRanges ()
        let textRangesModifier = TextRangesModifier textRanges

        this.StartApplyingModifications textRangesModifier

        for i = 0 to textRanges.Count - 1 do
            delegateFun textRanges textRangesModifier command i

        this.StopApplyingModifications textRangesModifier

    member private this.PerformLinesTextRangesCommand delegateFun command =
        let selectedRanges = mySelections.GetSelectedRanges ()
        let textRanges = this.MergeRangesByLines selectedRanges
        let textRangesModifier = TextRangesModifier textRanges

        this.StartApplyingModifications textRangesModifier

        for i = 0 to textRanges.Count - 1 do
            delegateFun textRanges textRangesModifier command i

        this.StopApplyingModifications textRangesModifier

    member private _.MergeRangesByLines ranges : ResizeArray<TextRange> =
        let mergedRanges = ResizeArray<TextRange> ()

        let range = ranges |> Seq.head
        let mutable prevRange = {
            range with first.char = 0; last.char = 0
        }

        for range in ranges |> Seq.tail do
            if prevRange.last.line = range.first.line then
                prevRange <- {
                    prevRange with last.line = range.last.line
                                   last.char = 0
                }
            else
                mergedRanges.Add prevRange
                prevRange <- {
                    range with first.char = 0; last.char = 0
                }

        mergedRanges.Add prevRange
        mergedRanges

    // delegating

    member private _.DelegateTextRangesCommand
        isLinesModified displayPosToken textRanges textRangesModifier command textRangeIndex =

        textRangesModifier.AssureItemsPreparedUpTo textRangeIndex

        let state: TextRangesCommandInState = {
            textRangeIndex = textRangeIndex
            textRange      = textRanges[textRangeIndex]
        }

        let outState, outCommands =
            myDispatcher.TextRangesPerformer.PerformCommand state command

        for outCommand in outCommands do
            match outCommand with
            | ApplyInsert        spec -> mySelsModifier    .ApplyInsert        spec
                                         mySelsRegisters   .ApplyInsert        spec
                                         textRangesModifier.ApplyInsert        spec
            | ApplyDelete        spec -> mySelsModifier    .ApplyDelete        spec
                                         mySelsRegisters   .ApplyDelete        spec
                                         textRangesModifier.ApplyDelete        spec
            | ApplyOneLineInsert spec -> mySelsModifier    .ApplyOneLineInsert spec
                                         mySelsRegisters   .ApplyOneLineInsert spec
                                         textRangesModifier.ApplyOneLineInsert spec
            | ApplyOneLineDelete spec -> mySelsModifier    .ApplyOneLineDelete spec
                                         mySelsRegisters   .ApplyOneLineDelete spec
                                         textRangesModifier.ApplyOneLineDelete spec

            displayPosToken.Modify outCommand

        if outState.isLinesModified then
            isLinesModified.Value <- true

    // auxiliary

    member private _.StartApplyingModifications textRangesModifier =
        mySelsModifier.StartApplyingModifications ()
        mySelsRegisters.StartApplyingModifications ()
        textRangesModifier.StartApplyingModifications ()

    member private _.StopApplyingModifications textRangesModifier =
        mySelsModifier.StopApplyingModifications ()
        mySelsRegisters.StopApplyingModifications ()
        textRangesModifier.StopApplyingModifications ()
