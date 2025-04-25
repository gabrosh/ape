namespace PromptDelegators

open Commands.InCommands
open Commands.OutCommands
open Common
open DataTypes
open Selection
open TextRange
open TextRangesModifier

open TextRangesPerformer

type PromptTextRanges (
    inBasicState:   BasicState,
    myLines:        Lines,
    mySelections:   Selections.Selections,
    mySelsModifier: TextRangesModifier<Selection>,
    myDispatcher:   PromptDispatcher.PromptDispatcher
) =
    inherit PromptDelegator (inBasicState)

    // text ranges commands

    member this.PerformTextRangesCommand command (isLinesModified: bool ref) =
        this.LoadBasicState ()

        let dpToken =
            myDispatcher.DisplayRenderer.GetDisplayPosToken
                (TextRangesCommand command) this.DisplayPos

        let delegateFun = this.DelegateTextRangesCommand isLinesModified dpToken
        this.SwitchToTextRangesCommand command delegateFun

        // Modify display position only if it's not at the start of file.
        if not (this.DisplayPos.line = 0 && this.DisplayPos.lineRow = 0) then
            this.DisplayPos <-
                myDispatcher.DisplayRenderer.GetModifiedDisplayPos dpToken

        this.StoreBasicState ()

    member private this.SwitchToTextRangesCommand command delegateFun =
        myDispatcher.DisplayRenderer.DisallowLinesCache ()

        match command with
        | Indent
        | Unindent -> this.PerformLinesTextRangesCommand delegateFun command
        | _        -> this.PerformBasicTextRangesCommand delegateFun command

        myDispatcher.DisplayRenderer.ResetLinesCache ()

    member private this.PerformBasicTextRangesCommand delegateFun command =
        let textRanges = mySelections.GetSelectedRanges ()
        let textRangesModifier = TextRangesModifier textRanges

        this.StartApplyingModifications ()
        textRangesModifier.StartApplyingModifications ()
        delegateFun textRanges textRangesModifier command
        this.StopApplyingModifications ()
        textRangesModifier.StopApplyingModifications ()

    member private this.PerformLinesTextRangesCommand delegateFun command =
        let selectedRanges = mySelections.GetSelectedRanges ()
        let textRanges = this.MergeRangesByLines selectedRanges
        let textRangesModifier = TextRangesModifier textRanges

        this.StartApplyingModifications ()
        textRangesModifier.StartApplyingModifications ()
        delegateFun textRanges textRangesModifier command
        this.StopApplyingModifications ()
        textRangesModifier.StopApplyingModifications ()

    member private _.MergeRangesByLines ranges : ResizeArray<TextRange> =
        let mergedRanges = ResizeArray<TextRange> ()

        let range = ranges |> Seq.head
        let lastChar = myLines[range.last.line].Length
        let mutable prevRange = {
            range with first.char = 0; last.char = lastChar
        }

        mergedRanges.Add prevRange
        mergedRanges

    // delegating

    member private _.DelegateTextRangesCommand
        isLinesModified displayPosToken textRanges textRangesModifier command =

        textRangesModifier.AssureItemsPreparedUpTo 0

        let state: TextRangesCommandInState = {
            textRangeIndex = 0
            textRange      = textRanges[0]
        }

        let outState, outCommands =
            myDispatcher.TextRangesPerformer.PerformCommand state command

        for outCommand in outCommands do
            match outCommand with
            | ApplyInsert        spec -> mySelsModifier    .ApplyInsert        spec
                                         textRangesModifier.ApplyInsert        spec
            | ApplyDelete        spec -> mySelsModifier    .ApplyDelete        spec
                                         textRangesModifier.ApplyDelete        spec
            | ApplyOneLineInsert spec -> mySelsModifier    .ApplyOneLineInsert spec
                                         textRangesModifier.ApplyOneLineInsert spec
            | ApplyOneLineDelete spec -> mySelsModifier    .ApplyOneLineDelete spec
                                         textRangesModifier.ApplyOneLineDelete spec

            displayPosToken.Modify outCommand

        if outState.isLinesModified then
            isLinesModified.Value <- true

    // auxiliary

    member private _.StartApplyingModifications () =
        mySelsModifier.StartApplyingModifications ()

    member private _.StopApplyingModifications () =
        mySelsModifier.StopApplyingModifications ()
