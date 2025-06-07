namespace TextAreaDelegator

open Commands.InCommands
open Commands.OutCommands
open Common
open DataTypes
open Position
open Selection
open TextRangesModifier

open ModifyingPerformer

type TextAreaModifying (
    inBasicState:    BasicState,
    myLines:         Lines,
    myRegisters:     Registers.Registers,
    mySelections:    Selections.Selections,
    mySelsModifier:  TextRangesModifier<Selection>,
    mySelsRegisters: SelectionsRegisters.SelectionsRegisters,
    myWantedColumns: Helpers.WantedColumns,
    myDispatcher:    TextAreaDispatcher.TextAreaDispatcher
) =
    inherit TextAreaDelegator (inBasicState)

    // private properties

    member private _.IsSingleSelection =
        mySelections.Count = 1

    // modifying commands

    member this.PerformModifyingCommand isNormalMode command count (isLinesModified: bool ref) =
        this.LoadBasicState ()

        let inputs, outputs =
            myDispatcher.ModifyingPerformer.GetWantedColumnsActions command

        // Repeatable command must not request any wanted columns inputs.
        if count > 1 && not (List.isEmpty inputs) then
            invalidOp ""

        let dpToken =
            myDispatcher.DisplayRenderer.GetDisplayPosToken
                (ModifyingCommand command) this.DisplayPos

        myWantedColumns.PerformInputWCActions myDispatcher.GetCharWantedColumns inputs

        let delegateFun = this.DelegateModifyingCommand isLinesModified dpToken
        this.SwitchToModifyingCommand isNormalMode command count delegateFun

        myWantedColumns.RegisterOutputWCActions outputs

        // Modify display position only if it's not at the start of file.
        if not (this.DisplayPos.line = 0 && this.DisplayPos.lineRow = 0) then
            this.DisplayPos <-
                myDispatcher.DisplayRenderer.GetModifiedDisplayPos dpToken

        this.StoreBasicState ()

    member private this.SwitchToModifyingCommand isNormalMode command count delegateFun =
        match command with
        | AlignSelections ->
            // DisallowLinesCache is called in PerformAlignSelection.
            ()
        | _ ->
            myDispatcher.DisplayRenderer.DisallowLinesCache ()

        myDispatcher.ModifyingPerformer.PrepareRegister command

        for _ = 1 to count do
            match command with
            | EnterAppending      -> this.PerformEnterAppendingCommand delegateFun

            | InsertChar        _
            | InsertChars       _
            | InsertNewLine
            | InsertNewLineIndent -> this.PerformInsertCommand         delegateFun command

            | Yank              r -> this.PerformYankCommand           delegateFun r
            | Delete              -> this.PerformDeleteCommand         delegateFun
            | YankAndDelete     r -> this.PerformYankAndDeleteCommand  delegateFun r
            | PasteBefore       r -> this.PerformPasteBeforeCommand    delegateFun r isNormalMode
            | PasteAfter        r -> this.PerformPasteAfterCommand     delegateFun r
            | Replace           r -> this.PerformReplaceCommand        delegateFun r

            | AlignSelections     -> this.PerformAlignSelection        delegateFun

            | PasteBeforeAux    _
            | AlignSelectionAux _ -> invalidOp ""

            | _                   -> this.PerformSimpleCommand         delegateFun command

            this.PrevCommand <- Some (ModifyingCommand command)

        myDispatcher.ModifyingPerformer.PublishRegister command

        match command with
        | AlignSelections ->
            // ResetLinesCache is called in PerformAlignSelection.
            ()
        | _ ->
            myDispatcher.DisplayRenderer.ResetLinesCache ()

    member private this.PerformSimpleCommand delegateFun command =
        this.StartApplyingModifications ()

        for i = 0 to mySelections.Count - 1 do
            delegateFun (ref false) command i

        this.StopApplyingModifications ()

    member private _.PerformEnterAppendingCommand delegateFun =
        for i = 0 to mySelections.Count - 1 do
            delegateFun (ref false) EnterAppending i

    member private this.PerformInsertCommand delegateFun command =
        this.StartApplyingModifications ()

        for i = 0 to mySelections.Count - 1 do
            delegateFun (ref false) command i

        this.StopApplyingModifications ()

        // Reposition all cursors and anchors.
        let command' = CursorAfterSelection false
        for i = 0 to mySelections.Count - 1 do
            this.DelegateCommonCommand command' i

    member private _.PerformYankCommand delegateFun register =
        for i = 0 to mySelections.Count - 1 do
            delegateFun (ref false) (Yank register) i

    member private this.PerformDeleteCommand delegateFun =
        this.StartApplyingModifications ()

        for i = 0 to mySelections.Count - 1 do
            if not (mySelsModifier.IsEliminated i) then
                delegateFun (ref false) Delete i

        this.StopApplyingModifications ()

    member private this.PerformYankAndDeleteCommand delegateFun register =
        this.PerformYankCommand delegateFun register

        this.PerformDeleteCommand delegateFun

    member private this.PerformPasteBeforeCommand delegateFun register isNormalMode =
        let isPossibleEolAfterEof = ref false

        this.PerformPasteBeforeCommandAux delegateFun isPossibleEolAfterEof register

        if not isNormalMode then
            // Reposition all cursors and anchors corresponding to non-empty slots.
            let command = CursorAfterSelection true
            for i = 0 to mySelections.Count - 1 do
                let lines = myRegisters.GetSlot register i
                if lines <> None then
                    this.DelegateCommonCommand command i

        if isPossibleEolAfterEof.Value then
            this.AssureSelectionsBeforeEof ()

    member private this.PerformPasteAfterCommand delegateFun register =
        let isPossibleEolAfterEof = ref false

        // Reposition all cursors and anchors, don't add new line at EOF.
        let command = CursorAfterSelection true
        for i = 0 to mySelections.Count - 1 do
            this.DelegateCommonCommand command i

        // Selections might have been reordered by the repositioning.
        mySelections.Sort ()

        this.PerformPasteBeforeCommandAux delegateFun isPossibleEolAfterEof register

        // Reposition all cursors and anchors corresponding to empty slots.
        let command = CursorLeft
        for i = 0 to mySelections.Count - 1 do
            let lines = myRegisters.GetSlot register i
            if lines = None then
                this.DelegateCommonCommand command i

        if isPossibleEolAfterEof.Value then
            this.AssureSelectionsBeforeEof ()

    member private this.PerformReplaceCommand delegateFun register =
        let isPossibleEolAfterEof = ref false

        this.PerformDeleteCommand delegateFun

        this.PerformPasteBeforeCommandAux delegateFun isPossibleEolAfterEof register

        if isPossibleEolAfterEof.Value then
            this.AssureSelectionsBeforeEof ()

    member private this.PerformPasteBeforeCommandAux delegateFun isPossibleEolAfterEof register =
        this.StartApplyingModifications ()

        for i = 0 to mySelections.Count - 1 do
            delegateFun isPossibleEolAfterEof (PasteBeforeAux (register, false)) i

        this.StopApplyingModifications ()

    member private this.PerformAlignSelection delegateFun =
        let alignColumn, selections = this.GetAlignmentColumns ()

        // DisallowLinesCache is not called in PerformModifyingCommand.
        myDispatcher.DisplayRenderer.DisallowLinesCache ()

        this.StartApplyingModifications ()

        for i, firstColumn in selections do
            let delta = alignColumn - firstColumn
            delegateFun (ref false) (AlignSelectionAux delta) i

        this.StopApplyingModifications ()

        // ResetLinesCache is not called in PerformModifyingCommand.
        myDispatcher.DisplayRenderer.ResetLinesCache ()

    // delagating

    // simplified version of TextAreaBasic.DelegateBasicCommand
    member private this.DelegateCommonCommand command selectionIndex =
        let state = {
            selection         = mySelections[selectionIndex]
            displayLine       = this.DisplayPos.line
            displayLineRow    = this.DisplayPos.lineRow
            displayColumn     = this.DisplayPos.column
            isSingleSelection = this.IsSingleSelection
            prevCommand       = this.PrevCommand
        }

        let outState =
            myDispatcher.CommonPerformer.PerformCommand state command

        if outState.toUpdateSelection then
            let position = { line = outState.line; char = outState.char }
            mySelections[selectionIndex] <-
                mySelections[selectionIndex].WithCursorAndAnchorEqual position

        this.DisplayPos <- {
            line    = outState.displayLine
            lineRow = outState.displayLineRow
            column  = outState.displayColumn
        }

    member private _.DelegateModifyingCommand
        isLinesModified displayPosToken isPossibleEolAfterEof command selectionIndex =

        mySelsModifier.AssureItemsPreparedUpTo selectionIndex

        let state: ModifyingCommandInState = {
            selectionIndex  = selectionIndex
            isLastSelection = (selectionIndex = mySelections.Count - 1)
            selection       = mySelections[selectionIndex]
        }

        let outState, outCommands =
            myDispatcher.ModifyingPerformer.PerformCommand state command

        for outCommand in outCommands do
            match outCommand with
            | ApplyInsert        spec -> mySelsModifier .ApplyInsert        spec
                                         mySelsRegisters.ApplyInsert        spec
            | ApplyDelete        spec -> mySelsModifier .ApplyDelete        spec
                                         mySelsRegisters.ApplyDelete        spec
            | ApplyOneLineInsert spec -> mySelsModifier .ApplyOneLineInsert spec
                                         mySelsRegisters.ApplyOneLineInsert spec
            | ApplyOneLineDelete spec -> mySelsModifier .ApplyOneLineDelete spec
                                         mySelsRegisters.ApplyOneLineDelete spec

            displayPosToken.Modify outCommand

        if outState.toUpdateSelection then
            mySelections[selectionIndex] <- outState.selection

        if outState.isLinesModified then
            isLinesModified.Value <- true
        if outState.isPossibleEolAfterEof then
            isPossibleEolAfterEof.Value <- true

    // auxiliary

    member private _.StartApplyingModifications () =
        mySelsModifier.StartApplyingModifications ()
        mySelsRegisters.StartApplyingModifications ()

    member private _.StopApplyingModifications () =
        mySelsModifier.StopApplyingModifications ()
        mySelsRegisters.StopApplyingModifications ()

    member private _.AssureSelectionsBeforeEof () =
        let line = myLines.Count - 1
        let char = myLines[line].Length

        let lastEolPosition = { line = line; char = char }

        // Add EOL after EOF if any selection would be invalid without it.
        if mySelections.IsAnyAfterEof lastEolPosition then
            myDispatcher.ModifyingPerformer.AddEolAfterEof ()

        // Modify all selections which are not positioned before EOF.
        mySelsRegisters.AssureAllBeforeEof lastEolPosition

    member private _.GetAlignmentColumns () =
        let mutable alignColumn = 0
        let mutable lastLine = -1

        let selections = ResizeArray<int * int> ()

        for i = 0 to mySelections.Count - 1 do
            let first = mySelections[i].first
            let line  = first.line
            let char_ = first.char

            // The first selection on the line ?
            if line <> lastLine then
                lastLine <- line

                let firstColumn, _lastColumn =
                    myDispatcher.DisplayRenderer.GetCharColumns line char_
                selections.Add (i, firstColumn)

                alignColumn <- max alignColumn firstColumn

        (alignColumn, selections)
