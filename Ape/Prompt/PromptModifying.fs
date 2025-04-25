namespace PromptDelegators

open Commands.InCommands
open Commands.OutCommands
open Common
open DataTypes
open Position
open Selection
open TextRangesModifier

open ModifyingPerformer

type PromptModifying (
    inBasicState:   BasicState,
    myLines:        Lines,
    myRegisters:    Registers.Registers,
    mySelections:   Selections.Selections,
    mySelsModifier: TextRangesModifier<Selection>,
    myDispatcher:   PromptDispatcher.PromptDispatcher
) =
    inherit PromptDelegator (inBasicState)

    // private properties

    member private _.SelectionEndsAtEol =
        mySelections[0].last.char = myLines[0].Length

    // modifying commands

    member this.PerformModifyingCommand isNormalMode command (isLinesModified: bool ref) =
        this.LoadBasicState ()

        let dpToken =
            myDispatcher.DisplayRenderer.GetDisplayPosToken
                (ModifyingCommand command) this.DisplayPos

        let delegateFun = this.DelegateModifyingCommand isLinesModified dpToken
        this.SwitchToModifyingCommand isNormalMode command delegateFun

        // Modify display position only if it's not at the start of file.
        if not (this.DisplayPos.line = 0 && this.DisplayPos.lineRow = 0) then
            this.DisplayPos <-
                myDispatcher.DisplayRenderer.GetModifiedDisplayPos dpToken

        this.StoreBasicState ()

    member private this.SwitchToModifyingCommand isNormalMode command delegateFun =
        myDispatcher.DisplayRenderer.DisallowLinesCache ()

        myDispatcher.ModifyingPerformer.PrepareRegister command

        match command with
        | EnterAppending      -> this.PerformEnterAppendingCommand delegateFun

        | InsertChar        _
        | InsertChars       _ -> this.PerformInsertCommand         delegateFun command

        | Yank              r -> this.PerformYankCommand           delegateFun r
        | Delete              -> this.PerformDeleteCommand         delegateFun
        | YankAndDelete     r -> this.PerformYankAndDeleteCommand  delegateFun r
        | PasteBefore       r -> this.PerformPasteBeforeCommand    delegateFun r isNormalMode
        | PasteAfter        r -> this.PerformPasteAfterCommand     delegateFun r
        | Replace           r -> this.PerformReplaceCommand        delegateFun r

        | InsertNewLine
        | InsertNewLineIndent
        | AlignSelections

        | PasteBeforeAux    _
        | AlignSelectionAux _ -> invalidOp ""

        | _                   -> this.PerformSimpleCommand delegateFun command

        myDispatcher.ModifyingPerformer.PublishRegister command

        myDispatcher.DisplayRenderer.ResetLinesCache ()

    member private this.PerformSimpleCommand delegateFun command =
        this.StartApplyingModifications ()
        delegateFun command
        this.StopApplyingModifications ()

    member private this.PerformEnterAppendingCommand delegateFun =
        if not this.SelectionEndsAtEol then
            delegateFun EnterAppending
        else
            // Reposition cursor and anchor to the end of line.
            this.DelegateCommonCommand CursorAtEol

    member private this.PerformInsertCommand delegateFun command =
        this.StartApplyingModifications ()
        delegateFun command
        this.StopApplyingModifications ()

        // Reposition cursor and anchor.
        this.DelegateCommonCommand (CursorAfterSelection false)

    member private _.PerformYankCommand delegateFun register =
        delegateFun (Yank register)

    member private this.PerformDeleteCommand delegateFun =
        this.StartApplyingModifications ()
        delegateFun Delete
        this.StopApplyingModifications ()

    member private this.PerformYankAndDeleteCommand delegateFun register =
        this.PerformYankCommand delegateFun register

        this.PerformDeleteCommand delegateFun

    member private this.PerformPasteBeforeCommand delegateFun register isNormalMode =
        let lines = myRegisters.GetSlot register 0

        if Registers.isPastableToPrompt lines then
            this.PerformPasteBeforeCommandAux delegateFun register

            if not isNormalMode then
                // Reposition cursor and anchor corresponding to non-empty slot.
                if lines <> None then
                    this.DelegateCommonCommand (CursorAfterSelection false)

    member private this.PerformPasteAfterCommand delegateFun register =
        let lines = myRegisters.GetSlot register 0

        if Registers.isPastableToPrompt lines then
            if not this.SelectionEndsAtEol then
                // Reposition cursor and anchor, don't add new line at EOF.
                this.DelegateCommonCommand (CursorAfterSelection true)

                this.PerformPasteBeforeCommandAux delegateFun register

                // Reposition cursor and anchor corresponding to empty slot.
                if lines = None then
                    this.DelegateCommonCommand CursorLeft
            else
                // Reposition cursor and anchor to the end of line.
                this.DelegateCommonCommand CursorAtEol

                this.PerformPasteBeforeCommandAux delegateFun register

    member private this.PerformReplaceCommand delegateFun register =
        let lines = myRegisters.GetSlot register 0

        if Registers.isPastableToPrompt lines then
            this.PerformDeleteCommand delegateFun

            this.PerformPasteBeforeCommandAux delegateFun register

    member private this.PerformPasteBeforeCommandAux delegateFun register =
        this.StartApplyingModifications ()
        delegateFun (PasteBeforeAux (register, true))
        this.StopApplyingModifications ()

    // delagating

    // simplified version of PromptBasic.DelegateBasicCommand
    member private this.DelegateCommonCommand command =
        let state = {
            selection         = mySelections[0]
            displayLine       = this.DisplayPos.line
            displayLineRow    = this.DisplayPos.lineRow
            displayColumn     = this.DisplayPos.column
            isSingleSelection = true
            prevCommand       = Some (CommonCommand command)
        }

        let outState =
            myDispatcher.CommonPerformer.PerformCommand state command

        if outState.toUpdateSelection then
            let position = { line = outState.line; char = outState.char }
            mySelections[0] <-
                mySelections[0].WithCursorAndAnchorEqual position

        this.DisplayPos <- {
            line    = outState.displayLine
            lineRow = outState.displayLineRow
            column  = outState.displayColumn
        }

    member private _.DelegateModifyingCommand isLinesModified displayPosToken command =

        mySelsModifier.AssureItemsPreparedUpTo 0

        let state: ModifyingCommandInState = {
            selectionIndex  = 0
            isLastSelection = (0 = mySelections.Count - 1)
            selection       = mySelections[0]
        }

        let outState, outCommands =
            myDispatcher.ModifyingPerformer.PerformCommand state command

        for outCommand in outCommands do
            match outCommand with
            | ApplyInsert        spec -> mySelsModifier.ApplyInsert        spec
            | ApplyDelete        spec -> mySelsModifier.ApplyDelete        spec
            | ApplyOneLineInsert spec -> mySelsModifier.ApplyOneLineInsert spec
            | ApplyOneLineDelete spec -> mySelsModifier.ApplyOneLineDelete spec

            displayPosToken.Modify outCommand

        if outState.toUpdateSelection then
            mySelections[0] <- outState.selection

        if outState.isLinesModified then
            isLinesModified.Value <- true

    // auxiliary

    member private _.StartApplyingModifications () =
        mySelsModifier.StartApplyingModifications ()

    member private _.StopApplyingModifications () =
        mySelsModifier.StopApplyingModifications ()
