namespace TextAreaDelegator

open Commands.InCommands
open Common
open Position

type TextAreaBasic (
    inBasicState:    BasicState,
    mySelections:    Selections.Selections,
    myWantedColumns: Helpers.WantedColumns,
    myMatchRanges:   IMatchRanges.IMatchRanges,
    myDispatcher:    TextAreaDispatcher.TextAreaDispatcher
) =
    inherit TextAreaDelegator (inBasicState)

    // private properties

    member private _.IsSingleSelection =
        mySelections.Count = 1

    // basic commands

    member this.PerformOnAllSelections isExtending command count =
        this.LoadBasicState ()

        this.PerformOnSelections
            true (seq { 0 .. mySelections.Count - 1 })
            isExtending command count

        this.StoreBasicState ()

    member this.PerformOnMainSelection isExtending command count =
        this.LoadBasicState ()

        this.PerformOnSelections
            true (seq { mySelections.MainIndex })
            isExtending command count

        this.StoreBasicState ()

    member this.PerformViewCommand isExtending command count =
        this.LoadBasicState ()

        this.PerformOnSelections
            false (seq { mySelections.MainIndex })
            isExtending command count

        this.StoreBasicState ()

    member this.PerformMatchCommand isExtending command count isForward =
        this.LoadBasicState ()

        let oldMainCursor = mySelections.Main.Cursor

        this.PerformOnSelections
            true (seq { 0 .. mySelections.Count - 1 })
            isExtending command count

        this.CenterAfterMatch isForward oldMainCursor

        this.StoreBasicState ()

    member private this.PerformOnSelections
        toSetPrevCommand selectionIndexes isExtending command count =

        let inputs, outputs = myDispatcher.GetWantedColumnsActions command this.IsSingleSelection

        for _ = 1 to count do
            myWantedColumns.PerformInputWCActions myDispatcher.GetCharWantedColumns inputs

            for i in selectionIndexes do
                this.DelegateBasicCommand isExtending command i

            myWantedColumns.RegisterOutputWCActions outputs

            if toSetPrevCommand then
                this.PrevCommand <- Some command

    member private this.CenterAfterMatch isForward oldMainCursor =
        let newMainCursor = mySelections.Main.Cursor

        if myMatchRanges.GetMainGroupCount () <> 0 then
            let hitFileBoundary =
                if isForward then
                    // Hit bottom of the file.
                    TextAreaBasic.IsLower   newMainCursor oldMainCursor
                else
                    // Hit top of the file.
                    TextAreaBasic.IsGreater newMainCursor oldMainCursor

            let hitLineBoundary =
                // Moved to another line or stayed at the same one, but hit file boundary.
                newMainCursor.line <> oldMainCursor.line || hitFileBoundary

            let command = WrapLinesDepCommand (
                CenterAfterMatch (isForward, hitFileBoundary, hitLineBoundary)
            )
            this.DelegateBasicCommand false command mySelections.MainIndex

    // delegating

    // a simplified version of this method exists in TextAreaModifying class
    member private this.DelegateBasicCommand isExtending command selectionIndex =
        let state = {
            selection         = mySelections[selectionIndex]
            displayLine       = this.DisplayPos.line
            displayLineRow    = this.DisplayPos.lineRow
            displayColumn     = this.DisplayPos.column
            isSingleSelection = this.IsSingleSelection
            prevCommand       = this.PrevCommand
        }

        let outState = myDispatcher.PerformCommand state command

        if outState.toUpdateSelection then
            let position = { line = outState.line; char = outState.char }
            mySelections[selectionIndex] <-
                if not isExtending then
                    mySelections[selectionIndex].WithCursorAndAnchorEqual position
                else
                    mySelections[selectionIndex].WithCursor position

        this.DisplayPos <- {
            line    = outState.displayLine
            lineRow = outState.displayLineRow
            column  = outState.displayColumn
        }

    // auxiliary

    static member private IsLower (pos1: Position) (pos2: Position) =
        pos1.line < pos2.line || (pos1.line = pos2.line && pos1.char < pos2.char)

    static member private IsGreater (pos1: Position) (pos2: Position) =
        pos1.line > pos2.line || (pos1.line = pos2.line && pos1.char > pos2.char)
