module TextAreaRenderer

open DataTypes
open PositionClassifier

type TextAreaRenderer (
    myLines:       Lines,
    mySelections:  Selections.Selections,
    myMatchRanges: MatchRanges.MatchRanges,
    myDispatcher:  TextAreaDispatcher.TextAreaDispatcher
) =
    member private _.MainLine = mySelections.Main.Cursor.line
    member private _.MainChar = mySelections.Main.Cursor.char

    member _.GetDisplayRows displayPos =
        let endLine =
            myDispatcher.DisplayRenderer.GetLastDisplayedLine displayPos
        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors displayPos.line endLine
        let matchRanges =
            myMatchRanges.GetInIntervalFromAllGroups displayPos.line endLine

        let classifier = PositionClassifier (
            mainCursor, cursors, selectionRanges, matchRanges
        )

        myDispatcher.DisplayRenderer.GetDisplayRows displayPos classifier

    member this.GetCursorPosForStatusArea () =
        let firstColumn, lastColumn =
            myDispatcher.DisplayRenderer.GetCharColumns this.MainLine this.MainChar

        let columnsCount =
            let chars = myLines[this.MainLine]
            let char_ = this.MainChar
            if char_ < chars.Length && chars[char_] = Utils.charTab then
                lastColumn - firstColumn + 1
            else
                0

        {
            selectionsCount = mySelections.Count
            line            = this.MainLine
            char_           = this.MainChar
            firstColumn     = firstColumn
            columnsCount    = columnsCount
        }
