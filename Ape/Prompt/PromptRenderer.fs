module PromptRenderer

open PositionClassifier
open TextRanges

type PromptRenderer (
    mySelections: Selections.Selections,
    myDispatcher: PromptDispatcher.PromptDispatcher
) =
    member _.GetDisplayRows displayPos =
        let endLine =
            myDispatcher.DisplayRenderer.GetLastDisplayedLine displayPos
        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors displayPos.line endLine
        let matchRanges =
            [| 0, TextRanges () |]

        let classifier = PositionClassifier (
            mainCursor, cursors, selectionRanges, matchRanges
        )

        myDispatcher.DisplayRenderer.GetDisplayRows displayPos classifier
