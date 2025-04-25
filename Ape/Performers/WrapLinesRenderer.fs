module WrapLinesRenderer

open System
open System.Collections.Generic

open BinarySearch
open CharCategories
open Commands.InCommands
open Commands.OutCommands
open Common
open Context
open DataTypes
open Position
open PositionClassifier
open Selection
open TextRange
open TextRangesModifier
open WrappedRef

open WrapLinesBuilder

let private compareCharFieldTo char_ a =
    compareTo char_ a.char

let private compareRowFieldTo char_ a =
    compareTo char_ a.row

let private linesCacheMaxCount = 4
let private linesCacheMaxDelta = 2

/// Returns records made by iterating through chars.
/// Reaching line end is indicated by cOpt = EOL and NoChar, but char, lineColumn, row and column
/// have still valid/usable values.
let private getLineCharsArrayFromChars (context: AreaContext) chars =
    let lineChars = seq {
        let textWidth  = context.textWidth
        let wrapAtWord = context.wrapAtWord

        let b = WrapLinesBuilder textWidth

        let isLineBreakNeeded char_ =
            if char_ > 0 && isAtWordStartSingleLine chars char_ then
                let wordLength, adjChars = getWordLength chars char_
                b.Column + wordLength + adjChars > textWidth
            else
                false

        for c in chars do
            let rowEndCount, c', count =
                if c = Utils.charTab then
                    0, c, getTabSpaces context.tabStop b.LineColumn
                elif wrapAtWord && isLineBreakNeeded b.Char then
                    textWidth - b.Column, c, 1
                elif Utils.canBeDisplayed c then
                    0, c, 1
                else
                    0, '\uFFFD', 1

            if rowEndCount <> 0 then
                yield! b.MakeFillArray NoChar rowEndCount
                b.NextRow ()

            yield! b.GetNextItems (Char c') count
            b.NextChar ()

        yield! b.GetNextItems EOL 1

        while b.Column < textWidth do
            yield! b.GetNextItems NoChar 1
    }

    lineChars |> Seq.toArray

/// Returns lineChars splitted to separate rows.
/// Reaching line end is indicated by cOpt = EOL and NoChar, but char, lineColumn, row and column
/// have still valid/usable values.
let private splitLineCharsToRows (context: AreaContext) lineChars =
    seq {
        let mutable row = null

        for x in lineChars do
            if x.column = 0 then
                if row <> null then
                    yield row
                row <- ResizeArray context.textWidth
            row.Add x

        if row <> null then
            yield row
    }

let private getLineRowsAfterEOF context =
    let chars = charToChars charAfterEOF
    let lineChars = getLineCharsArrayFromChars context chars
    splitLineCharsToRows context lineChars

type private DisplayPosToken (inTop: ITextRange) =
    member val internal Top = inTop with get, set

    interface IDisplayPosToken with
        // Modifies "top" selection.
        member this.Modify outCommand =
            this.Top <-
                match outCommand with
                | ApplyInsert spec ->
                    this.Top.ApplyInsert spec
                | ApplyDelete spec ->
                    let _isEliminated, textRange = this.Top.ApplyDelete spec
                    textRange
                | ApplyOneLineInsert spec ->
                    this.Top.ApplyOneLineInsert spec
                | ApplyOneLineDelete spec ->
                    let _isEliminated, textRange = this.Top.ApplyOneLineDelete spec
                    textRange

type WrapLinesRenderer (
    myContextRef: IWrappedRef<AreaContext>,
    myLines:      Lines
) as this_ =
    let mutable myContext = myContextRef.Value
    let mutable myLineRowsAfterEOF = getLineRowsAfterEOF myContext

    let handleContextChanged () =
        myContext <- myContextRef.Value
        myLineRowsAfterEOF <- getLineRowsAfterEOF myContext
        (this_ :> IDisplayRenderer).ResetLinesCache ()

    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    // lines cache used for rendering
    let mutable myLinesCache = Dictionary<int, LineChar array> ()

    member internal _.LastLine =
        myLines.Count - 1

    // rendering

    interface IDisplayRenderer with
        member this.GetCharColumns line char_ =
            let first, last = this.GetCharLineChars line char_
            (first.lineColumn, last.lineColumn)

        member this.GetCharWantedColumns line char_ =
            let first = this.GetCharFirstLineChar line char_
            let hardWantedColumn = first.lineColumn
            let softWantedColumn = first.column
            (hardWantedColumn, softWantedColumn)

        member this.GetLastDisplayedLine displayPos =
            let h = myContext.areaHeight

            let mutable rowsCount = -displayPos.lineRow
            let mutable line      =  displayPos.line

            // Body of this statement is always performed at least once.
            while line < myLines.Count && rowsCount < h do
                let lineChars = this.GetLineCharsArray line
                let lineRowsCount = this.GetLastRow lineChars + 1
                rowsCount <- rowsCount + lineRowsCount
                line      <- line + 1

            line - 1

        member this.GetDisplayRows displayPos positionClassifier =
            let displayRows = ResizeArray myContext.areaHeight

            let mutable line = displayPos.line

            while displayRows.Count < myContext.areaHeight do
                let rows =
                    if line < myLines.Count then
                        let lineChars = this.GetLineCharsArray line
                        splitLineCharsToRows myContext lineChars
                    else
                        myLineRowsAfterEOF

                let maxRowsCount = myContext.areaHeight - displayRows.Count

                let rows, firstRow =
                    if line = displayPos.line then
                        (rows |> Seq.skip displayPos.lineRow, displayPos.lineRow)
                    else
                        (rows, 0)
                let rows =
                    rows |> Seq.truncate maxRowsCount

                displayRows.AddRange (
                    this.GetLineDisplayRows rows firstRow line positionClassifier
                )
                line <- line + 1

            displayRows

        member this.GetDisplayPosToken command displayPos =
            let line    = displayPos.line
            let lineRow = displayPos.lineRow

            let line, char_ =
                match command with
                | ModifyingCommand  ( EnterInserting      )
                | ModifyingCommand  ( EnterAppending      )
                | ModifyingCommand  ( EnterInsertingAtSol )
                | ModifyingCommand  ( EnterAppendingAtEol )
                | ModifyingCommand  ( InsertChar _        )
                | ModifyingCommand  ( InsertChars _       )
                | ModifyingCommand  ( InsertNewLine       )
                | ModifyingCommand  ( InsertNewLineIndent )
                | ModifyingCommand  ( Yank _              )
                | ModifyingCommand  ( PasteBefore _       )
                | ModifyingCommand  ( PasteAfter _        )
                | ModifyingCommand  ( PasteBeforeAux _    )
                | ModifyingCommand  ( Replace _           )
                | ModifyingCommand  ( AlignSelections     )
                | ModifyingCommand  ( AlignSelectionAux _ )
                | TextRangesCommand ( FillWithChar _      )
                | TextRangesCommand ( ToUppercase         )
                | TextRangesCommand ( InvertCase          )
                | TextRangesCommand ( TabsToSpaces        )
                | TextRangesCommand ( RegexEscape         )
                | TextRangesCommand ( Indent              ) -> (line, this.GetLineRowFirstChar line lineRow)
                | ModifyingCommand  ( DeleteChar          )
                | ModifyingCommand  ( DeletePrevChar      )
                | ModifyingCommand  ( DeletePrevChars _   )
                | ModifyingCommand  ( Delete              )
                | ModifyingCommand  ( YankAndDelete _     )
                | TextRangesCommand ( SpacesToTabs        )
                | TextRangesCommand ( Unindent            ) -> (line, this.GetLineRowLastChar  line lineRow)

                | CommonCommand       _
                | WrapLinesDepCommand _
                | SelectionsCommand   _
                | UndoRedoCommand     _ -> invalidOp ""

            let position = { Position.line = line; char = char_ }

            DisplayPosToken {
                first = position
                last  = position
            }

        // Returns line and lineRow derived from "top" selection.
        member this.GetModifiedDisplayPos (token: IDisplayPosToken) =
            let token = token :?> DisplayPosToken

            let line  = token.Top.First.line
            let char_ = token.Top.First.char

            // Some commands may move the token beyond the end of file.
            let line  = min (myLines.Count - 1) line
            let char_ = min (myLines[line].Length) char_

            let lineRow = this.GetCharFirstRow line char_

            { line = line; lineRow = lineRow; column = 0 }

        member this.GetCopiedSelectionUp selection =
            let line = selection.first.line

            if line > 0 then
                Some (this.GetCopiedSelection (line - 1) selection)
            else
                None

        member this.GetCopiedSelectionDown selection =
            let line = selection.last.line

            if line < myLines.Count - 1 then
                Some (this.GetCopiedSelection (line + 1) selection)
            else
                None

        member _.DisallowLinesCache () =
            myLinesCache <- null

        member _.ResetLinesCache () =
            myLinesCache <- Dictionary<int, LineChar array> ()

        member this.TrimLinesCacheIfNeeded firstLine lastLine =
            let maxCount = linesCacheMaxCount * myContext.areaHeight
            if myLinesCache.Count > maxCount then
                this.TrimLinesCache firstLine lastLine

    /// Returns display rows for given rows. line must be line
    /// index of rows and it's used for highlighting purposes.
    member private _.GetLineDisplayRows
        rows firstRow line (positionClassifier: PositionClassifier) =

        let displayRows = ResizeArray ()

        let normalColors     = getNormalColors myContext myLines.Count line
        let lineNumberColors = myContext.colorScheme.lineNumber
        let selectionColors  = myContext.colorScheme.selection
        let matchesColors    = myContext.colorScheme.matches

        let totalWidth = myContext.lineNumbersWidth + myContext.textWidth

        for i, row in rows |> Seq.indexed do
            let displayRow = ResizeArray totalWidth

            let lineNumberString =
                if i = 0 && firstRow = 0 && line < myLines.Count then
                    getLineNumberString myContext line
                else
                    getLineNumberFillString myContext

            for c in lineNumberString do
                displayRow.Add { c = c; colors = lineNumberColors }

            for x in row do
                let cOpt, char_ = x.cOpt, x.char
                let c = getCharToDisplay cOpt

                let colors =
                    if cOpt <> NoChar then
                        let positionClass = positionClassifier.Classify line char_
                        match positionClass with
                        | PositionMainCursor    -> getMainCursorColors    myContext cOpt
                        | PositionNonMainCursor -> getNonMainCursorColors myContext cOpt
                        | PositionSelection     -> selectionColors
                        | PositionMatch i       -> matchesColors[i]
                        | _                     -> normalColors
                    else
                        normalColors

                displayRow.Add { c = c; colors = colors }

            displayRows.Add displayRow

        displayRows

    // lines cache used for rendering

    member internal _.GetLineCharsArray line =
        if myLinesCache.ContainsKey line then
            myLinesCache[line]
        else
            let lineChars = getLineCharsArrayFromChars myContext myLines[line]
            myLinesCache[line] <- lineChars
            lineChars

    member private _.TrimLinesCache firstLine lastLine =
        let maxDelta = linesCacheMaxDelta * myContext.areaHeight

        let keys =
            myLinesCache.Keys
            |> Seq.filter (fun line ->
                   (line < lastLine) && not (firstLine - line < maxDelta) ||
                   (line > firstLine) && not (line - lastLine < maxDelta)
               )
            |> Seq.toArray

        for key in keys do
            myLinesCache.Remove key |> ignore

    // auxiliary

    member private this.GetCharFirstLineChar line char_ =
        let lineChars = this.GetLineCharsArray line
        let a = lineChars |> getFirstEqualInSortedArray (compareCharFieldTo char_)
        a

    member private this.GetCharLineChars line char_ =
        let lineChars = this.GetLineCharsArray line
        let a = lineChars |> getFirstEqualInSortedArray (compareCharFieldTo char_)
        let b = lineChars |> getLastEqualInSortedArray  (compareCharFieldTo char_)
        (a, b)

    member internal this.GetCharFirstRow line char_ =
        let first = this.GetCharFirstLineChar line char_
        first.row

    member internal this.GetCharRows line char_ =
        let first, last = this.GetCharLineChars line char_
        (first.row, last.row)

    member private this.GetLineRowFirstChar line lineRow =
        let lineChars = this.GetLineCharsArray line
        let a = lineChars |> getFirstEqualInSortedArray (compareRowFieldTo lineRow)
        a.char

    member private this.GetLineRowLastChar line lineRow =
        let lineChars = this.GetLineCharsArray line
        let a = lineChars |> getLastEqualInSortedArray  (compareRowFieldTo lineRow)
        a.char

    member internal _.GetLastRow lineChars =
        let a = lineChars |> Array.last
        a.row

    member private this.GetCopiedSelection line selection =
        let first =
            this.GetHardWantedPosition line selection.firstWC.hard
        let last =
            this.GetHardWantedPosition line selection.lastWC.hard

        { selection with first = first; last = last }

    member private this.GetHardWantedPosition line hardWantedColumn =
        let lineChars = this.GetLineCharsArray line
        let a = this.GetHardWantedLineChar lineChars hardWantedColumn

        if myContext.cursorBeforeEol then
            let b = this.AssureHardPositionBeforeEol lineChars a
            { line = line; char = b.char }
        else
            { line = line; char = a.char }

    member internal _.GetHardWantedLineChar lineChars hardWantedColumn =
        lineChars |> Array.find (
            fun x -> x.cOpt = EOL || x.lineColumn = hardWantedColumn
        )

    member internal _.GetSoftWantedLineChar lineChars row softWantedColumn =
        lineChars |> Array.find (
            fun x -> x.row = row && (x.cOpt = EOL || x.column = softWantedColumn)
        )

    member internal _.AssureHardPositionBeforeEol lineChars a =
        if a.cOpt = EOL then
            lineChars |> Array.find (
                fun x -> x.cOpt = EOL || x.lineColumn = a.lineColumn - 1
            )
        else
            a

    member internal _.AssureSoftPositionBeforeEol lineChars a =
        if a.cOpt = EOL then
            lineChars |> Array.find (
                fun x -> x.row = a.row && (x.cOpt = EOL || x.column = a.column - 1)
            )
        else
            a

    // IDisposable

    abstract Dispose: unit -> unit

    default _.Dispose () =
        myContextChangedDisposable.Dispose ()

    interface IDisposable with
        member this.Dispose () =
            this.Dispose ()
