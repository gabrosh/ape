module NoWrapLinesRenderer

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

open NoWrapLinesBuilder

let private compareCharFieldTo char_ a =
    compareTo char_ a.char

let private linesCacheMaxCount = 4
let private linesCacheMaxDelta = 2

/// Returns records made by iterating through chars.
/// Reaching line end is indicated by cOpt = EOL, but char and column
/// have still valid/usable values.
let getLineCharsArrayFromChars (context: AreaContext) chars =
    let lineChars = seq {
        let b = NoWrapLinesBuilder ()

        for c in chars do
            let c', count =
                if c = Utils.charTab then
                    c, getTabSpaces context.tabStop b.Column
                elif Utils.canBeDisplayed c then
                    c, 1
                else
                    '\uFFFD', 1

            yield! b.GetNextItems (Char c') count
            b.NextChar ()

        yield! b.GetNextItems EOL 1
    }

    lineChars |> Seq.toArray

let private getLineCharsAfterEOF context =
    let chars = [charAfterEOF]
    getLineCharsArrayFromChars context chars

type private DisplayPosToken (inTop: ITextRange, inColumn: int) =
    member val internal Top    = inTop    with get, set
    member val internal Column = inColumn with get

    interface IDisplayPosToken with
        // Modifies "top" selection, keeps column unmodified.
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

type NoWrapLinesRenderer (
    myContextRef: IWrappedRef<AreaContext>,
    myLines:      Lines
) as this_ =
    let mutable myContext = myContextRef.Value
    let mutable myLineCharsAfterEOF = getLineCharsAfterEOF myContext

    let handleContextChanged () =
        myContext <- myContextRef.Value
        myLineCharsAfterEOF <- getLineCharsAfterEOF myContext
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
            let firstColumn, lastColumn = this.GetCharColumns line char_
            (firstColumn, lastColumn)

        member this.GetCharWantedColumns line char_ =
            let firstColumn = this.GetCharFirstColumn line char_
            (firstColumn, firstColumn)

        member this.GetLastDisplayedLine displayPos =
            min (displayPos.line + (myContext.areaHeight - 1))
                this.LastLine

        member this.GetDisplayRows displayPos positionClassifier =
            let displayRows = ResizeArray myContext.areaHeight

            let mutable line = displayPos.line

            while displayRows.Count < myContext.areaHeight do
                let lineChars, displayColumn =
                    if line < myLines.Count then
                        this.GetLineCharsArray line, displayPos.column
                    else
                        myLineCharsAfterEOF, 0

                displayRows.Add (
                    this.GetLineDisplayRow lineChars line displayColumn positionClassifier
                )
                line <- line + 1

            displayRows

        member this.GetDisplayPosToken command displayPos =
            let line = displayPos.line

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
                | TextRangesCommand ( Indent              ) -> (line, this.GetLineFirstChar line)
                | ModifyingCommand  ( DeleteChar          )
                | ModifyingCommand  ( DeletePrevChar      )
                | ModifyingCommand  ( DeletePrevChars _   )
                | ModifyingCommand  ( Delete              )
                | ModifyingCommand  ( YankAndDelete _     )
                | TextRangesCommand ( SpacesToTabs        )
                | TextRangesCommand ( Unindent            ) -> (line, this.GetLineLastChar  line)

                | CommonCommand       _
                | WrapLinesDepCommand _
                | SelectionsCommand   _
                | UndoRedoCommand     _ -> invalidOp ""

            let position = { Position.line = line; char = char_ }

            DisplayPosToken (
                {
                    first = position
                    last  = position
                },
                displayPos.column
            )

        // Returns line derived from "top" selection and unmodified column.
        member _.GetModifiedDisplayPos (token: IDisplayPosToken) =
            let token = token :?> DisplayPosToken

            let line   = token.Top.First.line
            let column = token.Column

            // Some commands may move the token beyond the end of file.
            let line = min (myLines.Count - 1) line

            { line = line; lineRow = 0; column = column }

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

    /// Returns display row for given lineChars, starting at displayColumn. line
    /// must be line index of lineChars and it's used for highlighting purposes.
    member private _.GetLineDisplayRow
        lineChars line displayColumn (positionClassifier: PositionClassifier) =

        let normalColors     = getNormalColors myContext myLines.Count line
        let lineNumberColors = myContext.colorScheme.lineNumber
        let selectionColors  = myContext.colorScheme.selection
        let matchesColors    = myContext.colorScheme.matches

        let displayEndColumn = displayColumn + myContext.textWidth

        let totalWidth = myContext.lineNumbersWidth + myContext.textWidth

        let displayRow = ResizeArray totalWidth

        let lineNumberString =
            if line < myLines.Count then
                getLineNumberString myContext line
            else
                getLineNumberFillString myContext

        for c in lineNumberString do
            displayRow.Add { c = c; colors = lineNumberColors }

        let row =
            lineChars
            |> Seq.skipWhile (fun x -> x.column < displayColumn)
            |> Seq.takeWhile (fun x -> x.column < displayEndColumn)
            |> Seq.toArray

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

        let fillLength = totalWidth - displayRow.Count
        let fillChar = { c = charAfterEOL; colors = normalColors }
        for _ = 1 to fillLength do
            displayRow.Add fillChar

        displayRow

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

    member internal this.GetCharFirstColumn line char_ =
        let first = this.GetCharFirstLineChar line char_
        first.column

    member internal this.GetCharColumns line char_ =
        let first, last = this.GetCharLineChars line char_
        (first.column, last.column)

    member private this.GetLineFirstChar line =
        let lineChars = this.GetLineCharsArray line
        let a = lineChars |> Array.head
        a.char

    member private this.GetLineLastChar line =
        let lineChars = this.GetLineCharsArray line
        let a = lineChars |> Array.last
        a.char

    member internal _.GetLastColumn lineChars =
        let a = lineChars |> Array.last
        a.column

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
            let b = this.AssurePositionBeforeEol myLines[line] a.char
            { line = line; char = b }
        else
            { line = line; char = a.char }

    member internal _.GetHardWantedLineChar lineChars hardWantedColumn =
        lineChars |> Array.find (
            fun x -> x.cOpt = EOL || x.column = hardWantedColumn
        )

    member internal _.AssurePositionBeforeEol (chars: Chars) char_ =
        if char_ = chars.Length then
            max 0 (char_ - 1)
        else
            char_

    // IDisposable

    abstract Dispose: unit -> unit

    default _.Dispose () =
        myContextChangedDisposable.Dispose ()

    interface IDisposable with
        member this.Dispose () =
            this.Dispose ()
