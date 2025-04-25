module WrapLinesPerformer

open Commands.InCommands
open BinarySearch
open Common
open Context
open DataTypes
open WrappedRef

open WrapLinesBuilder
open WrapLinesRenderer

type private DisplayPos = {
    line:    int
    lineRow: int
}

let private zeroDisplayPos = { line = 0; lineRow = 0 }

let private isEqual pos1 pos2 =
    pos1 = pos2

let private isLower pos1 pos2 =
    pos1.line < pos2.line || (pos1.line = pos2.line && pos1.lineRow < pos2.lineRow)

let private isGreater pos1 pos2 =
    pos1.line > pos2.line || (pos1.line = pos2.line && pos1.lineRow > pos2.lineRow)

let private getLower pos1 pos2 =
    if isLower pos1 pos2 then
        pos1
    else
        pos2

let private getGreater pos1 pos2 =
    if isGreater pos1 pos2 then
        pos1
    else
        pos2

let private compareRowFieldTo row a =
    let c = compareTo row a.row
    if c = 0 && a.cOpt = NoChar then
        1
    else
        c

/// WrapLinesPerformer is performer for commands related to WrapLines mode.
/// It performs one command on a single selection at a time.
/// In addition, it provides input and output WantedColumnsActions for the command
/// to be performed.

type WrapLinesPerformer (
    myContextRef: IWrappedRef<AreaContext>,
    myLines:      Lines
) =
    inherit WrapLinesRenderer (myContextRef, myLines)

    let mutable myContext = myContextRef.Value
    let handleContextChanged () = myContext <- myContextRef.Value
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    // set by PerformCommand
    let mutable myLine             = 0
    let mutable myLineRow          = 0
    let mutable myChar             = 0
    let mutable myHardWantedColumn = 0
    let mutable mySoftWantedColumn = 0
    let mutable myDisplayPos       = { line = 0; lineRow = 0 }

    let getScrollRows () =
        myContext.areaHeight - 1

    // commands

    interface ICommandsPerformer<WrapLinesDepCommand> with
        member this.PerformCommand state command =
            myLine             <- state.selection.Cursor.line
            myChar             <- state.selection.Cursor.char
            myHardWantedColumn <- state.selection.CursorWC.hard
            mySoftWantedColumn <- state.selection.CursorWC.soft
            myDisplayPos       <- { line = state.displayLine; lineRow = state.displayLineRow }

            myLineRow          <- this.GetCharFirstRow myLine myChar

            match command with
            | CursorHardUp               -> this.CursorHardUp        ()
            | CursorHardDown             -> this.CursorHardDown      ()
            | CursorSoftUp               -> this.CursorSoftUp        ()
            | CursorSoftDown             -> this.CursorSoftDown      ()
            | CursorSoftLineStart        -> this.CursorSoftLineStart ()
            | CursorSoftLineEnd          -> this.CursorSoftLineEnd   ()
            | CursorSoftFileStart        -> this.CursorSoftFileStart ()
            | CursorSoftFileEnd          -> this.CursorSoftFileEnd   ()
            | CursorHardToLine line      -> this.CursorHardToLine    line
            | ScrollPageUp               -> this.ScrollPageUp        state.isSingleSelection
            | ScrollPageDown             -> this.ScrollPageDown      state.isSingleSelection
            | CenterVertically           -> this.CenterVertically    None true
            | CenterHorizontally         -> this.CenterHorizontally  None true
            | CenterAfterMatch (a, b, c) -> this.CenterAfterMatch    a b c
            | AdaptDisplayPos            -> this.AdaptDisplayPos     ()

            let toUpdateSelection =
                match command with
                | ScrollPageUp
                | ScrollPageDown         -> state.isSingleSelection
                | CenterVertically
                | CenterHorizontally
                | CenterAfterMatch _
                | AdaptDisplayPos        -> false

                | CursorHardUp
                | CursorHardDown
                | CursorSoftUp
                | CursorSoftDown
                | CursorSoftLineStart
                | CursorSoftLineEnd
                | CursorSoftFileStart
                | CursorSoftFileEnd
                | CursorHardToLine _
                | CenterAfterMatch _     -> true

            {
                line              = myLine
                char              = myChar
                toUpdateSelection = toUpdateSelection
                displayLine       = myDisplayPos.line
                displayLineRow    = myDisplayPos.lineRow
                displayColumn     = state.displayColumn
            }

        member _.GetWantedColumnsActions command isSingleSelection =
            match command with
            | CursorHardUp
            | CursorHardDown
                -> [SetHardWantedColumn]
            | CursorSoftUp
            | CursorSoftDown
                -> [SetSoftWantedColumn]
            | CursorSoftLineStart
            | CursorSoftLineEnd
                -> []
            | CursorSoftFileStart
            | CursorSoftFileEnd
                -> [SetSoftWantedColumn]
            | CursorHardToLine _
                -> [SetHardWantedColumn]
            | ScrollPageUp
            | ScrollPageDown
                -> if isSingleSelection then
                        [SetSoftWantedColumn]
                   else
                        []
            | CenterVertically
            | CenterHorizontally
            | CenterAfterMatch _
            | AdaptDisplayPos
                -> []

            ,

            match command with
            | CursorHardUp
            | CursorHardDown
                -> [SetSoftWantedColumn]
            | CursorSoftUp
            | CursorSoftDown
                -> [SetHardWantedColumn]
            | CursorSoftLineStart
            | CursorSoftLineEnd
                -> [SetHardWantedColumn; SetSoftWantedColumn]
            | CursorSoftFileStart
            | CursorSoftFileEnd
                -> [SetHardWantedColumn]
            | CursorHardToLine _
                -> [SetSoftWantedColumn]
            | ScrollPageUp
            | ScrollPageDown
                -> if isSingleSelection then
                        [SetHardWantedColumn]
                   else
                        []
            | CenterVertically
            | CenterHorizontally
            | CenterAfterMatch _
            | AdaptDisplayPos
                -> []

    // CursorHardUp/Down, CursorSoftUp/Down

    member private this.CursorHardUp () =
        myLine <- max 0 (myLine - 1)
        let lineChars = this.GetCursorLineChars ()
        this.ApplyHardWantedColumn lineChars

    member private this.CursorHardDown () =
        myLine <- min this.LastLine (myLine + 1)
        let lineChars = this.GetCursorLineChars ()
        this.ApplyHardWantedColumn lineChars

    member private this.CursorSoftUp () =
        if myLineRow > 0 then
            let lineChars = this.GetCursorLineChars ()
            this.ApplySoftWantedColumn lineChars (myLineRow - 1)
        elif myLine > 0 then
            myLine <- myLine - 1
            let lineChars = this.GetCursorLineChars ()
            let lastRow = this.GetLastRow lineChars
            this.ApplySoftWantedColumn lineChars lastRow

    member private this.CursorSoftDown () =
        let lineChars = this.GetCursorLineChars ()
        let lastRow   = this.GetLastRow lineChars

        if myLineRow < lastRow then
            this.ApplySoftWantedColumn lineChars (myLineRow + 1)
        elif myLine < this.LastLine then
            myLine <- myLine + 1
            let lineChars = this.GetCursorLineChars ()
            this.ApplySoftWantedColumn lineChars 0

    // CursorSoftLineStart/End, CursorSoftFileStart/End

    member private this.CursorSoftLineStart () =
        let lineChars = this.GetCursorLineChars ()
        let a = lineChars |> getFirstEqualInSortedArray (compareRowFieldTo myLineRow)

        myChar <- a.char

    member private this.CursorSoftLineEnd () =
        let lineChars = this.GetCursorLineChars ()
        let a = lineChars |> getLastEqualInSortedArray  (compareRowFieldTo myLineRow)

        myChar <-
            if a.cOpt = EOL then
                max 0 (a.char - 1)
            else
                a.char

    member private this.CursorSoftFileStart () =
        myLine <- 0
        let lineChars = this.GetCursorLineChars ()
        this.ApplySoftWantedColumn lineChars 0

    member private this.CursorSoftFileEnd () =
        myLine <- this.LastLine
        let lineChars = this.GetCursorLineChars ()
        let lastRow = this.GetLastRow lineChars
        this.ApplySoftWantedColumn lineChars lastRow

    // CursorHardToLine

    member private this.CursorHardToLine line =
        myLine <- min this.LastLine (max 0 line)
        let lineChars = this.GetCursorLineChars ()
        this.ApplyHardWantedColumn lineChars

    /// Sets cursor char according to myHardWantedColumn.
    member private this.ApplyHardWantedColumn lineChars =
        let a = this.GetHardWantedLineChar lineChars myHardWantedColumn

        if myContext.cursorBeforeEol then
            let b = this.AssureHardPositionBeforeEol lineChars a
            myChar <- b.char
        else
            myChar <- a.char

    /// Sets cursor char according to row and mySoftWantedColumn.
    member private this.ApplySoftWantedColumn lineChars row =
        let a = this.GetSoftWantedLineChar lineChars row mySoftWantedColumn

        if myContext.cursorBeforeEol then
            let b = this.AssureSoftPositionBeforeEol lineChars a
            myChar <- b.char
        else
            myChar <- a.char

    /// Sets cursor line and char according to displayPos and mySoftWantedColumn.
    member private this.SetCursorBySoftWantedColumn displayPos =
        myLine <- displayPos.line
        let lineChars = this.GetCursorLineChars ()
        this.ApplySoftWantedColumn lineChars displayPos.lineRow

    // ScrollPageUp

    member private this.ScrollPageUp isSingleSelection =
        let scrollRows = getScrollRows ()

        let top = this.FindLineRowUp myDisplayPos.line myDisplayPos.lineRow scrollRows

        myDisplayPos <- top

        if isSingleSelection then
            let scrolledCursor = this.FindLineRowUp myLine myLineRow scrollRows
            let cursor = this.GetAmendedCursorUp top scrolledCursor
            this.SetCursorBySoftWantedColumn cursor

    /// Returns cursor position amended so that there is no need to adapt
    /// displayPos after scrolling up display to top position.
    member private this.GetAmendedCursorUp top cursor =
        if isEqual top zeroDisplayPos then
            cursor
        else
            let offset = (myContext.areaHeight - 1) - myContext.scrollOffsetRows
            let maxCursor = this.FindLineRowDown top.line top.lineRow offset
            getLower maxCursor cursor

    // ScrollPageDown

    member private this.ScrollPageDown isSingleSelection =
        let scrollRows = getScrollRows ()

        let top = this.FindLineRowDown myDisplayPos.line myDisplayPos.lineRow scrollRows

        let maxTop = this.GetMaxTop ()
        let ammendedTop = getLower maxTop top
        let top = getGreater ammendedTop myDisplayPos

        myDisplayPos <- top

        if isSingleSelection then
            let scrolledCursor = this.FindLineRowDown myLine myLineRow scrollRows
            let cursor = this.GetAmendedCursorDown top maxTop scrolledCursor
            this.SetCursorBySoftWantedColumn cursor

    /// Returns cursor position amended so that there is no need to adapt
    /// displayPos after scrolling down display to top position.
    member private this.GetAmendedCursorDown top maxTop cursor =
        if not (isLower top maxTop) then
            cursor
        else
            let offset = 0 + myContext.scrollOffsetRows
            let minCursor = this.FindLineRowDown top.line top.lineRow offset
            getGreater minCursor cursor

    // changing display position

    member private this.CenterVertically (isForward: bool option) (hitBoundary: bool) =
        let offset = (myContext.areaHeight - 1) / 2
        let top = this.FindLineRowUp myLine myLineRow offset

        let maxTop = this.GetMaxTop ()

        let top =
            if isLower top myDisplayPos then
                if not hitBoundary && isForward.Value then
                    myDisplayPos
                else
                    top
            elif isGreater top myDisplayPos then
                if not hitBoundary && not isForward.Value then
                    myDisplayPos
                elif isGreater top maxTop then
                    getGreater myDisplayPos maxTop
                else
                    top
            else  // top = myDisplayPos
                top

        myDisplayPos <- top

    member private _.CenterHorizontally (_isForward: bool option) (_hitFileBoundary: bool) =
        ()

    member private this.CenterAfterMatch isForward hitFileBoundary hitLineBoundary =
        this.CenterVertically   (Some isForward) hitFileBoundary
        this.CenterHorizontally (Some isForward) hitLineBoundary

    member private this.AdaptDisplayPos () =
        let tb = this.GetTopBound ()
        let bb = this.GetBottomBound ()

        if isGreater myDisplayPos tb then
            myDisplayPos <- tb
        elif isLower myDisplayPos bb then
            myDisplayPos <- bb

    /// Returns maximum displayPos, by setting which scrollOffsetRows
    /// will be displayed before the cursor row.
    member private this.GetTopBound () =
        this.FindLineRowUp myLine myLineRow myContext.scrollOffsetRows

    /// Returns minimum displayPos, by setting which scrollOffsetRows
    /// will be displayed after the cursor row.
    member private this.GetBottomBound () =
        let bottom, eof =
            this.FindLineRowDownWithEOF myLine myLineRow myContext.scrollOffsetRows

        let offset =
            if eof then
                (myContext.areaHeight - 1) - 1
            else
                (myContext.areaHeight - 1)

        this.FindLineRowUp bottom.line bottom.lineRow offset

    /// Returns position which is "offset" lines up from line/startRow.
    /// Stops at the start of file.
    member private this.FindLineRowUp line startRow offset =
        let mutable ln = line
        let mutable rc = offset

        rc <- rc - startRow

        while ln > 0 && rc > 0 do
            ln <- ln - 1
            let lineChars = this.GetLineCharsArray ln
            let lastRow = this.GetLastRow lineChars
            rc <- rc - (lastRow + 1)

        { line = ln; lineRow = (max 0 -rc) }

    member private this.FindLineRowDown line startRow offset =
        let position, _eof = this.FindLineRowDownWithEOF line startRow offset
        position

    /// Returns position which is "offset" lines down from line/startRow.
    /// Stops at the end of file - eof return value is true in such case.
    member private this.FindLineRowDownWithEOF line startRow offset =
        let mutable ln = line
        let mutable rc = offset

        let lineChars = this.GetLineCharsArray ln
        let mutable lastRow = this.GetLastRow lineChars
        rc <- rc - (lastRow - startRow)

        while ln < this.LastLine && rc > 0 do
            ln <- ln + 1
            let lineChars = this.GetLineCharsArray ln
            lastRow <- this.GetLastRow lineChars
            rc <- rc - (lastRow + 1)

        let eof = rc > 0

        { line = ln; lineRow = lastRow - (max 0 -rc) }, eof

    // auxiliary

    member private this.GetCursorLineChars () =
        this.GetLineCharsArray myLine

    /// Returns minimum displayPos setting which will cause "~" to be displayed
    /// on the last displayed line.
    member private this.GetMaxTop () =
        let lineChars = this.GetLineCharsArray this.LastLine
        let lastRow = this.GetLastRow lineChars

        let offset = myContext.areaHeight - 2
        this.FindLineRowUp this.LastLine lastRow offset

    // IDisposable

    override _.Dispose () =
        myContextChangedDisposable.Dispose ()
        base.Dispose ()
