module NoWrapLinesPerformer

open Commands.InCommands
open Common
open Context
open DataTypes
open WrappedRef

open NoWrapLinesBuilder
open NoWrapLinesRenderer

type private DisplayPos = {
    line:   int
    column: int
}

/// NoWrapLinesPerformer is performer for commands related to NoWrapLines mode.
/// It performs one command on a single selection at a time.
/// In addition, it provides input and output WantedColumnsActions for the command
/// to be performed.

type NoWrapLinesPerformer (
    myContextRef: IWrappedRef<AreaContext>,
    myLines:      Lines
) =
    inherit NoWrapLinesRenderer (myContextRef, myLines)

    let mutable myContext = myContextRef.Value
    let handleContextChanged () = myContext <- myContextRef.Value
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    // set by PerformCommand
    let mutable myLine             = 0
    let mutable myChar             = 0
    let mutable myHardWantedColumn = 0
    let mutable myDisplayPos       = { line = 0; column = 0 }

    let getScrollRows () =
        myContext.areaHeight - 1

    member private _.Chars =
        myLines[myLine]

    // commands

    interface ICommandsPerformer<WrapLinesDepCommand> with
        member this.PerformCommand state command =
            myLine             <- state.selection.Cursor.line
            myChar             <- state.selection.Cursor.char
            myHardWantedColumn <- state.selection.CursorWC.hard
            myDisplayPos       <- { line = state.displayLine; column = state.displayColumn }

            match command with
            | CursorHardUp             -> this.CursorHardUp        ()
            | CursorHardDown           -> this.CursorHardDown      ()
            | CursorSoftUp             -> this.CursorSoftUp        ()
            | CursorSoftDown           -> this.CursorSoftDown      ()
            | CursorSoftLineStart      -> this.CursorSoftLineStart ()
            | CursorSoftLineEnd        -> this.CursorSoftLineEnd   ()
            | CursorSoftFileStart      -> this.CursorSoftFileStart ()
            | CursorSoftFileEnd        -> this.CursorSoftFileEnd   ()
            | CursorHardToLine line    -> this.CursorHardToLine    line
            | ScrollPageUp             -> this.ScrollPageUp        state.isSingleSelection
            | ScrollPageDown           -> this.ScrollPageDown      state.isSingleSelection
            | CenterVertically         -> this.CenterVertically    None true
            | CenterHorizontally       -> this.CenterHorizontally  None true
            | CenterAfterMatch (a,b,c) -> this.CenterAfterMatch    a b c
            | ScrollCursorTop          -> this.ScrollCursorTop     ()
            | ScrollCursorBottom       -> this.ScrollCursorBottom  ()
            | ScrollCursorLeft         -> this.ScrollCursorLeft    ()
            | ScrollCursorRight        -> this.ScrollCursorRight   ()
            | AdaptDisplayPos          -> this.AdaptDisplayPos     ()

            let toUpdateSelection =
                match command with
                | ScrollPageUp
                | ScrollPageDown       -> state.isSingleSelection
                | CenterVertically
                | CenterHorizontally
                | CenterAfterMatch _
                | ScrollCursorTop
                | ScrollCursorBottom
                | ScrollCursorLeft
                | ScrollCursorRight
                | AdaptDisplayPos      -> false

                | CursorHardUp             
                | CursorHardDown           
                | CursorSoftUp             
                | CursorSoftDown           
                | CursorSoftLineStart      
                | CursorSoftLineEnd        
                | CursorSoftFileStart      
                | CursorSoftFileEnd        
                | CursorHardToLine _    -> true

            {
                line              = myLine
                char              = myChar
                toUpdateSelection = toUpdateSelection
                displayLine       = myDisplayPos.line
                displayLineRow    = state.displayLineRow
                displayColumn     = myDisplayPos.column
            }

        member _.GetWantedColumnsActions command _isSingleSelection =
            match command with
            | CursorHardUp
            | CursorHardDown
            | CursorSoftUp
            | CursorSoftDown
                -> [SetHardWantedColumn]
            | CursorSoftLineStart
            | CursorSoftLineEnd
                -> []
            | CursorSoftFileStart
            | CursorSoftFileEnd
            | CursorHardToLine _
            | ScrollPageUp
            | ScrollPageDown
                -> [SetHardWantedColumn]
            | CenterVertically
            | CenterHorizontally
            | CenterAfterMatch _
            | ScrollCursorTop
            | ScrollCursorBottom
            | ScrollCursorLeft
            | ScrollCursorRight
            | AdaptDisplayPos
                -> []

            ,

            []

    // CursorHardUp/Down, CursorSoftUp/Down

    member private this.CursorHardUp () =
        myLine <- max 0 (myLine - 1)
        let lineChars = this.GetCursorLineChars ()
        this.ApplyWantedColumn lineChars

    member private this.CursorHardDown () =
        myLine <- min this.LastLine (myLine + 1)
        let lineChars = this.GetCursorLineChars ()
        this.ApplyWantedColumn lineChars

    member private this.CursorSoftUp () =
        this.CursorHardUp ()

    member private this.CursorSoftDown () =
        this.CursorHardDown ()

    // CursorSoftLineStart/End, CursorSoftFileStart/End

    member private _.CursorSoftLineStart () =
        myChar <- 0

    member private this.CursorSoftLineEnd () =
        myChar <- max 0 (this.Chars.Length - 1)

    member private this.CursorSoftFileStart () =
        myLine <- 0
        let lineChars = this.GetCursorLineChars ()
        this.ApplyWantedColumn lineChars

    member private this.CursorSoftFileEnd () =
        myLine <- this.LastLine
        let lineChars = this.GetCursorLineChars ()
        this.ApplyWantedColumn lineChars

    // CursorHardToLine

    member private this.CursorHardToLine line =
        myLine <- min this.LastLine (max 0 line)
        let lineChars = this.GetCursorLineChars ()
        this.ApplyWantedColumn lineChars

    /// Sets cursor char according to myHardWantedColumn.
    member private this.ApplyWantedColumn lineChars =
        let a = this.GetHardWantedLineChar lineChars myHardWantedColumn

        if myContext.cursorBeforeEol then
            myChar <- this.AssurePositionBeforeEol this.Chars a.char
        else
            myChar <- a.char

    /// Sets cursor line and char according to line and myHardWantedColumn.
    member private this.SetCursorByHardWantedColumn line =
        myLine <- line
        let lineChars = this.GetCursorLineChars ()
        this.ApplyWantedColumn lineChars

    // ScrollPageUp

    member private this.ScrollPageUp isSingleSelection =
        let scrollRows = getScrollRows ()

        let top = max 0 (myDisplayPos.line - scrollRows)

        myDisplayPos <- { myDisplayPos with line = top }

        if isSingleSelection then
            let scrolledCursor = max 0 (myLine - scrollRows)
            let cursor = this.GetAmendedCursorUp top scrolledCursor
            this.SetCursorByHardWantedColumn cursor

    /// Returns cursor line amended so that there is no need to adapt
    /// display line after scrolling up display to top line.
    member private this.GetAmendedCursorUp top cursor =
        if top = 0 then
            cursor
        else
            let offset = (myContext.areaHeight - 1) - myContext.scrollOffsetRows
            let maxCursor = min this.LastLine (top + offset)
            min maxCursor cursor

    // ScrollPageDown

    member private this.ScrollPageDown isSingleSelection =
        let scrollRows = getScrollRows ()

        let top = min this.LastLine (myDisplayPos.line + scrollRows)

        let maxTop = this.GetMaxTop ()
        let ammendedTop = min maxTop top
        let top = max ammendedTop myDisplayPos.line

        myDisplayPos <- { myDisplayPos with line = top }

        if isSingleSelection then
            let scrolledCursor = min this.LastLine (myLine + scrollRows)
            let cursor = this.GetAmendedCursorDown top maxTop scrolledCursor
            this.SetCursorByHardWantedColumn cursor

    /// Returns cursor line amended so that there is no need to adapt
    /// displayLine after scrolling down display to top line.
    member private this.GetAmendedCursorDown top maxTop cursor =
        if top >= maxTop then
            cursor
        else
            let offset = 0 + myContext.scrollOffsetRows
            let minCursor = min this.LastLine (top + offset)
            max minCursor cursor

    // changing display position

    member private this.CenterVertically (isForward: bool option) (hitBoundary: bool) =
        let offset = (myContext.areaHeight - 1) / 2
        let top = max 0 (myLine - offset)

        let maxTop = this.GetMaxTop ()

        let top =
            if top < myDisplayPos.line then
                if not hitBoundary && isForward.Value then
                    myDisplayPos.line
                else
                    top
            elif top > myDisplayPos.line then
                if not hitBoundary && not isForward.Value then
                    myDisplayPos.line
                elif top > maxTop then
                    max myDisplayPos.line maxTop
                else
                    top
            else  // top = myDisplayPos.line
                top

        myDisplayPos <- { myDisplayPos with line = top }

    member private this.CenterHorizontally (isForward: bool option) (hitBoundary: bool) =
        let cursorFirstColumn, _cursorLastColumn =
            this.GetCursorColumns ()

        let offset =
            match isForward with
            | Some false -> 3 * (myContext.textWidth - 1) / 4
            | Some true  -> 3 * (myContext.textWidth - 1) / 4
            | None       ->     (myContext.textWidth - 1) / 2

        let left = max 0 (cursorFirstColumn - offset)

        let maxLeft = this.GetMaxLeft ()

        let left =
            if left < myDisplayPos.column then
                if not hitBoundary && isForward.Value then
                    myDisplayPos.column
                else
                    left
            elif left > myDisplayPos.column then
                if not hitBoundary && not isForward.Value then
                    myDisplayPos.column
                elif left > maxLeft then
                    max myDisplayPos.column maxLeft
                else
                    left
            else  // left = myDisplayPos.column
                left

        myDisplayPos <- { myDisplayPos with column = left }

    member private this.CenterAfterMatch isForward hitFileBoundary hitLineBoundary =
        this.CenterVertically   (Some isForward) hitFileBoundary
        this.CenterHorizontally (Some isForward) hitLineBoundary

    member private _.ScrollCursorTop () =
        myDisplayPos <- { myDisplayPos with line = IntType.MaxValue }

    member private _.ScrollCursorBottom () =
        myDisplayPos <- { myDisplayPos with line = 0 }

    member private _.ScrollCursorLeft () =
        myDisplayPos <- { myDisplayPos with column = IntType.MaxValue }

    member private _.ScrollCursorRight () =
        myDisplayPos <- { myDisplayPos with column = 0 }

    member private this.AdaptDisplayPos () =
        let bb = this.GetBottomBound ()
        let tb = this.GetTopBound ()

        let cursorFirstColumn, cursorLastColumn =
            this.GetCursorColumns ()

        let lb = this.GetLeftBound cursorFirstColumn
        let rb = this.GetRightBound cursorLastColumn

        if myDisplayPos.line > tb then
            myDisplayPos <- { myDisplayPos with line = tb }
        elif myDisplayPos.line < bb then
            myDisplayPos <- { myDisplayPos with line = bb }

        if myDisplayPos.column > lb then
            myDisplayPos <- { myDisplayPos with column = lb }
        elif myDisplayPos.column < rb then
            myDisplayPos <- { myDisplayPos with column = rb }

    /// Returns maximum displayLine, by setting which scrollOffsetRows
    /// will be displayed before the cursor row.
    member private _.GetTopBound () =
        max 0 (myLine - myContext.scrollOffsetRows)

    /// Returns minimum displayLine, by setting which scrollOffsetRows
    /// will be displayed before the cursor row.
    member private _.GetBottomBound () =
        let bottom =
            min (myLine + myContext.scrollOffsetRows)
                myLines.Count  // the first "~" line after EOF
        max 0 (bottom - (myContext.areaHeight - 1))

    /// Returns maximum displayColumn, by setting which scrollOffsetColumns
    /// will be displayed before the cursor first column.
    member private _.GetLeftBound cursorFirstColumn =
        max 0 (cursorFirstColumn - myContext.scrollOffsetColumns)

    /// Returns minimum displayColumn, by setting which scrollOffsetColumns
    /// will be displayed after the last cursor column.
    member private this.GetRightBound cursorLastColumn =
        let right =
            min (cursorLastColumn + myContext.scrollOffsetColumns)
                (this.GetCursorLineLastColumn ())
        max 0 (right - (myContext.textWidth - 1))

    // auxiliary

    member private this.GetCursorColumns () =
        this.GetCharColumns myLine myChar

    member private this.GetCursorLineLastColumn () =
        let lineChars = this.GetCursorLineChars ()
        this.GetLastColumn lineChars

    member private this.GetCursorLineChars () =
        this.GetLineCharsArray myLine

    /// Returns minimum displayPos.line setting which will cause "~" to be displayed
    /// on the last displayed line.
    member private this.GetMaxTop () =
        let offset = myContext.areaHeight - 2
        max 0 (this.LastLine - offset)

    /// Returns minimum displayPos.column setting which will cause EOL to be displayed
    /// on the last displayed column of cursor line.
    member private this.GetMaxLeft () =
        let offset = myContext.textWidth - 1
        let lastColumn = this.GetCursorLineLastColumn ()
        max 0 (lastColumn - offset)

    // IDisposable

    override _.Dispose () =
        myContextChangedDisposable.Dispose ()
        base.Dispose ()
