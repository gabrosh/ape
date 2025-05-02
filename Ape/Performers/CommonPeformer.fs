module CommonPerformer

open System

open BinarySearch
open CharCategories
open Commands.InCommands
open Common
open Context
open DataTypes
open Position
open Selection
open TextRange
open UserMessages
open WrappedRef

let private compareFirstTo (first: Position) (a: TextRange) =
    compareTo first a.first

/// CommonPerformer is performer for commands independent from NoWrap and WrapLines modes.
/// It performs one command on a single selection at a time.
/// In addition, it provides input and output WantedColumnsActions for the command
/// to be performed.

[<Sealed>]
type CommonPerformer (
    myContextRef:   IWrappedRef<AreaContext>,
    myUserMessages: UserMessages,
    myLines:        Lines,
    myMatchRanges:  IMatchRanges.IMatchRanges
) =
    let mutable myContext = myContextRef.Value
    let handleContextChanged () = myContext <- myContextRef.Value
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    // set by PerformCommand, changed by the command
    let mutable myLine = 0
    let mutable myChar = 0
    // set by PerformCommand, not changed by the command
    let mutable mySelection   = Selection_Invalid
    let mutable myIsFirstCall = false

    member private _.Chars =
        myLines[myLine]

    // commands

    member this.PerformCommand state command =
        myLine      <- state.selection.Cursor.line
        myChar      <- state.selection.Cursor.char
        mySelection <- state.selection

        myIsFirstCall <-
            match state.prevCommand with
            | Some (CommonCommand prevCommand) ->
                this.GetIsFirstCall prevCommand command
            | _ ->
                true

        match command with
        | CursorLeft                 -> this.CursorLeft                 ()
        | CursorRight                -> this.CursorRight                ()
        | CursorAfterSelection ae    -> this.CursorAfterSelection       ae
        | CursorLeftAtWordStart      -> this.CursorLeftAtWordStart      ()
        | CursorLeftAtWordEnd        -> this.CursorLeftAtWordEnd        ()
        | CursorLeftAfterWordEnd     -> this.CursorLeftAfterWordEnd     ()
        | CursorLeftToChar c         -> this.CursorLeftToChar           c
        | CursorLeftUntilChar c      -> this.CursorLeftUntilChar        c
        | CursorRightAtWordStart     -> this.CursorRightAtWordStart     ()
        | CursorRightAtWordEnd       -> this.CursorRightAtWordEnd       ()
        | CursorRightBeforeWordStart -> this.CursorRightBeforeWordStart ()
        | CursorRightToChar c        -> this.CursorRightToChar          c
        | CursorRightUntilChar c     -> this.CursorRightUntilChar       c
        | CursorToPairChar           -> this.CursorToPairChar           ()
        | CursorHardLineStart        -> this.CursorHardLineStart        ()
        | CursorHardLineEnd          -> this.CursorHardLineEnd          ()
        | CursorAtEol                -> this.CursorAtEol                ()
        | CursorBeforeEol            -> this.CursorBeforeEol            ()
        | CursorAt char              -> this.CursorAt                   char
        | CursorToNextMatch ii       -> this.CursorToNextMatch          ii
        | CursorToPrevMatch ii       -> this.CursorToPrevMatch          ii
        | ClearInfoMessage           -> this.ClearInfoMessage           ()
        | AssertNonWhiteSpace        -> this.AssertNonWhiteSpace        ()

        {
            line              = myLine
            char              = myChar
            toUpdateSelection = true
            displayLine       = state.displayLine
            displayLineRow    = state.displayLineRow
            displayColumn     = state.displayColumn
        }

    member _.GetWantedColumnsActions _command _isSingleSelection =
        []

        ,

        [SetHardWantedColumn; SetSoftWantedColumn]

    member private _.GetIsFirstCall prevCommand command =
        match command with
        | CursorLeftAtWordStart | CursorRightAtWordStart ->
            match prevCommand with
            | CursorLeftAtWordStart
            | CursorRightAtWordStart -> false
            | _ -> true
        | CursorLeftAtWordEnd   | CursorRightAtWordEnd   ->
            match prevCommand with
            | CursorLeftAtWordEnd
            | CursorRightAtWordEnd   -> false
            | _ -> true
        | CursorLeftToChar c    | CursorRightToChar c    ->
            match prevCommand with
            | CursorLeftToChar  x
            | CursorRightToChar x    -> x <> c
            | _ -> true
        | CursorToNextMatch _   | CursorToPrevMatch _    ->
            match prevCommand with
            | CursorToNextMatch _
            | CursorToPrevMatch _    -> false
            | _ -> true
        | _ ->
            command <> prevCommand

    // CursorLeft/Right, CursorAfterSelection

    member private this.CursorLeft () =
        if myChar > 0 then
            myChar <- myChar - 1
        elif myLine > 0 then
            myLine <- myLine - 1
            myChar <- this.Chars.Length

    member private this.CursorRight () =
        if myChar < this.Chars.Length then
            myChar <- myChar + 1
        elif myLine < myLines.Count - 1 then
            myLine <- myLine + 1
            myChar <- 0

    member private _.CursorAfterSelection afterEof =
        let line  = mySelection.last.line
        let char_ = mySelection.last.char

        if char_ < myLines[line].Length then
            myLine <- line
            myChar <- char_ + 1
        elif line < myLines.Count - 1 || afterEof then
            // existing or not existing position
            myLine <- line + 1
            myChar <- 0
        else
            myLine <- line
            myChar <- char_

    // CursorLeft...

    member private this.CursorLeftAtWordStart () =
        this.CursorLeftWord isAtWordStart

    member private this.CursorLeftAtWordEnd () =
        this.CursorLeftWord isAtWordEnd

    member private this.CursorLeftAfterWordEnd () =
        this.CursorLeftWord isAfterWordEnd

    member private this.CursorLeftWord isAtFun =
        this.CursorToWithStop
            myIsFirstCall this.GetPrevPositionWithStop isAtFun

    member private this.CursorLeftToChar c =
        this.CursorToCharCycling
            myIsFirstCall this.GetPrevPositionCycling (isAtChar c)

    member private this.CursorLeftUntilChar c =
        this.CursorUntilCharCycling
            myIsFirstCall this.GetPrevPositionCycling (isAtChar c)

    // CursorRight...

    member private this.CursorRightAtWordStart () =
        this.CursorRightWord isAtWordStart

    member private this.CursorRightAtWordEnd () =
        this.CursorRightWord isAtWordEnd

    member private this.CursorRightBeforeWordStart () =
        this.CursorRightWord isBeforeWordStart

    member private this.CursorRightWord isAtFun =
        this.CursorToWithStop
            myIsFirstCall this.GetNextPositionWithStop isAtFun

    member private this.CursorRightToChar c =
        this.CursorToCharCycling
            myIsFirstCall this.GetNextPositionCycling (isAtChar c)

    member private this.CursorRightUntilChar c =
        this.CursorUntilCharCycling
            myIsFirstCall this.GetNextPositionCycling (isAtChar c)

    // CursorToPairing

    member private this.CursorToPairChar () =
        match this.GetPairCharArgs myLine myChar with
        | Some (getNextPosition, openChar, closeChar) ->
            this.CursorToPairCharWithStop getNextPosition openChar closeChar
        | None ->
            myUserMessages.RegisterMessage ERROR_CURSOR_AT_NON_PAIRED_CHAR

    // CursorHardLineStart/End, CursorToNewLine

    member private _.CursorHardLineStart () =
        myChar <- 0

    member private this.CursorHardLineEnd () =
        this.SetCursorPosition this.Chars.Length

    member private this.CursorAtEol () =
        myChar <- this.Chars.Length

    member private this.CursorBeforeEol () =
        myChar <- max 0 (this.Chars.Length - 1)

    member private _.CursorAt char =
        myChar <- char

    /// Sets cursor position according to char_.
    member private this.SetCursorPosition char_ =
        if myContext.cursorBeforeEol && char_ = this.Chars.Length then
            myChar <- max 0 (char_ - 1)
        else
            myChar <- char_

    // CursorNextMatch, CursorPrevMatch

    member private _.CursorToNextMatch isInitial =
        let toResearch = (
            not isInitial && myMatchRanges.GetMainGroupCount () = 0
        )

        if toResearch then
            myMatchRanges.ReSearch ()

        if myMatchRanges.GetMainGroupCount () <> 0 then
            let compareFun = compareFirstTo { line = myLine; char = myChar }

            let f = if isInitial || toResearch || myIsFirstCall
                    then findFirstEqOrGtInSortedArray
                    else findFirstGreaterInSortedArray

            let matchRanges = myMatchRanges.GetAllFromMainGroup ()

            match f compareFun matchRanges with
            | Some m ->
                let first = matchRanges[m].first
                myLine <- first.line
                myChar <- first.char
            | None   ->
                let first = matchRanges[0].first
                myLine <- first.line
                myChar <- first.char
                myUserMessages.RegisterMessage WARNING_SEARCH_HIT_BOTTOM_CONT_AT_TOP

    member private _.CursorToPrevMatch isInitial =
        let toResearch = (
            not isInitial && myMatchRanges.GetMainGroupCount () = 0
        )

        if toResearch then
            myMatchRanges.ReSearch ()

        if myMatchRanges.GetMainGroupCount () <> 0 then
            let compareFun = compareFirstTo { line = myLine; char = myChar }

            let f = if isInitial || toResearch || myIsFirstCall 
                    then findLastEqOrLtInSortedArray
                    else findLastLowerInSortedArray

            let matchRanges = myMatchRanges.GetAllFromMainGroup ()

            match f compareFun matchRanges with
            | Some m ->
                let first = matchRanges[m].first
                myLine <- first.line
                myChar <- first.char
            | None   ->
                let first = matchRanges[matchRanges.Count - 1].first
                myLine <- first.line
                myChar <- first.char
                myUserMessages.RegisterMessage WARNING_SEARCH_HIT_TOP_CONT_AT_BOTTOM

    // Clear..., Assert...

    member private _.ClearInfoMessage () =
        if myUserMessages.HasInfoMessage then
            myUserMessages.RetrieveMessage ()
                |> ignore

    member private _.AssertNonWhiteSpace () =
        let chars = myLines[myLine]

        let isAtWhiteSpace =
            myChar = chars.Length || System.Char.IsWhiteSpace chars[myChar]

        if isAtWhiteSpace then
            myUserMessages.RegisterMessage ERROR_CURSOR_AT_WHITE_SPACE

    // auxiliary - CursorToWithStop, CursorToCycling, CursorUntilCycling

    member private _.CursorToWithStop isFirstCall getSuccesivePosition isAtFun =
        // Returns found position or the last valid position.
        let rec loop = fun oldLine oldChar ->
            match getSuccesivePosition oldLine oldChar with
            | Some (line, char_) ->
                if isAtFun myLines line char_ then
                    (line, char_)
                else
                    loop line char_
            | None ->
                (oldLine, oldChar)

        if not (isFirstCall && isAtFun myLines myLine myChar) then
            let line, char_ = loop myLine myChar
            myLine <- line
            myChar <- char_

    member private _.CursorToCharCycling isFirstCall getSuccesivePosition isAtFun =
        // Returns found position or None.
        let rec loop = fun origin oldLine oldChar ->
            match getSuccesivePosition origin oldLine oldChar with
            | Some (line, char_) ->
                if isAtFun myLines line char_ then
                    Some (line, char_)
                else
                    loop origin line char_
            | None ->
                None

        if not (isFirstCall && isAtFun myLines myLine myChar) then
            match loop (myLine, myChar) myLine myChar with
            | Some (line, char_) ->
                myLine <- line
                myChar <- char_
            | None ->
                if not (isAtFun myLines myLine myChar) then
                    // Replace the previous warning message.
                    myUserMessages.RegisterUpdatedMessage WARNING_CHAR_NOT_FOUND

    member private _.CursorUntilCharCycling isFirstCall getSuccesivePosition isAtFun =
        // Returns position preceding to the found position or None.
        let rec loop = fun origin oldLine oldChar ->
            match getSuccesivePosition origin oldLine oldChar with
            | Some (line, char_) ->
                if isAtFun myLines line char_ then
                    Some (oldLine, oldChar)
                else
                    loop origin line char_
            | None ->
                None

        match getSuccesivePosition (myLine, myChar) myLine myChar with
        | Some (newLine, newChar) ->
            if not (isFirstCall && isAtFun myLines newLine newChar) then
                match loop (newLine, newChar) newLine newChar with
                | Some (line, char_) ->
                    myLine <- line
                    myChar <- char_
                | None ->
                    if not (isAtFun myLines newLine newChar) then
                        // Replace the previous warning message.
                        myUserMessages.RegisterUpdatedMessage WARNING_CHAR_NOT_FOUND

        | None ->
            // Replace the previous warning message.
            myUserMessages.RegisterUpdatedMessage WARNING_CHAR_NOT_FOUND

    // auxiliary - GetPairCharArgs, CursorToPairCharWithStop

    member private this.GetPairCharArgs line char_ =
        let chars = myLines[line]

        if char_ < chars.Length then
            match chars[char_] with
            | '(' -> Some (this.GetNextPositionWithStop, '(', ')')
            | '[' -> Some (this.GetNextPositionWithStop, '[', ']')
            | '{' -> Some (this.GetNextPositionWithStop, '{', '}')
            | '<' -> Some (this.GetNextPositionWithStop, '<', '>')
            | ')' -> Some (this.GetPrevPositionWithStop, ')', '(')
            | ']' -> Some (this.GetPrevPositionWithStop, ']', '[')
            | '}' -> Some (this.GetPrevPositionWithStop, '}', '{')
            | '>' -> Some (this.GetPrevPositionWithStop, '>', '<')
            | _   -> None
        else
            None

    member private this.CursorToPairCharWithStop getNextPosition openChar closeChar =
        // Returns found position or None.
        let rec loop = fun oldLine oldChar level ->
            match getNextPosition oldLine oldChar with
            | Some (line, char_) ->
                let c = this.GetCharAt line char_

                if c = closeChar && level = 0 then
                    Some (line, char_)
                elif c = closeChar then
                    loop line char_ (level - 1)
                elif c = openChar then
                    loop line char_ (level + 1)
                else
                    loop line char_ level
            | None ->
                None

        match loop myLine myChar 0 with
        | Some (line, char_) ->
            myLine <- line
            myChar <- char_
        | None               ->
            // Replace the previous warning message.
            myUserMessages.RegisterUpdatedMessage WARNING_PAIRING_CHAR_NOT_FOUND

    member private _.GetCharAt line char_ =
        let chars = myLines[line]

        if char_ < chars.Length then
            chars[char_]
        else
            '\n'

    // auxiliary - GetPrevPosition..., GetNextPosition...

    member private _.GetPrevPositionWithStop oldLine oldChar =
        if oldChar > 0 then
            Some (oldLine, oldChar - 1)
        elif oldLine > 0 then
            let line = oldLine - 1
            Some (line, myLines[line].Length)
        else
            myUserMessages.RegisterMessage WARNING_SEARCH_STOPPED_AT_TOP
            None

    member private _.GetNextPositionWithStop oldLine oldChar =
        if oldChar < myLines[oldLine].Length then
            Some (oldLine, oldChar + 1)
        elif oldLine < myLines.Count - 1 then
            Some (oldLine + 1, 0)
        else
            myUserMessages.RegisterMessage WARNING_SEARCH_STOPPED_AT_BOTTOM
            None

    member private _.GetPrevPositionCycling origin line char_ =
        let newPosition =
            if char_ > 0 then
                (line, char_ - 1)
            elif line > 0 then
                let line = line - 1
                (line, myLines[line].Length)
            else
                myUserMessages.RegisterMessage WARNING_SEARCH_HIT_TOP_CONT_AT_BOTTOM
                let line = myLines.Count - 1
                (line, myLines[line].Length)

        if newPosition <> origin then
            Some newPosition
        else
            None

    member private _.GetNextPositionCycling origin line char_ =
        let newPosition =
            if char_ < myLines[line].Length then
                (line, char_ + 1)
            elif line < myLines.Count - 1 then
                (line + 1, 0)
            else
                myUserMessages.RegisterMessage WARNING_SEARCH_HIT_BOTTOM_CONT_AT_TOP
                (0, 0)

        if newPosition <> origin then
            Some newPosition
        else
            None

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable.Dispose ()
