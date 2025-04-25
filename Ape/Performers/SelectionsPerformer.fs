module SelectionsPerformer

open Commands.InCommands
open Common
open DataTypes
open LinesAccessor
open Position
open Selection
open Selections
open SelectionsRegisters
open TextRange
open UserMessages

let slrIsMatch (slr: SingleLineRegex.IsMatch) =
    slr.IsMatch

let mlrIsMatch (mlr: MultiLineRegex.IsMatch) =
    mlr.IsMatch

/// SelectionsPerformer is performer for commands on current selections.
/// It performs one command on all current selections at a time.
/// In addition, it provides input and output WantedColumnsActions for the command
/// to be performed.

type SelectionsPerformer (
    myUserMessages:        UserMessages,
    myLines:               Lines,
    mySelections:          Selections,
    mySelectionsRegisters: SelectionsRegisters,
    myUndoProvider:        UndoProvider.UndoProvider
) =
    let myAccessor = LinesAccessor myLines

    // commands

    /// Performs command.
    member this.PerformCommand displayRenderer command =
        match command with
        | CopyFirstUp           -> this.CopyFirstUp        displayRenderer
        | CopyLastDown          -> this.CopyLastDown       displayRenderer
        | Multiply        count -> this.Multiply           count
        | SelectWholeBuffer     -> this.SelectWholeBuffer  ()
        | InvertSelections      -> this.InvertSelections   ()
        | SplitOnLineStarts     -> this.SplitOnLineStarts  ()
        | MergeContiguous       -> this.MergeContiguous    ()
        | ExpandToFullLines     -> this.ExpandToFullLines  ()
        | TrimToFullLines       -> this.TrimToFullLines    ()
        | ReduceToCursor        -> this.ReduceToCursor     ()
        | ForwardDirection      -> this.ForwardDirection   ()
        | FlipDirection         -> this.FlipDirection      ()
        | KeepOnlyMain          -> this.KeepOnlyMain       ()
        | RemoveMain            -> this.RemoveMain         ()
        | RemoveLessIndented    -> this.RemoveLessIndented displayRenderer
        | RemoveMoreIndented    -> this.RemoveMoreIndented displayRenderer
        | RotateUp              -> this.RotateUp           ()
        | RotateDown            -> this.RotateDown         ()
        | SelectMatching  regex -> this.SelectMatching     regex
        | KeepMatching    regex -> this.KeepMatching       regex
        | DiscardMatching regex -> this.DiscardMatching    regex
        | Store           r     -> this.Store              r
        | Load            r     -> this.Load               r
        | RemoveStored    r     -> this.RemoveStored       r

    /// Returns wanted columns actions to be performed before and after given command.
    member _.GetWantedColumnsActions command =
        match command with
        | CopyFirstUp
        | CopyLastDown
            -> [SetHardWantedColumn; SetSoftWantedColumn]
        | Multiply _
            -> []
        | SelectWholeBuffer
        | InvertSelections
        | SplitOnLineStarts
        | MergeContiguous
        | ExpandToFullLines
        | TrimToFullLines
            -> []
        | ReduceToCursor
            -> [SetHardWantedColumn; SetSoftWantedColumn]
        | ForwardDirection
        | FlipDirection
        | KeepOnlyMain
        | RemoveMain
        | RemoveLessIndented
        | RemoveMoreIndented
        | RotateUp
        | RotateDown
            -> []
        | SelectMatching _
            -> []
        | KeepMatching _
        | DiscardMatching _
            -> []
        | Store _
            -> [SetHardWantedColumn; SetSoftWantedColumn]
        | Load _
        | RemoveStored _
            -> []

        ,

        match command with
        | CopyFirstUp
        | CopyLastDown
            -> []
        | Multiply _
            -> []
        | SelectWholeBuffer
        | InvertSelections
        | SplitOnLineStarts
        | MergeContiguous
        | ExpandToFullLines
        | TrimToFullLines
            -> [SetHardWantedColumn; SetSoftWantedColumn]
        | ReduceToCursor
            -> []
        | ForwardDirection
        | FlipDirection
        | KeepOnlyMain
        | RemoveMain
        | RemoveLessIndented
        | RemoveMoreIndented
        | RotateUp
        | RotateDown
            -> []
        | SelectMatching _
            -> [SetHardWantedColumn; SetSoftWantedColumn]
        | KeepMatching _
        | DiscardMatching _
            -> []
        | Store _
            -> []
        | Load _
            -> [SetHardWantedColumn; SetSoftWantedColumn]
        | RemoveStored _
            -> []

    // CopyFirstUp, CopyLastDown

    member private _.CopyFirstUp (dp: IDisplayRenderer) =
        let selection = mySelections[0]
        match dp.GetCopiedSelectionUp selection with
        | Some selection -> mySelections.Add selection
        | None           -> ()

    member private _.CopyLastDown (dp: IDisplayRenderer) =
        let selection = mySelections[mySelections.Count - 1]
        match dp.GetCopiedSelectionDown selection with
        | Some selection -> mySelections.Add selection
        | None           -> ()

    member private _.Multiply count =
        if count <= 0 then
            invalidOp "Invalid argument: count <= 0"

        let newSelections = ResizeArray ()

        let mainIndex = count * (mySelections.MainIndex + 1) - 1

        for i = 0 to mySelections.Count - 1 do
            for j = 1 to count do
                newSelections.Add mySelections[i]

        mySelections.SetFrom newSelections mainIndex

    // SelectWholeBuffer, InvertSelections, SplitOnLineStarts, MergeContiguous

    member private _.SelectWholeBuffer () =
        mySelections.Clear ()

        let maxLine = myLines.Count - 1
        let maxChar = myLines[maxLine].Length

        mySelections.Add {
            first     = { line = 0; char = 0 }
            last      = { line = maxLine; char = maxChar}
            firstWC   = WantedColumns_Default
            lastWC    = WantedColumns_Default
            isForward = true
        }

    member private this.InvertSelections () =
        let ranges = mySelections.GetSelectedRanges ()

        let newRanges = this.InvertRanges ranges

        let selections = seq {
            for range in newRanges -> {
                first     = range.first
                last      = range.last
                firstWC   = WantedColumns_Default
                lastWC    = WantedColumns_Default
                isForward = true
            }
        }

        mySelections.SetFrom selections -1

    member private _.InvertRanges ranges : ResizeArray<TextRange> =
        let result = ResizeArray<TextRange> ()

        let mutable first = Position_Zero

        for range in ranges do
            if first < range.first then
                let last = myAccessor.GetPrevChar range.first
                result.Add { first = first; last = last }

            first <- myAccessor.GetNextChar range.last

        let eofPosition = myAccessor.EofPosition

        if first < eofPosition then
            let last = myAccessor.GetPrevChar eofPosition
            result.Add { first = first; last = last }

        if result.Count = 0 then
            let last = myAccessor.GetPrevChar eofPosition
            result.Add { first = last; last = last }

        result

    member private this.SplitOnLineStarts () =
        mySelections.ReplaceSelections (fun selection ->
            let result = ResizeArray<Selection> ()

            if selection.first.line = selection.last.line then
                result.Add selection
            else
                let first     = selection.first
                let last      = selection.last
                let isForward = selection.isForward

                result.Add (this.GetLine2ndPart first.line first.char isForward)

                for i = first.line + 1 to last.line - 1 do
                    result.Add (this.GetWholeLine i isForward)

                result.Add (this.GetLine1stPart last.line  last.char  isForward)

            result
        ) |> ignore

    member private _.GetLine1stPart line char_ isForward =
        {
            Selection_Default with
                first     = { line = line; char = 0 }
                last      = { line = line; char = char_ }
                isForward = isForward
        }

    member private _.GetLine2ndPart line char_ isForward =
        {
            Selection_Default with
                first     = { line = line; char = char_ }
                last      = { line = line; char = myLines[line].Length }
                isForward = isForward
        }

    member private _.GetWholeLine line isForward =
        {
            Selection_Default with
                first     = { line = line; char = 0 }
                last      = { line = line; char = myLines[line].Length }
                isForward = isForward
        }

    member private this.MergeContiguous () =
        let ranges = mySelections.GetSelectedRanges ()

        let newRanges = this.JoinTouchingRanges ranges

        let selections = seq {
            for range in newRanges -> {
                first     = range.first
                last      = range.last
                firstWC   = WantedColumns_Default
                lastWC    = WantedColumns_Default
                isForward = true
            }
        }

        mySelections.SetFrom selections -1

    member private _.JoinTouchingRanges ranges : ResizeArray<TextRange> =
        let result = ResizeArray<TextRange> ()

        let mutable prevRange = ranges |> Seq.head

        for range in ranges |> Seq.tail do
            let next = myAccessor.GetNextChar prevRange.last

            if next = range.first then
                prevRange <- {
                    first = prevRange.first; last = range.last
                }
            else
                result.Add prevRange
                prevRange <- range

        result.Add prevRange
        result

    // ExpandToFullLines, TrimToFullLines, ReduceToCursor

    member private _.ExpandToFullLines () =
        mySelections.UpdateSelections (fun selection ->
            let first = selection.first
            let last  = selection.last

            let maxChar = myLines[last.line].Length

            {
                first     = { first with char = 0 }
                last      = { last  with char = maxChar }
                firstWC   = WantedColumns_Default
                lastWC    = WantedColumns_Default
                isForward = selection.isForward
            }
        )

    member private this.TrimToFullLines () =
        mySelections.UpdateSelections (fun selection ->
            let first = selection.first
            let last  = selection.last

            let deltaLine = last.line - first.line

            let hasFullLine =
                if deltaLine = 1 then
                    let maxChar = myLines[last.line].Length
                    first.char = 0 || last.char = maxChar
                else
                    deltaLine > 1

            let first = this.AssureFirstLineFull first hasFullLine
            let last  = this.AssureLastLineFull  last  hasFullLine

            {
                first     = first
                last      = last
                firstWC   = WantedColumns_Default
                lastWC    = WantedColumns_Default
                isForward = selection.isForward
            }
        )

    member private _.AssureFirstLineFull first hasFullLine =
        if hasFullLine && first.char <> 0 then
            let line = first.line + 1
            { line = line; char = 0 }
        else
            first

    member private _.AssureLastLineFull last hasFullLine =
        let maxChar = myLines[last.line].Length
        if hasFullLine && last.char <> maxChar then
            let line = last.line - 1
            let maxChar = myLines[line].Length
            { line = line; char = maxChar }
        else
            last

    member private _.ReduceToCursor () =
        mySelections.UpdateSelections (fun selection ->
            selection.ReduceToCursor ()
        )

    // ForwardDirection, FlipDirection, ...

    member private _.ForwardDirection () =
        mySelections.UpdateSelections (fun selection ->
            {
                selection with isForward = true
            }
        )

    member private _.FlipDirection () =
        mySelections.UpdateSelections (fun selection ->
            {
                selection with isForward = not selection.isForward
            }
        )

    member private _.KeepOnlyMain () =
        mySelections.KeepOnlyMain ()

    member private _.RemoveMain () =
        if mySelections.Count > 1 then
            mySelections.RemoveMain ()

    member private _.RemoveLessIndented displayRenderer =
        let first = mySelections.Main.first
        let mainFirstColumn, _mainLastColumn =
            displayRenderer.GetCharColumns first.line first.char

        mySelections.RemoveAll (
            fun selection ->
                let first = selection.first
                let firstColumn, _lastColumn =
                    displayRenderer.GetCharColumns first.line first.char
                firstColumn < mainFirstColumn
        ) |> ignore

    member private _.RemoveMoreIndented displayRenderer =
        let first = mySelections.Main.first
        let mainFirstColumn, _mainLastColumn =
            displayRenderer.GetCharColumns first.line first.char

        mySelections.RemoveAll (
            fun selection ->
                let first = selection.first
                let firstColumn, _lastColumn =
                    displayRenderer.GetCharColumns first.line first.char
                firstColumn > mainFirstColumn
        ) |> ignore

    member private _.RotateUp () =
        let isTopHit = mySelections.RotateUp()

        if isTopHit then
            myUserMessages.RegisterMessage INFO_ROTATION_HIT_TOP

    member private _.RotateDown () =
        let isBottomHit = mySelections.RotateDown()

        if isBottomHit then
            myUserMessages.RegisterMessage INFO_ROTATION_HIT_BOTTOM

    // SelectMatching, KeepMatching, DiscardMatching

    member private this.SelectMatching regex =
        let regexObject = RegexUtils.makeRegexObject regex

        if RegexUtils.isMultiLineRegex regex then
            this.SelectMatchingMultiLine regexObject
        else
            this.SelectMatchingSingleLine regexObject

    member private this.KeepMatching regex =
        let regexObject = RegexUtils.makeRegexObject regex

        if RegexUtils.isMultiLineRegex regex then
            this.RemoveFulfillingMultiLine  regexObject (mlrIsMatch >> not)
        else
            this.RemoveFulfillingSingleLine regexObject (slrIsMatch >> not)

    member private this.DiscardMatching regex =
        let regexObject = RegexUtils.makeRegexObject regex

        if RegexUtils.isMultiLineRegex regex then
            this.RemoveFulfillingMultiLine  regexObject mlrIsMatch
        else
            this.RemoveFulfillingSingleLine regexObject slrIsMatch

    member private this.SelectMatchingSingleLine regexObject =
        let slr = SingleLineRegex.AddMatchesAsSelections regexObject

        let wereAllRemoved = mySelections.ReplaceSelections (
            fun selection ->
                let first     = selection.first
                let last      = selection.last
                let rightKept = myAccessor.GetNextChar last
                let isForward = selection.isForward

                let result = ResizeArray<Selection> ()

                slr.Init isForward result
                this.ProcessText first rightKept slr.ProcessLine |> ignore
                slr.FinishProcessing ()

                result
        )

        if wereAllRemoved then
            myUserMessages.RegisterMessage WARNING_NO_MATCH_FOUND

    member private this.RemoveFulfillingSingleLine regexObject predicate =
        let slr = SingleLineRegex.IsMatch regexObject

        let mutable wasAnyRemoved = false

        let wereAllRemoved = mySelections.RemoveAll (
            fun selection ->
                let first     = selection.first
                let last      = selection.last
                let rightKept = myAccessor.GetNextChar last

                slr.Init ()
                this.ProcessText first rightKept slr.ProcessLine |> ignore
                slr.FinishProcessing ()

                let isToRemove = predicate slr
                wasAnyRemoved <- wasAnyRemoved || isToRemove
                isToRemove
        )

        if not wasAnyRemoved then
            myUserMessages.RegisterMessage INFO_NO_SELECTIONS_DISCARDED
        elif wereAllRemoved then
            myUserMessages.RegisterMessage WARNING_ALL_SELECTIONS_DISCARDED

    member private this.SelectMatchingMultiLine regexObject =
        let wereAllRemoved = mySelections.ReplaceSelections (
            fun selection ->
                let first     = selection.first
                let last      = selection.last
                let rightKept = myAccessor.GetNextChar last
                let isForward = selection.isForward

                let result = ResizeArray<Selection> ()

                let mlr = MultiLineRegex.AddMatchesAsSelections (
                    myUserMessages, regexObject, result, isForward
                )
                this.ProcessText first rightKept mlr.ProcessLine |> ignore
                mlr.FinishProcessing ()

                result
        )

        if wereAllRemoved then
            myUserMessages.RegisterMessage WARNING_NO_MATCH_FOUND

    member private this.RemoveFulfillingMultiLine regexObject predicate =
        let mutable wasAnyRemoved = false

        let wereAllRemoved = mySelections.RemoveAll (
            fun selection ->
                let first     = selection.first
                let last      = selection.last
                let rightKept = myAccessor.GetNextChar last

                let mlr = MultiLineRegex.IsMatch (
                    myUserMessages, regexObject
                )
                this.ProcessText first rightKept mlr.ProcessLine |> ignore
                mlr.FinishProcessing ()

                let isToRemove = predicate mlr
                wasAnyRemoved <- wasAnyRemoved || isToRemove
                isToRemove
        )

        if not wasAnyRemoved then
            myUserMessages.RegisterMessage INFO_NO_SELECTIONS_DISCARDED
        elif wereAllRemoved then
            myUserMessages.RegisterMessage WARNING_ALL_SELECTIONS_DISCARDED

    // StoreToRegister, LoadFromRegister

    member private _.Store r =
        mySelectionsRegisters.Set r mySelections.Items mySelections.MainIndex

    member private _.Load r =
        match mySelectionsRegisters.Get r with
        | Some (selections, mainIndex) ->
            mySelections.SetFrom selections mainIndex
        | None ->
            myUserMessages.RegisterMessage (
                formatMessage ERROR_SELECTIONS_REGISTER_IS_EMPTY (getRegisterName r)
            )

    member private _.RemoveStored r =
        mySelectionsRegisters.Remove r
        myUndoProvider.RemoveSelectionsRegister r

    // auxiliary

    member private _.ProcessText (first: Position) (rightKept: Position) f =
        // on single line ?
        if first.line = rightKept.line then
            myAccessor.Process first.line first.char rightKept.char f
        else
               myAccessor.ProcessToEnd first.line first.char f
            || myAccessor.ProcessLines (first.line + 1) rightKept.line f
               // endChars not empty ?
            || if rightKept.char <> 0 then
                   myAccessor.Process rightKept.line 0 rightKept.char f
               else
                   false
