module ModifyingPerformer

open System
open System.Collections.Immutable

open CharCategories
open Commands.InCommands
open Commands.OutCommands
open Common
open Context
open DataTypes
open LinesAccessor
open ModifyingAuxiliary
open Position
open Registers
open Selection
open WrappedRef

let private SelectionIndex_Invalid = IntType.MaxValue

type ModifyingCommandInState = {
    selectionIndex:  int
    isLastSelection: bool
    selection:       Selection
}

type ModifyingCommandOutState = {
    isLinesModified:       bool
    isPossibleEolAfterEof: bool
    toUpdateSelection:     bool
    selection:             Selection
}

/// ModifyingPerformer is performer for commands modifying the buffer through
/// selections, Yank command, and several other commands for entering InsertMode,
/// which may or doesn't modify the buffer.
/// It performs one command on a single selection at a time.
/// If needed, it prepares a register for the command to be performed and/or publishes
/// a register after performing the command.
/// In addition, it provides input and output WantedColumnsActions for the command
/// to be performed.

[<Sealed>]
type ModifyingPerformer (
    myContextRef: IWrappedRef<AreaContext>,
    myLines:      Lines,
    myRegisters:  Registers
) =
    let mutable myContext = myContextRef.Value
    let handleContextChanged () = myContext <- myContextRef.Value
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    // set by PerformCommand, changed by the command
    let mutable mySelection       = Selection_Invalid
    // set by PerformCommand, not changed by the command
    let mutable mySelectionIndex  = SelectionIndex_Invalid
    let mutable myIsLastSelection = false

    let myAccessor = LinesAccessor myLines

    // commands

    /// Performs command on given state of the performer. Returns new state
    /// of the performer and OutCommand to be performed on the caller side.
    member this.PerformCommand (state: ModifyingCommandInState) command =
        mySelectionIndex  <- state.selectionIndex
        myIsLastSelection <- state.isLastSelection
        mySelection       <- state.selection

        myAccessor.ClearFlags ()

        let outCommands =
            match command with
            | EnterInserting           -> this.EnterInserting      ()
            | EnterAppending           -> this.EnterAppending      ()
            | EnterInsertingAtSol      -> this.EnterInsertingAtSol ()
            | EnterAppendingAtEol      -> this.EnterAppendingAtEol ()
            | InsertChar        c      -> this.InsertChar          c
            | InsertChars       chars  -> this.InsertChars         chars
            | InsertNewLine            -> this.InsertNewLine       ()
            | InsertNewLineIndent      -> this.InsertNewLineIndent ()
            | DeleteChar               -> this.DeleteChar          ()
            | DeletePrevChar           -> this.DeletePrevChar      ()
            | DeletePrevChars   count  -> this.DeletePrevChars     count
            | Yank              r      -> this.Yank                r
            | Delete                   -> this.Delete              ()
            | PasteBeforeAux    (r, p) -> this.PasteBeforeAux      r p
            | AlignSelectionAux column -> this.AlignSelectionAux   column

            | YankAndDelete     _
            | PasteBefore       _
            | PasteAfter        _
            | Replace           _
            | AlignSelections          ->
                // In TextAreaBuffer these commands are split into or replaced by other commands.
                invalidOp ""

        // As the update of mySelection in TextAreaModifying instance is performed
        // only after performing outCommands on all selections, toUpdateSelection = true
        // means that any effects of outCommands on mySelection are eliminated.

        let toUpdateSelection =
            match command with
            | EnterInserting
            | EnterAppending
            | EnterInsertingAtSol
            | EnterAppendingAtEol
            | InsertChar        _
            | InsertChars       _
            | InsertNewLine
            | InsertNewLineIndent
            | PasteBeforeAux    _ -> true

            | DeleteChar
            | DeletePrevChar
            | DeletePrevChars   _
            | Yank              _
            | Delete
            | AlignSelectionAux _ -> false

            | YankAndDelete     _
            | PasteBefore       _
            | PasteAfter        _
            | Replace           _
            | AlignSelections     ->
                // In TextAreaBuffer these commands are split into or replaced by other commands.
                invalidOp ""

        let outState: ModifyingCommandOutState = {
            isLinesModified       = myAccessor.IsLinesModified
            isPossibleEolAfterEof = myAccessor.IsPossibleEolAfterEof
            toUpdateSelection     = toUpdateSelection
            selection             = mySelection
        }

        (outState, outCommands)

    /// Adds EOL after EOF.
    member _.AddEolAfterEof () =
        let lines = Lines [Chars.Empty]
        myAccessor.AppendLines lines

    /// Clears register related to command if required.
    /// Copies the clipboard to register if required.
    member _.PrepareRegister command =
        match command with
        | Yank          r
        | YankAndDelete r ->
            if not (isUpperRegister r) then
                myRegisters.CreateOrClear r
        | PasteBefore   r
        | PasteAfter    r
        | Replace       r ->
            if isClipboardRegister r then
                myRegisters.CopyFromClipboardToRegister r
        | _ ->
            ()

    /// Copies register to the clipboard if required.
    member _.PublishRegister command =
        match command with
        | Yank          r
        | YankAndDelete r ->
            if isClipboardRegister r then
                myRegisters.CopyFromRegisterToClipboard r
        | _ ->
            ()

    /// Returns wanted columns actions to be performed before and after given command.
    member _.GetWantedColumnsActions command =
        []

        ,

        match command with
        | Yank _ ->
            []
        | _      ->
            [SetHardWantedColumn; SetSoftWantedColumn]

    // EnterInserting.../Appending..., InsertChar, InsertNewLine

    member private _.EnterInserting () =
        let first = mySelection.first

        mySelection <- {
            mySelection with first = first; last = first
        }

        []

    member private _.EnterAppending () =
        let last = mySelection.last
        let next = myAccessor.GetNextChar last

        // Assure that cursor is at a valid position.
        if myAccessor.IsAfterEof next then
            myAccessor.AppendLines (Lines [Chars.Empty])

        mySelection <- {
            mySelection with first = next; last = next
        }

        // myLines is modified if next is at EOF, but no other
        // selection will be affected by this modification.
        []

    member private _.EnterInsertingAtSol () =
        let first = mySelection.first
        let first = {
            first with char = 0
        }

        mySelection <- {
            mySelection with first = first; last = first
        }

        []

    member private _.EnterAppendingAtEol () =
        let last = mySelection.last
        let last = {
            last with char = myLines[last.line].Length
        }

        mySelection <- {
            mySelection with first = last; last = last
        }

        []

    member private this.InsertChar c =
        if c = '\t' && myContext.tabBySpaces then
            this.InsertTabBySpaces ()
        else
            this.InsertSingleChar c

    member private this.InsertSingleChar c =
        let target = mySelection.Cursor

        myAccessor.InsertChar target.line target.char c

        TestMines.checkMine (nameof this.InsertChar)

        mySelection <- {
            mySelection with first = target; last = target
        }

        [
            ApplyInsert {
                target     = target
                startChars = 1
                newLines   = 0
                endChars   = 0
                preferMove = true
            }
        ]

    member private this.InsertTabBySpaces () =
        let target = mySelection.first

        let delta =
            getSpacesForTab myContext.tabStop myLines[target.line] target.char

        let lines = Lines [
            ImmutableArray.CreateRange (
                seq { for _ = 1 to delta do Utils.charSpace }
            )
        ]

        this.DoInsert target lines

        let insertSpec = myAccessor.GetInsertSpec target lines true
        let movedTarget = applyInsertToPos insertSpec target
        let newLast = myAccessor.GetPrevChar movedTarget

        mySelection <- {
            mySelection with first = target; last = newLast
        }

        [ApplyInsert insertSpec]

    member private _.InsertChars chars =
        if chars.Length <> 0 then
            let target = mySelection.Cursor

            myAccessor.Insert target.line target.char chars

            let insertSpec = {
                target     = target
                startChars = chars.Length
                newLines   = 0
                endChars   = 0
                preferMove = true
            }

            let movedTarget = applyInsertToPos insertSpec target
            let newLast = myAccessor.GetPrevChar movedTarget

            mySelection <- {
                mySelection with first = target; last = newLast
            }

            [ApplyInsert insertSpec]
        else
            []

    member private _.InsertNewLine () =
        let target = mySelection.Cursor

        myAccessor.SplitLine target.line target.char

        mySelection <- {
            mySelection with first = target; last = target
        }

        [
            ApplyInsert {
                target     = target
                startChars = 0
                newLines   = 1
                endChars   = 0
                preferMove = true
            }
        ]

    member private _.InsertNewLineIndent () =
        let target = mySelection.Cursor

        let endChars = myAccessor.SplitLineIndent target.line target.char

        let insertSpec: Position.InsertSpec = {
            target     = target
            startChars = 0
            newLines   = 1
            endChars   = endChars
            preferMove = true
        }

        let movedTarget = applyInsertToPos insertSpec target
        let newLast = myAccessor.GetPrevChar movedTarget

        mySelection <- {
            mySelection with first = target; last = newLast
        }

        [ApplyInsert insertSpec]

    // DeleteChar, DeletePrevChar

    member private this.DeleteChar () =
        if not (mySelection.first = mySelection.last) then
            invalidOp ""

        let first = mySelection.first
        let deleteSpec = myAccessor.GetDeleteSpec first first
        this.DoDelete deleteSpec.first deleteSpec.rightKept

        TestMines.checkMine (nameof this.DeleteChar)

        [ApplyDelete deleteSpec]

    member private this.DeletePrevChar () =
        if not (mySelection.first = mySelection.last) then
            invalidOp ""

        let first = mySelection.first

        if not (myAccessor.IsAtSof first) then
            let first = myAccessor.GetPrevChar first
            let deleteSpec = myAccessor.GetDeleteSpec first first
            this.DoDelete deleteSpec.first deleteSpec.rightKept

            TestMines.checkMine (nameof this.DeletePrevChar)

            [ApplyDelete deleteSpec]
        else
            []

    member private this.DeletePrevChars count =
        if not (mySelection.first = mySelection.last) then
            invalidOp ""

        let rigthKept = mySelection.first
        let first = rigthKept.Sub 0 count
        let last  = rigthKept.Sub 0 1
        let deleteSpec = myAccessor.GetDeleteSpec first last
        this.DoDelete deleteSpec.first deleteSpec.rightKept

        [ApplyDelete deleteSpec]

    // Yank, Delete

    member private this.Yank register =
        let first     = mySelection.first
        let last      = mySelection.last
        let rightKept = myAccessor.GetNextChar last

        this.DoYank first rightKept register mySelectionIndex

        []

    member private this.Delete () =
        let first = mySelection.first
        let last  = mySelection.last
        let deleteSpec = myAccessor.GetDeleteSpec first last
        this.DoDelete deleteSpec.first deleteSpec.rightKept

        [ApplyDelete deleteSpec]

    // PasteBeforeAux

    member private this.PasteBeforeAux register toPrompt =
        let lines =
            myRegisters.GetSlot register mySelectionIndex
        let lines =
            if toPrompt then
                getPastableToPrompt lines
            else
                lines

        match lines with
        | Some lines ->
            let target = mySelection.first

            this.DoInsert target lines

            let insertSpec = myAccessor.GetInsertSpec target lines true
            let movedTarget = applyInsertToPos insertSpec target
            let newLast = myAccessor.GetPrevChar movedTarget

            mySelection <- {
                mySelection with first = target; last = newLast
            }

            [ApplyInsert insertSpec]
        | None ->
            let target = mySelection.first

            mySelection <- {
                mySelection with first = target; last = target
            }

            []

    // AlignSelectionAux

    member private this.AlignSelectionAux delta =
        if delta > 0 then
            let target = mySelection.first

            let lines = Lines [
                ImmutableArray.CreateRange (
                    seq { for _ = 1 to delta do Utils.charSpace }
                )
            ]

            this.DoInsert target lines

            let insertSpec = myAccessor.GetInsertSpec target lines true

            [ApplyInsert insertSpec]
        else
            []

    // auxiliary

    member private _.DoYank first rightKept register index =
        let lines = Lines ()

        // on single line ?
        if first.line = rightKept.line then
            myAccessor.Yank first.line first.char rightKept.char
                |> lines.Add
        else
            myAccessor.YankToEnd first.line first.char
                |> lines.Add
            myAccessor.YankLines (first.line + 1) rightKept.line
                |> lines.AddRange
            // Is rightKept.line before EOF ?
            if rightKept.line < myLines.Count then
                myAccessor.Yank rightKept.line 0 rightKept.char
                    |> lines.Add
            else
                myAccessor.YankEmpty ()
                    |> lines.Add

        myRegisters.ApplyToSlot register index lines

    member private _.DoDelete first rightKept =
        // on single line ?
        if first.line = rightKept.line then
            myAccessor.Remove first.line first.char rightKept.char
        else
            myAccessor.RemoveToEnd first.line first.char
            myAccessor.RemoveLines (first.line + 1) rightKept.line
            // Was rightKept.line before EOF ?
            if first.line + 1 < myLines.Count then
                myAccessor.Remove (first.line + 1) 0 rightKept.char
                myAccessor.CombineTwoLines first.line

    member private this.DoInsert target lines =
        if myAccessor.IsAfterEof target then
            // Don't add possibly unnecessary EOL at the end of file.
            // It may be added later in AddEolAfterEofIfNeeded.
            let lines' =
                if hasLastEmptyLine lines then
                    trimLastLine lines
                else
                    lines
            myAccessor.IsPossibleEolAfterEof <- true
            this.DoInsertAfterEof target lines'

        elif myIsLastSelection && myAccessor.IsAtLastEol target then
            // Don't add possibly unnecessary EOL at the end of file.
            // It may be added later in AddEolAfterEofIfNeeded.
            let lines' =
                if hasLastEmptyLine lines then
                    myAccessor.IsPossibleEolAfterEof <- true
                    trimLastLine lines
                else
                    lines
            this.DoInsertBeforeEof target lines'

        else
            this.DoInsertBeforeEof target lines

    member private _.DoInsertBeforeEof target lines =
        let targetLine = target.line
        let targetChar = target.char
        let linesCount = lines.Count

        // on single line ?
        if linesCount = 1 then
            myAccessor.Insert targetLine targetChar lines[0]
        else
            myAccessor.SplitLine targetLine targetChar
            myAccessor.Append targetLine lines[0]
            myAccessor.Insert (targetLine + 1) 0 lines[linesCount - 1]
            // Are there any middle lines ?
            if linesCount > 2 then
                let what = lines.GetRange (1, linesCount - 2)
                myAccessor.InsertLines (targetLine + 1) what

    member private _.DoInsertAfterEof _target lines =
        myAccessor.AppendLines lines

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable.Dispose ()
