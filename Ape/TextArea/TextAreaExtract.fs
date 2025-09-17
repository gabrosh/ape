﻿module TextAreaExtract

open Commands.InCommands
open Common
open Context
open DataTypes
open ITextAreaBuffer
open MatchRangesExtract
open Position
open Selection
open TextAreaBufferBase
open UserMessages
open WrappedRef

type TextAreaExtract (
    myContextRef:    IWrappedRef<MainContext>,
    myUserMessages:  UserMessages,
    myRegisters:     Registers.Registers,
    inFilePath:      string,
    myLinesFromFile: Lines,
    myLinesExtract:  Lines
) as thisCtor =
    inherit TextAreaBufferBase (
        myContextRef, myUserMessages, myRegisters, inFilePath,
        myLinesFromFile,
        myLinesExtract,
        MatchRangesExtract (
            myUserMessages, myLinesFromFile, myLinesExtract
        )
    )

    let myMatchRanges = thisCtor.MatchRanges :?> MatchRangesExtract

    // search matching

    member this.SearchMatching regex isForward isExtending =
        let isInitial = (
               myMatchRanges.LastRegex <> Some regex
            || myMatchRanges.GetMainGroupCount () = 0
        )

        myMatchRanges.Search regex

        if not myUserMessages.HasErrorMessage then
            let command =
                if isForward then
                    CommonCommand (CursorToNextMatch isInitial)
                else
                    CommonCommand (CursorToPrevMatch isInitial)

            this.PerformCommand true isExtending command 1

    member _.ReSearchMatching () =
        myMatchRanges.ReSearch ()

    member _.ClearSearchMatching () =
        myMatchRanges.ClearSearch ()

    member this.ExtractMatching regex =
        this.WrapExtractAction (
            fun () -> myMatchRanges.Extract regex
        )

    member this.ReExtractMatching () =
        this.WrapExtractAction (
            fun () -> myMatchRanges.ReExtract ()
        )

    member this.ClearExtractMatching () =
        this.WrapExtractAction (
            fun () -> myMatchRanges.ClearExtract ()
        )

    member private this.WrapExtractAction action =
        let lineCurrent = this.Selections.Main.Cursor.line

        let lineOrig =
            if myMatchRanges.IsClearedExtract then
                lineCurrent
            else
                myMatchRanges.LineExtractToLine lineCurrent

        action ()

        let lineNew =
            if myMatchRanges.IsClearedExtract then
                lineOrig
            else
                myMatchRanges.LineToLineExtract lineOrig

        let cursorNew = { line = lineNew; char = 0 }

        this.ResetState cursorNew

        let command = WrapLinesDepCommand CenterVertically
        this.PerformCommand true false command 1

        this.ResetUndoState ()

    override this.ReSearchIfNeeded isInitial =
        if isInitial then
            true
        else
            if myMatchRanges.GetMainGroupCount () = 0 then
                this.ReSearchMatching ()
                true
            else
                false

    override _.ReSearchOrClearMatching () =
        ()

    // Undo/Redo

    member this.UndoCorruptedState () =
        myMatchRanges.ClearSearch ()
        myMatchRanges.ClearExtract ()

        let cursorNew = Position_Zero

        this.ResetState cursorNew
        this.ResetUndoState ()

    // others

    member this.LoadStrings (lines: string seq) =
        this.FileSupport.LoadStrings lines (
            fun () ->
                myMatchRanges.UpdateAfterReload ()
                this.ResetState Position_Zero
                this.ResetUndoState ()
        )

    member this.LoadFile encoding strictEncoding =
        this.FileSupport.LoadFile this.FilePath encoding strictEncoding (
            fun () ->
                myMatchRanges.UpdateAfterReload ()
                this.ResetState Position_Zero
                this.ResetUndoState ()
        )

    member this.Reload encoding strictEncoding warnIfNoMatchFound =
        let cursor      = this.Main.Cursor
        let cursorWC    = this.Main.CursorWC
        let displayLine = this.DisplayPos.line

        this.FileSupport.ReloadFile this.FilePath encoding strictEncoding (
            fun () ->
                myMatchRanges.RunWithSetWarnIfNoMatchFound warnIfNoMatchFound (
                    fun () -> this.ResetStateAfterReload cursor cursorWC displayLine
                )

                this.ResetUndoState ()
        )

    // auxiliary

    member private this.ResetState cursor =
        this.Selections.Clear ()
        this.Selections.Add {
            Selection_Zero with first = cursor; last = cursor
        }
        this.WantedColumns.SetPendingWCActions [
            SetHardWantedColumn; SetSoftWantedColumn
        ]

        this.SelsRegisters.Clear ()

        this.DisplayPos <- DisplayPos_Zero

        this.PrevCommand <- None

        this.Dispatcher.ApplyChangedLinesCountIfNeeded ()
        this.Dispatcher.DisplayRenderer.ResetLinesCache ()

        this.IsBufferChanged <- false

    member private this.ResetStateAfterReload cursor cursorWC displayLine =
        // Remember any message from failed reload.
        let userMessage = myUserMessages.RetrieveMessage ()

        myMatchRanges.UpdateAfterReload ()

        match userMessage with
        | Some userMessage ->
            // Re-register the message from failed reload.
            myUserMessages.RetrieveMessage () |> ignore
            myUserMessages.RegisterMessage userMessage
        | None ->
            ()

        this.Selections.Clear ()
        let newCursor = this.GetValidCursorPos cursor
        this.Selections.Add {
            Selection_Zero with first   = newCursor ; last   = newCursor
                                firstWC = cursorWC  ; lastWC = cursorWC
        }

        this.SelsRegisters.Clear ()

        let newDisplayLine = this.GetValidDisplayLine displayLine

        this.DisplayPos <- {
            DisplayPos_Zero with line = newDisplayLine
        }

        this.PrevCommand <- None

        this.Dispatcher.ApplyChangedLinesCountIfNeeded ()
        this.Dispatcher.DisplayRenderer.ResetLinesCache ()

        this.IsBufferChanged <- false

    // ITextAreaBuffer

    interface ITextAreaBuffer with

        // properties

        member this.FilePath
            with get ()    = this.FilePath
            and  set value = this.FilePath <- value

        member this.BufferName
            with get ()    = this.BufferName
            and  set value = this.BufferName <- value

        member this.Lines                  = myLinesExtract
        member this.LinesForCompletion     = myLinesFromFile
        member this.Selections             = this.Selections
        member this.IsReadOnly             = this.IsReadOnly
        member this.IsBufferChanged        = this.IsBufferChanged
        member this.HasUndoToRegister      = this.HasUndoToRegister
        member this.HasUndoLinesToRegister = this.HasUndoLinesToRegister

        member this.StatusChar =
            if myMatchRanges.IsClearedExtract then 'o' else '.'
                        
        // commands

        member this.PerformCommand isNormalMode isExtending command count =
            this.PerformCommand isNormalMode isExtending command count

        // rendering

        member this.GetDisplayRows ()            = this.GetDisplayRows ()
        member this.GetCursorPosForStatusArea () = this.GetCursorPosForStatusArea ()

        // search matching

        member this.SearchMatching regex isForward isExtending =
            this.SearchMatching regex isForward isExtending

        member this.ReSearchMatching ()    = this.ReSearchMatching ()
        member this.ClearSearchMatching () = this.ClearSearchMatching ()

        // Undo/Redo

        member this.RegisterUndo isStateVolatile = this.RegisterUndo isStateVolatile
        member this.ClearIsLastUndoVolatile ()   = this.ClearIsLastUndoVolatile ()
        member this.UndoCorruptedState ()        = this.UndoCorruptedState ()

        // others

        member this.Reload encoding strictEncoding warnIfNoMatchFound =
            this.Reload encoding strictEncoding warnIfNoMatchFound

        member this.WriteFile filePath encoding fileFormat endWithNewLine =
            this.WriteFile filePath encoding fileFormat endWithNewLine

        member _.GetFirstChild () =
            None

    // IDisposable

    override _.Dispose () =
        base.Dispose ()

let makeTextAreaExtract (
    contextRef:   IWrappedRef<MainContext>,
    userMessages: UserMessages,
    registers:    Registers.Registers,
    filePath:     string
) =
    new TextAreaExtract (
        contextRef,
        userMessages,
        registers,
        filePath,
        Lines [Chars.Empty],
        Lines []              // lines added in MatchRangesExtract constructor
    )
