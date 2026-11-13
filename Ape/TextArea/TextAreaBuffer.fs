module TextAreaBuffer

open System

open Commands.InCommands
open Common
open Context
open DataTypes
open ITextAreaBuffer
open MatchRanges
open Position
open Selection
open TextAreaBufferBase
open TextAreaFileReader
open TextAreaFileWriter
open UserMessages
open WrappedRef

type TextAreaBuffer (
    myContextRef:    IWrappedRef<MainContext>,
    myUserMessages:  UserMessages,
    myRegisters:     Registers.Registers,
    inFilePath:      string,
    myLinesFromFile: Lines,
    inDiskFileState: FileUtils.DiskFileState
) as thisCtor =
    inherit TextAreaBufferBase (
        myContextRef, myUserMessages, myRegisters, inFilePath,
        myLinesFromFile,
        MatchRanges (
            myUserMessages, myLinesFromFile
        )
    )

    let myFileReader = new TextAreaFileReader (
        myContextRef, myUserMessages, myLinesFromFile
    )

    let mutable myDiskFileState = inDiskFileState
    
    let myMatchRanges = thisCtor.MatchRanges

    // registered children (extract buffers)
    let myChildren = ResizeArray<ITextAreaBuffer> ()

    // only for testing purposes
    member _.ReloadFileParams = myFileReader.ReloadFileParams

    // children

    member _.RegisterChild buffer =
        myChildren.Add buffer

    member _.UnregisterChild buffer =
        myChildren.Remove buffer
            |> ignore

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

    override this.ReSearchIfNeeded isInitial =
        if isInitial then
            true
        else
            if myMatchRanges.GetMainGroupCount () = 0 then
                this.ReSearchMatching ()
                true
            else
                false

    override this.ReSearchOrClearMatching () =
        if this.Context.reSearchMatching then
            myMatchRanges.UpdateAfterChange ()
        else
            myMatchRanges.ClearSearchAll ()

    // Undo/Redo

    member this.UndoCorruptedState () =
        this.Dispatcher.UndoRedoPerformer.UndoCorruptedState ()

        this.HasUndoToRegister      <- false
        this.HasUndoLinesToRegister <- false

        myMatchRanges.ClearSearchAll ()

        this.Dispatcher.ApplyChangedLinesCountIfNeeded ()
        this.Dispatcher.DisplayRenderer.ResetLinesCache ()

    // others

    member this.IsDiskFileChanged =
        TextAreaFileWriter.IsDiskFileChanged this.FilePath myDiskFileState

    member this.LoadStrings (lines: string seq) =
        myFileReader.LoadStrings lines (
            fun () ->
                this.ResetState Position_Zero
                this.ResetUndoState ()
        )

    member this.LoadFile encoding strictEncoding =
        myFileReader.LoadFile this.FilePath encoding strictEncoding (
            fun () ->
                this.ResetState Position_Zero
                this.ResetUndoState ()
        )
        |> Result.map (
            fun (fileFormat, endsWithNewLine) ->
                myDiskFileState <- TextAreaFileWriter.GetDiskFileState this.FilePath
                (fileFormat, endsWithNewLine)
        )

    member this.Reload encoding strictEncoding =
        let cursor      = this.Main.Cursor
        let cursorWC    = this.Main.CursorWC
        let displayLine = this.DisplayPos.line

        myFileReader.ReloadFile this.FilePath encoding strictEncoding (
            fun () ->
                this.ResetStateAfterReload cursor cursorWC displayLine
                this.ResetUndoState ()
        )
        |> Result.map (
            fun (fileFormat, endsWithNewLine) ->
                myDiskFileState <- TextAreaFileWriter.GetDiskFileState this.FilePath
                (fileFormat, endsWithNewLine)
        )

    // auxiliary

    member private this.ResetState cursor =
        this.Selections.Clear ()
        this.Selections.Add {
            Selection_Zero with first = cursor; last = cursor
        }
        this.WantedColumns.SetPendingWCActions [
        ]

        this.SelsRegisters.Clear ()

        myMatchRanges.ClearSearchAll ()

        this.DisplayPos <- DisplayPos_Zero

        this.PrevCommand <- None

        this.Dispatcher.ApplyChangedLinesCountIfNeeded ()
        this.Dispatcher.DisplayRenderer.ResetLinesCache ()

        this.IsBufferChanged <- false

    member private this.ResetStateAfterReload cursor cursorWC displayLine =
        // Remember any message from failed reload.
        let userMessage = myUserMessages.RetrieveMessage ()

        myMatchRanges.UpdateAfterChange ()

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

        member this.Lines                  = myLinesFromFile
        member this.LinesForCompletion     = myLinesFromFile
        member this.Selections             = this.Selections
        member this.IsReadOnly             = this.IsReadOnly
        member this.IsBufferChanged        = Some this.IsBufferChanged
        member this.IsDiskFileChanged      = Some this.IsDiskFileChanged
        member this.HasUndoToRegister      = this.HasUndoToRegister
        member this.HasUndoLinesToRegister = this.HasUndoLinesToRegister
        member this.IsSndSearch            = myMatchRanges.IsSndSearch

        member this.StatusChar =
            if this.IsReadOnly then
                if this.IsBufferChanged then '+' else '-'
            else
                if this.IsBufferChanged then '*' else ' '
        
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
        member this.ToggleSndSearch ()     = myMatchRanges.ToggleSndSearch ()

        // Undo/Redo

        member this.RegisterUndo isStateVolatile = this.RegisterUndo isStateVolatile
        member this.ClearIsLastUndoVolatile ()   = this.ClearIsLastUndoVolatile ()
        member this.UndoCorruptedState ()        = this.UndoCorruptedState ()

        // others

        member this.Reload encoding strictEncoding =
            this.Reload encoding strictEncoding

        member this.WriteFile filePath encoding fileFormat endWithNewLine =
            this.WriteFile filePath encoding fileFormat endWithNewLine
            |> Result.map (
                fun () ->
                    myDiskFileState <- TextAreaFileWriter.GetDiskFileState this.FilePath
                    ()
            )
            
        member _.GetFirstChild () =
            if myChildren.Count <> 0 then
                Some myChildren[0]
            else
                None

    // IDisposable

    override _.Dispose () =
        (myFileReader :> IDisposable).Dispose ()
        base.Dispose ()

let makeTextAreaBuffer (
    contextRef:   IWrappedRef<MainContext>,
    userMessages: UserMessages,
    registers:    Registers.Registers,
    filePath:     string
) =
    new TextAreaBuffer (
        contextRef,
        userMessages,
        registers,
        filePath,
        Lines Chars.Empty,
        FileUtils.DiskFileState_Default filePath
    )
