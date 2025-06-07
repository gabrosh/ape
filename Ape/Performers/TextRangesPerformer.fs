module TextRangesPerformer

open System

open Commands.InCommands
open Commands.OutCommands
open Common
open Context
open DataTypes
open LinesAccessor
open ModifyingAuxiliary
open TextRange
open WrappedRef

let private TextRangeIndex_Invalid = IntType.MaxValue

type TextRangesCommandInState = {
    textRangeIndex: int
    textRange:      TextRange
}

type TextRangesCommandOutState = {
    isLinesModified: bool
}

/// TextRangesPerformer is performer for commands modifying the buffer through TextRanges.
/// It performs one command on a single TextRange at a time.
/// In addition, it provides input and output WantedColumnsActions for the command
/// to be performed.

[<Sealed>]
type TextRangesPerformer (
    myContextRef: IWrappedRef<AreaContext>,
    myLines:      Lines
) =
    let mutable myContext = myContextRef.Value
    let handleContextChanged () = myContext <- myContextRef.Value
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    // set by PerformCommand, changed by the command
    let mutable myTextRange      = TextRange_Invalid
    // set by PerformCommand, not changed by the command
    let mutable myTextRangeIndex = TextRangeIndex_Invalid

    let myAccessor = LinesAccessor myLines

    // commands

    /// Performs command on given state of the performer. Returns new state
    /// of the performer and OutCommand to be performed on the caller side.
    member this.PerformCommand (state: TextRangesCommandInState) command =
        myTextRangeIndex <- state.textRangeIndex
        myTextRange      <- state.textRange

        myAccessor.ClearFlags ()

        let outCommands =
            match command with
            | FillWithChar c -> this.FillWithChar c
            | ToUppercase    -> this.ToUppercase  ()
            | InvertCase     -> this.InvertCase   ()
            | TabsToSpaces   -> this.TabsToSpaces ()
            | SpacesToTabs   -> this.SpacesToTabs ()
            | Indent         -> this.Indent       ()
            | Unindent       -> this.Unindent     ()
            | RegexEscape    -> this.RegexEscape  ()

        let outState: TextRangesCommandOutState = {
            isLinesModified = myAccessor.IsLinesModified
        }

        (outState, outCommands)

    /// Returns wanted columns actions to be performed before and after given command.
    member _.GetWantedColumnsActions _command =
        []

        ,

        // This is needed also for FillWithChar, ToUppercase and Invercase
        // as StartApplyingModifications in TextAreaTextRanges class, which clears
        // the selections' wanted columns, is called for all TextRanges commands.
        [SetHardWantedColumn; SetSoftWantedColumn]

    // FillWithChar, ToUppercase, InvertCase, ...

    member private this.FillWithChar c =
        let first     = myTextRange.first
        let last      = myTextRange.last
        let rightKept = myAccessor.GetNextChar last

        this.TransformText first rightKept (fillWithChar c)

    member private this.ToUppercase () =
        let first     = myTextRange.first
        let last      = myTextRange.last
        let rightKept = myAccessor.GetNextChar last

        this.TransformText first rightKept toUppercase

    member private this.InvertCase () =
        let first     = myTextRange.first
        let last      = myTextRange.last
        let rightKept = myAccessor.GetNextChar last

        this.TransformText first rightKept invertCase

    member private this.TabsToSpaces () =
        let first     = myTextRange.first
        let last      = myTextRange.last
        let rightKept = myAccessor.GetNextChar last

        this.TransformText first rightKept (tabsToSpaces myContext.tabStop)

    member private this.SpacesToTabs () =
        let first     = myTextRange.first
        let last      = myTextRange.last
        let rightKept = myAccessor.GetNextChar last

        this.TransformText first rightKept (spacesToTabs myContext.tabStop)

    member private this.RegexEscape () =
        let first     = myTextRange.first
        let last      = myTextRange.last
        let rightKept = myAccessor.GetNextChar last

        this.TransformText first rightKept regexEscape

    member private this.Indent () =
        let first     = myTextRange.first
        let last      = myTextRange.last

        this.TransformTextByLines first last (indent myContext.tabStop myContext.tabBySpaces)

    member private this.Unindent () =
        let first     = myTextRange.first
        let last      = myTextRange.last

        this.TransformTextByLines first last (unindent myContext.tabStop)

    // auxiliary

    member private _.TransformText first rightKept f =
        let outCommands = ResizeArray<ModifyingOutCommand> ()

        // On single line ?
        if first.line = rightKept.line then
            myAccessor.Transform first.line first.char rightKept.char f
                |> outCommands.AddRange
        else
            myAccessor.TransformToEnd first.line first.char f
                |> outCommands.AddRange
            myAccessor.TransformLines (first.line + 1) rightKept.line f
                |> outCommands.AddRange
            // endChars not empty ?
            if rightKept.char <> 0 then
                myAccessor.Transform rightKept.line 0 rightKept.char f
                    |> outCommands.AddRange
            else
                ()

        outCommands |> List.ofSeq

    member private _.TransformTextByLines first last f =
        let outCommands = ResizeArray<ModifyingOutCommand> ()

        myAccessor.TransformLines first.line (last.line + 1) f
            |> outCommands.AddRange

        outCommands |> List.ofSeq

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable.Dispose ()
