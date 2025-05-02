module PromptDispatcher

open System

open Commands.InCommands
open Common
open Context
open DataTypes
open UserMessages
open WrappedRef

open CommonPerformer
open ModifyingPerformer
open NoWrapLinesPerformer
open SelectionsPerformer
open TextRangesPerformer
open UndoRedoPerformer
open WrapLinesPerformer

let private castWrapLinesDepPerformer performer =
    performer :> ICommandsPerformer<WrapLinesDepCommand> ,
    performer :> IDisplayRenderer

let private getWrapLinesDepInterfaces (contextRef: IWrappedRef<AreaContext>) lines =
    if contextRef.Value.wrapLines then
        castWrapLinesDepPerformer (new WrapLinesPerformer   (contextRef, lines))
    else
        castWrapLinesDepPerformer (new NoWrapLinesPerformer (contextRef, lines))

[<Sealed>]
type PromptDispatcher (
    myContextRef:      IWrappedRef<MainContext>,
    myExtraContextRef: IWrappedRef<ExtraContext>,
    inUserMessages:    UserMessages,
    myLines:           Lines,
    inRegisters:       Registers.Registers,
    inSelections:      Selections.Selections,
    inSelsRegisters:   SelectionsRegisters.SelectionsRegisters,
    inWantedColumns:   Helpers.WantedColumns,
    inMatchRanges:     IMatchRanges.IMatchRanges,
    inUndoProvider:    UndoProvider.UndoProvider
) =
    let myAreaContextRef = WrappedRef (
        makePromptContext myContextRef.Value myExtraContextRef.Value
    )

    let myCommonPerformer = new CommonPerformer (
        myAreaContextRef, inUserMessages, myLines, inMatchRanges
    )
    let myModifyingPerformer = new ModifyingPerformer (
        myAreaContextRef, myLines, inRegisters
    )
    let myTextRangesPerformer = new TextRangesPerformer (
        myAreaContextRef, myLines
    )
    let mySelectionsPerformer = SelectionsPerformer (
        inUserMessages, myLines, inSelections, inSelsRegisters, inUndoProvider
    )
    let myUndoRedoPerformer = UndoRedoPerformer (
        myLines, inSelections, inSelsRegisters, inWantedColumns, inUndoProvider
    )

    // Both fields hold reference to the same object.
    let mutable myWrapLinesDepPerformer, myDisplayRenderer =
        getWrapLinesDepInterfaces myAreaContextRef myLines

    let handleContextChanged () =
        (myWrapLinesDepPerformer :?> IDisposable).Dispose ()
        myAreaContextRef.Value <-
            makePromptContext myContextRef.Value myExtraContextRef.Value
        let a, b = getWrapLinesDepInterfaces myAreaContextRef myLines
        myWrapLinesDepPerformer <- a
        myDisplayRenderer       <- b

    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    let myExtraContextChangedDisposable =
        myExtraContextRef.Subscribe handleContextChanged

    member _.CommonPerformer       = myCommonPerformer
    member _.ModifyingPerformer    = myModifyingPerformer
    member _.TextRangesPerformer   = myTextRangesPerformer
    member _.SelectionsPerformer   = mySelectionsPerformer
    member _.UndoRedoPerformer     = myUndoRedoPerformer
    member _.WrapLinesDepPerformer = myWrapLinesDepPerformer
    member _.DisplayRenderer       = myDisplayRenderer

    // main functionality

    member _.PerformCommand inState command =
        match command with
        | CommonCommand       x ->
            myCommonPerformer.PerformCommand inState x
        | WrapLinesDepCommand x ->
            myWrapLinesDepPerformer.PerformCommand inState x
        | _                     ->
            invalidOp ""

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable     .Dispose ()
            myExtraContextChangedDisposable.Dispose ()
            (myCommonPerformer       :>  IDisposable).Dispose ()
            (myModifyingPerformer    :>  IDisposable).Dispose ()
            (myTextRangesPerformer   :>  IDisposable).Dispose ()
            (myWrapLinesDepPerformer :?> IDisposable).Dispose ()
