module Selections

open System.Collections.Generic
open System.Collections.Immutable

open BinarySearch
open Position
open Selection
open TextRange

type private UpdateFun  = Selection -> Selection

type private ReplaceFun = Selection -> ResizeArray<Selection>

/// Selections holds a list of a buffer's current selections, provides information
/// related to them and performs various operations on them, including updating and
/// replacing of individual selections.

type Selections (mySelections: ResizeArray<Selection>) =

    let mutable myMain = 0

    do
        mySelections.Sort ()

    /// Clears selections repository.
    member _.Clear () =
        mySelections.Clear ()

    /// Adds given selection into selections repository keeping it sorted.
    /// Sets the added selection as the main selection.
    member _.Add selection =
        let index =
            getIndexForInsertIntoSortedArray (compareTo selection) mySelections
        mySelections.Insert (index, selection)
        myMain <- index

    /// Clears selections repository and adds given selections into it. Sets selection
    /// with mainIndex as the main selection. If mainIndex = -1, then the last selection
    /// is set as the main selection.
    member _.SetFrom (selections: IEnumerable<Selection>) mainIndex =
        mySelections.Clear ()
        mySelections.AddRange selections

        if mainIndex <> -1 then
            myMain <- mainIndex
        else
            myMain <- mySelections.Count - 1

    // methods used only for non-empty selections repository

    /// Returns count of selections in selections repository.
    member _.Count = mySelections.Count

    /// Returns all selections in selections repository.
    member _.Items = mySelections

    /// Accesses selections repository by the index of selection.
    member _.Item
        with get index       = mySelections[index]
        and  set index value = mySelections[index] <- value

    /// Returns an immutable copy of selections repository.
    member _.GetImmutable () =
        mySelections |> ImmutableArray.CreateRange

    /// Returns the main selection index.
    member _.MainIndex =
        myMain

    /// Returns and sets the main selection.
    member _.Main
        with get ()    = mySelections[myMain]
        and  set value = mySelections[myMain] <- value

    /// Removes the main selection and sets the next selection as the main selection.
    member _.RemoveMain () =
        mySelections.RemoveAt myMain
        if not (myMain < mySelections.Count) then
            myMain <- 0

    /// Removes all selections except the main selection.
    member this.KeepOnlyMain () =
        let mainBackup = this.Main
        mySelections.Clear ()
        this.Add mainBackup

    /// Removes all selections fulfilling predicate.
    /// Returns true if all selections were removed.
    member this.RemoveAll predicate =
        this.PerformActionOnSelections (
            fun () -> mySelections.RemoveAll predicate |> ignore
        )

    /// Sorts selections repository.
    member this.Sort () =
        this.PerformActionOnSelections (
            fun () -> mySelections.Sort ()
        ) |> ignore

    /// Returns true if any selection is positioned after EOF.
    member _.IsAnyAfterEof lastEolPosition =
        mySelections |> Seq.exists (
            fun selection ->
                selection.first > lastEolPosition ||
                selection.last  > lastEolPosition
        )

    member private this.PerformActionOnSelections action =
        let mainBackup = mySelections[myMain]
        let mainOrder  = this.GetSelectionOrder myMain mainBackup

        action ()

        let firstEqOrGtOrLastLtMainIndex =
            findFirstEqOrGtOrLastLtInSortedArray (compareTo mainBackup) mySelections

        let mutable wereAllRemoved = false

        match firstEqOrGtOrLastLtMainIndex with
        | m, Some 0 ->
            myMain <- m + mainOrder
        | m, Some _ ->
            myMain <- m
        | _, None   ->
            this.Add (mainBackup.ReduceToCursor ())
            wereAllRemoved <- true

        wereAllRemoved

    member private _.GetSelectionOrder index selection =
        mySelections
        |> Seq.take index
        |> Seq.filter (fun x -> x = selection)
        |> Seq.length

    /// Sets selection previous to the main as the main selection.
    /// Returns true if rotate hit the top, false otherwise.
    member _.RotateUp () =
        if myMain > 0 then
            myMain <- myMain - 1
            false
        else
            myMain <- mySelections.Count - 1
            true

    /// Sets selection next to the main as the main selection.
    /// Returns true if rotate hit the bottom, false otherwise.
    member _.RotateDown () =
        if myMain < mySelections.Count - 1 then
            myMain <- myMain + 1
            false
        else
            myMain <- 0
            true

    /// Returns the first and last position of the first and last selection, respectively.
    member _.GetSelectionsSpan () =
        let first = mySelections[0].first
        let last  = mySelections[mySelections.Count - 1].last

        (first, last)

    /// Returns sorted selected ranges. Selected ranges don't overlap
    /// and those that touch each other are not joined together.
    member _.GetSelectedRanges () =
        let ranges = ResizeArray<TextRange> ()

        for selection in mySelections do
            TextRanges.incorporateRange ranges {
                first = selection.first; last = selection.last
            }

        ranges

    /// Returns sorted selected ranges and sorted cursors for lines in interval
    /// <startLine, endLine>. Selected ranges don't overlap and those that touch each other
    /// are not joined together. Cursors on the same position are not merged into one.
    member _.GetSelectedRangesAndCursors startLine endLine =
        let cursors = ResizeArray<Position> ()
        let ranges  = ResizeArray<TextRange> ()

        let selections = mySelections |> Seq.filter (
            fun x -> x.last.line >= startLine && x.first.line <= endLine
        )

        for selection in selections do
            TextRanges.incorporateRange ranges {
                first = selection.first; last = selection.last
            }

            cursors.Add selection.Cursor

        cursors.Sort ()

        let mainCursor = mySelections[myMain].Cursor

        (mainCursor, cursors, ranges)

    /// Applies f on each selection and replaces it by returned selection.
    member _.UpdateSelections (f: UpdateFun)  =
        for i = 0 to mySelections.Count - 1 do
            mySelections[i] <- f mySelections[i]

    /// Applies f on each selection and replaces it by returned selections.
    member this.ReplaceSelections (f: ReplaceFun) =
        let mainBackup = this.Main

        let mutable i = 0

        while i < mySelections.Count do
            let selection = mySelections[i]
            let isForward = selection.isForward
            let newSelections = f selection

            mySelections.RemoveAt i
            mySelections.InsertRange (i, newSelections)

            if (not isForward && i < myMain) || (isForward && i <= myMain) then
                myMain <- myMain + (max 0 (newSelections.Count - 1))

            i <- i + newSelections.Count

        let mutable wereAllRemoved = false

        if mySelections.Count = 0 then
            this.Add (mainBackup.ReduceToCursor ())
            wereAllRemoved <- true

        // Were the main selection and all subsequent selections removed ?
        myMain <- min (mySelections.Count - 1) myMain

        wereAllRemoved
