namespace Helpers

open Common
open Selections

/// WantedColumns registers output wanted columns actions and performs input
/// wanted columns actions requested by various commands performers. Possible
/// actions are either SetHardWantedColumn, SetSoftWantedColumn or both.

type WantedColumns (mySelections: Selections) =

    let mutable myPendingWCActions = Set<WantedColumnsAction> []

    /// Returns pending wanted columns actions.
    member _.GetPendingWCActions () =
        myPendingWCActions |> Set.toList

    /// Sets pending wanted columns actions.
    member _.SetPendingWCActions actions =
        myPendingWCActions <- Set<WantedColumnsAction> actions

    /// Adds new actions to the pending wanted columns actions.
    member _.RegisterOutputWCActions actions =
        myPendingWCActions <-
            myPendingWCActions |> Set.union (Set actions)

    /// Performs pending wanted columns actions.
    member this.PerformInputWCActions getCharWCFun actions =
        // Get wanted columns actions to perform.
        let atps =
            myPendingWCActions |> Set.intersect (Set actions)

        let f =
            if   atps.Contains SetHardWantedColumn
              && atps.Contains SetSoftWantedColumn then
                Some this.SetBothWantedColumns
            elif atps.Contains SetHardWantedColumn then
                Some this.SetHardWantedColumn
            elif atps.Contains SetSoftWantedColumn then
                Some this.SetSoftWantedColumn
            else
                None

        match f with
        | Some f ->
            for i = 0 to mySelections.Count - 1 do
                mySelections[i] <- f getCharWCFun mySelections[i]
        | None   ->
            ()

        // Unregister performed wanted columns actions.
        myPendingWCActions <-
            atps |> Set.difference myPendingWCActions

    /// Sets hard wanted columns on given selections using getCharWCFun
    /// for getting wanted columns corresponding to given line and char.
    member private _.SetHardWantedColumn getCharWCFun selection =
        let cursor, anchor = selection.Cursor, selection.Anchor

        let cursorHard, _ =
            getCharWCFun cursor.line cursor.char
        let anchorHard, _ =
            getCharWCFun anchor.line anchor.char

        selection.WithCursorAndAnchorWC
            { selection.CursorWC with hard = cursorHard }
            { selection.AnchorWC with hard = anchorHard }

    /// Sets soft wanted columns on given selections using getCharWCFun
    /// for getting wanted columns corresponding to given line and char.
    member private _.SetSoftWantedColumn getCharWCFun selection =
        let cursor, anchor = selection.Cursor, selection.Anchor

        let _, cursorSoft =
            getCharWCFun cursor.line cursor.char
        let _, anchorSoft =
            getCharWCFun anchor.line anchor.char

        selection.WithCursorAndAnchorWC
            { selection.CursorWC with soft = cursorSoft }
            { selection.AnchorWC with soft = anchorSoft }

    /// Sets both wanted columns on given selections using getCharWCFun
    /// for getting wanted columns corresponding to given line and char.
    member private _.SetBothWantedColumns getCharWCFun selection =
        let cursor, anchor = selection.Cursor, selection.Anchor

        let cursorHard, cursorSoft =
            getCharWCFun cursor.line cursor.char
        let anchorHard, anchorSoft =
            getCharWCFun anchor.line anchor.char

        selection.WithCursorAndAnchorWC
            { hard = cursorHard; soft = cursorSoft }
            { hard = anchorHard; soft = anchorSoft }
