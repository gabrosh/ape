module Selection

open DataTypes
open Position
open TextRange
open TextRangesModifier

// WantedColumns

[<Struct>]
type WantedColumns = {
    hard: int
    soft: int
}

let WantedColumns_Zero    = { hard = 0; soft = 0 }
let WantedColumns_Max     = { hard = IntType.MaxValue; soft = IntType.MaxValue }
let WantedColumns_Default = WantedColumns_Zero
let WantedColumns_Invalid = WantedColumns_Max

// Selection

type Selection = {
    first:     Position
    last:      Position
    firstWC:   WantedColumns
    lastWC:    WantedColumns
    isForward: bool
}
with
    member this.Cursor =
        if this.isForward then this.last else this.first

    member this.Anchor =
        if this.isForward then this.first else this.last

    member this.CursorWC =
        if this.isForward then this.lastWC else this.firstWC

    member this.AnchorWC =
        if this.isForward then this.firstWC else this.lastWC

    /// Sets cursor position, preserves anchor position and cursor's and anchor's WCs.
    member this.WithCursor cursor =
        let isForward = (cursor >= this.Anchor)

        if isForward then
            {
                first     = this.Anchor
                last      = cursor
                firstWC   = this.AnchorWC
                lastWC    = this.CursorWC
                isForward = isForward
            }
        else
            {
                first     = cursor
                last      = this.Anchor
                firstWC   = this.CursorWC
                lastWC    = this.AnchorWC
                isForward = isForward
            }

    /// Sets cursor and anchor position, sets anchor's WCs to cursor's WCs.
    member this.WithCursorAndAnchorEqual common =
        {
            first     = common
            last      = common
            firstWC   = this.CursorWC
            lastWC    = this.CursorWC
            isForward = this.isForward
        }

    /// Sets anchor position to cursor position, sets anchor's WCs to cursor's WCs.
    member this.ReduceToCursor () =
        {
            first     = this.Cursor
            last      = this.Cursor
            firstWC   = this.CursorWC
            lastWC    = this.CursorWC
            isForward = this.isForward
        }

    /// Sets cursor's and anchor's wanted columns.
    member this.WithCursorAndAnchorWC cursorWC anchorWC =
        if this.isForward then
            { this with firstWC = anchorWC; lastWC = cursorWC }
        else
            { this with firstWC = cursorWC; lastWC = anchorWC }

    member private this.AsTextRange : ITextRange =
        {
            first = this.first
            last  = this.last
        } 

    member private this.MakeApplied (textRange: ITextRange) =
        {
            first     = textRange.First
            last      = textRange.Last
            firstWC   = WantedColumns_Default
            lastWC    = WantedColumns_Default
            isForward = this.isForward
        }

    interface ITextRange with
        member this.First = this.first
        member this.Last  = this.last

        /// Returns item prepared to be used in SelectionsModifier.
        member this.PrepareItem newLinesDelta =
            {
                first     = this.first.Add newLinesDelta 0
                last      = this.last .Add newLinesDelta 0
                firstWC   = WantedColumns_Default
                lastWC    = WantedColumns_Default
                isForward = this.isForward
            }

        /// Returns a new selection created by modifying selection according to insertSpec.
        member this.ApplyInsert (insertSpec: InsertSpec) =
            let textRange =
                this.AsTextRange.ApplyInsert insertSpec

            this.MakeApplied textRange

        /// Returns a new selection created by modifying selection according to deleteSpec,
        /// and a flag specifying if the selection has been eliminated by modifying it.
        member this.ApplyDelete (deleteSpec: DeleteSpec) =
            let isElimineted, textRange =
                this.AsTextRange.ApplyDelete deleteSpec

            (isElimineted, this.MakeApplied textRange)

        /// Returns a new selection created by modifying selection according to one-line insertSpec.
        member this.ApplyOneLineInsert (insertSpec: OneLineInsertSpec) =
            let textRange =
                this.AsTextRange.ApplyOneLineInsert insertSpec

            this.MakeApplied textRange

        /// Returns a new selection created by modifying selection according to one-line deleteSpec,
        /// and a flag specifying if the selection has been eliminated by modifying it.
        member this.ApplyOneLineDelete (deleteSpec: OneLineDeleteSpec) =
            let isElimineted, textRange =
                this.AsTextRange.ApplyOneLineDelete deleteSpec

            (isElimineted, this.MakeApplied textRange)

let Selection_Zero = {
    first     = Position_Zero
    last      = Position_Zero
    firstWC   = WantedColumns_Zero
    lastWC    = WantedColumns_Zero
    isForward = true
}

let Selection_Default = {
    first     = Position_Default
    last      = Position_Default
    firstWC   = WantedColumns_Default
    lastWC    = WantedColumns_Default
    isForward = true
}

let Selection_Invalid = {
    first     = Position_Invalid
    last      = Position_Invalid
    firstWC   = WantedColumns_Invalid
    lastWC    = WantedColumns_Invalid
    isForward = true
}
