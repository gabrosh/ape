module Register

open System.Text

open DataTypes

/// Returns true if lines is empty and thus unsuitable for storing into register slot.
let isContentEmpty (lines: Lines) =
       (lines.Count = 0)
    || (lines.Count = 1 && lines[0].Length = 0)

/// Makes Lines from given string formed by one or multiple lines separated by '\n'.
let makeLinesFromString (s: string) =
    let lines = s.Split '\n'
    let result = ResizeArray lines.Length

    for line in lines do
        let trimmed = line.TrimEnd '\r'
        let chars = stringToChars trimmed
        result.Add chars

    result

let private joinSlots (slots: ResizeArray<Lines>) =
    seq {
        let count = slots.Count

        // There is always at least one slot.
        for i = 0 to count - 2 do
            yield! slots[i] |> trimLastEmptyLine

        yield! slots[count - 1]
    }

let private appendLines (lines: Chars seq) (sb: StringBuilder) =
    let mutable firstSlot = true

    for line in lines do
        if not firstSlot then
            sb.Append '\n' |> ignore
        else
            firstSlot <- false
        sb.Append (line.AsSpan ()) |> ignore

/// Register holds a list of register's slots and performs operations on them.

type Register () =
    let mySlots = ResizeArray<Lines> ()

    /// Clears all register slots.
    member _.Clear () =
        mySlots.Clear ()

    /// Sets register slots "index" to lines.
    member this.SetSlot index (lines: Lines) =
        if isContentEmpty lines then
            invalidOp "Can't set an empty content to the register slot"

        if index < mySlots.Count then
            this.SetToExistingSlot index lines
        else
            this.AddNewSlot index lines

    /// Appends lines to register slot "index".
    member this.AppendToSlot index (lines: Lines) =
        if isContentEmpty lines then
            invalidOp "Can't append an empty content to the register slot"

        if index < mySlots.Count then
            this.AppendToExistingSlot index lines
        else
            this.AddNewSlot index lines

    /// Returns count of register slots.
    member _.GetSlotsCount () =
        mySlots.Count

    /// Returns register slots "index" or None if there is no such slot.
    member _.GetSlot index =
        if index < mySlots.Count then
            Some mySlots[index]
        else
            None

    /// Returns all register slots joined together or None if there are no slots.
    member _.GetJoinedSlots () =
        if mySlots.Count <> 0 then
            Some (joinSlots mySlots |> Lines)
        else
            None

    /// Sets the first register slots to given string.
    member this.SetFromString (s: string) =
        let lines = makeLinesFromString s
        if not (isContentEmpty lines) then
            this.SetSlot 0 lines

    /// Returns all register slots joined together using new line character as a single string.
    member _.GetAsString () =
        let sb = StringBuilder ()
        appendLines (joinSlots mySlots) sb
        sb.ToString ()

    member private _.AddNewSlot index lines =
        if not (index = mySlots.Count) then
            invalidOp ""

        mySlots.Add lines

    member private _.SetToExistingSlot index lines =
        mySlots[index] <- lines

    member private _.AppendToExistingSlot index lines =
        let count = lines.Count
        let head  = lines[0]
        let tail  = lines.GetRange (1, count - 1)

        let slot = mySlots[index]
        slot[slot.Count - 1] <- slot[slot.Count - 1].AddRange head
        slot.AddRange tail
