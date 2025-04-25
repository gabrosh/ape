module Position

open DataTypes

// Position

[<Struct>]
type Position = {
    line: int
    char: int
}
with
    member this.Add lines chars =
        { line = this.line + lines; char = this.char + chars }

    member this.Sub lines chars =
        { line = this.line - lines; char = this.char - chars }

let Position_Zero    = { line = 0; char = 0 }
let Position_Max     = { line = IntType.MaxValue; char = IntType.MaxValue }
let Position_Default = Position_Zero
let Position_Invalid = Position_Max

// operations' specifications

type InsertSpec = {
    target:     Position  // position where the block will be inserted
    startChars: int       // count of starting chars
    newLines:   int       // count of new lines after the starting chars
    endChars:   int       // count of chars after the last new line
    preferMove: bool      // in some cases, prefer moving to widening
}

type DeleteSpec = {
    leftKept:   Position  // last kept before the deleted block
    first:      Position  // first character of the deleted block
    rightKept:  Position  // first kept after the deleted block
}
with
    member this.NewLines =
        this.rightKept.line - this.first.line

type OneLineInsertSpec = {
    line:       int       // line
    target:     int       // position where the block will be inserted
    chars:      int       // count of inserted chars
    preferMove: bool      // in some cases, prefer moving to widening
}

type OneLineDeleteSpec = {
    line:       int       // line
    first:      int       // first character of the deleted block
    rightKept:  int       // first kept after the deleted block
}

// changing position according to operation's specification

/// Returns a new position created by modifying position according to insertSpec.
let applyInsertToPos (insertSpec: InsertSpec) (position: Position) =
    if not (insertSpec.target <= position) then
        invalidOp ""

    let newLines = insertSpec.newLines

    if position.line = insertSpec.target.line then
        if newLines = 0 then
            // Move position right by the specified delta.
            position.Add 0 insertSpec.startChars
        else
            // The line was split.
            // insertSpec.endChars were inserted at the start of the separated line.
            // insertSpec.target.char chars are not part of the separated line.
            position.Add newLines (insertSpec.endChars - insertSpec.target.char)
    else
        position.Add newLines 0

/// Returns a new position created by modifying position according to deleteSpec.
let applyDeleteToPos (deleteSpec: DeleteSpec) (position: Position) =
    if not (deleteSpec.rightKept <= position) then
        invalidOp ""

    let newLines = deleteSpec.NewLines

    if position.line = deleteSpec.rightKept.line then
        if newLines = 0 then
            // Move position left by the specified delta.
            position.Sub newLines (deleteSpec.rightKept.char - deleteSpec.first.char)
        else
            // Lines first.line and rightKept.line were joined together.
            // deleteSpec.rightKept.char chars were deleted from the joined line.
            // deleteSpec.first.char chars are part of the joined line.
            position.Sub newLines (deleteSpec.rightKept.char - deleteSpec.first.char)
    else
        position.Sub newLines 0

/// Returns a new position created by modifying position according to insertSpec.
let applyOneLineInsertToPos
    (insertSpec: OneLineInsertSpec) (position: Position) =

    position.Add 0 insertSpec.chars

/// Returns a new position created by modifying position according to deleteSpec.
let applyOneLineDeleteToPos
    (deleteSpec: OneLineDeleteSpec) (position: Position) =

    let first     = deleteSpec.first
    let rightKept = deleteSpec.rightKept

    let posChar = position.char

    if rightKept < posChar then
        { position with char = first + (posChar - rightKept) }
    else
        { position with char = first }
