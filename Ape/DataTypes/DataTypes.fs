module DataTypes

open System
open System.Collections.Immutable

open Colors

// custom exceptions

type RegexMatchTimeoutException (message: string) =
    inherit Exception(message)

/// int type supplement for accessing int's static members.
type IntType = int

type Chars = FileUtils.Chars
type Lines = FileUtils.Lines

/// Converts position to user position.
let posToUserPos x =
    x + 1

/// Converts user position to position.
let userPosToPos x =
    x - 1

/// Converts string to Chars.
let stringToChars (s: string) : Chars =
    ImmutableArray.Create<char> (s.AsSpan ())

/// Converts single char to Chars.
let charToChars (c: char) : Chars =
    ImmutableArray.Create<char> c

/// Converts Chars to string.
let charsToString (chars: Chars) =
    chars.AsSpan().ToString()

/// Returns true if lines contains only a single empty line.
let isSingleEmptyLine (lines: Lines) =
    lines.Count = 1 && lines[0].IsEmpty

/// Returns true if the last line of given lines is empty.
let hasLastEmptyLine (lines: Lines) =
    lines[lines.Count - 1].IsEmpty

/// Removes the last line of given lines.
let trimLastLine (lines: Lines) =
    lines |> Seq.take (lines.Count - 1) |> Lines

/// Removes the last line of given lines if it is empty.
let trimLastEmptyLine (lines: Lines) =
    if hasLastEmptyLine lines then
        trimLastLine lines
    else
        lines

/// Returns width of the "line numbers" text area column.
let getLineNumbersWidth (showLineNumbers: bool) (linesCount: int) =
    if showLineNumbers then
        (Utils.getDecimalWidth (uint64 linesCount)) + 1
    else
        0

[<Struct>]
type CharOption =
    | Char of char
    | EOL
    | NoChar

[<Struct>]
type DisplayChar = {
    c:      char
    colors: CharColors
}

type CursorPosForStatusArea = {
    selectionsCount: int
    line:            int
    char_:           int
    firstColumn:     int
    columnsCount:    int
}

[<Struct>]
type RegisterSelection =
    | DefaultRegister
    | SelectedRegister of isUpper: bool * c: char
