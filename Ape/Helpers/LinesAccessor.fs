module LinesAccessor

open System.Collections.Immutable
open System.Collections.ObjectModel

open Commands.OutCommands
open DataTypes
open Position

type private TransformFun =
    int -> int -> int -> Chars ->
        (Chars * ReadOnlyCollection<ModifyingOutCommand>) option

type private ProcessFun =
    int -> int -> int -> bool -> Chars ->
        bool

/// Returns indentation characters for splitting chars at char_.
let private getIndentChars (chars: Chars) char_ =
    let firstNonWhiteSpace = chars |> Seq.tryFindIndex (
        System.Char.IsWhiteSpace >> not
    )

    let whiteSpaces = firstNonWhiteSpace |> Option.defaultValue chars.Length

    chars.Slice (0, min char_ whiteSpaces)

/// LinesAccessor provides methods used in various commands performers for
/// getting and modifying the content of myLines. All modifying operations on
/// myLines are performed only through instances of this class. This class also
/// provides several helper properties and methods used in the context getting
/// and modifying the content of myLines.

type LinesAccessor (myLines: Lines) =

    // flags

    member val IsLinesModified = false
        with get, set

    member val IsPossibleEolAfterEof = false
        with get, set

    member this.ClearFlags () =
        this.IsLinesModified       <- false
        this.IsPossibleEolAfterEof <- false

    // positions

    member _.GetPrevChar (position: Position) : Position =
        let line, char_ = position.line, position.char

        if char_ > 0 then
            { line = line; char = char_ - 1 }
        elif line > 0 then
            { line = line - 1; char = myLines[line - 1].Length }
        else
            // not existing position
            { line = line - 1; char = 0 }

    member _.GetNextChar (position: Position) : Position =
        let line, char_ = position.line, position.char

        if char_ < myLines[line].Length then
            { line = line; char = char_ + 1 }
        elif line < myLines.Count - 1 then
            { line = line + 1; char = 0 }
        else
            // not existing position
            { line = line + 1; char = 0 }

    member _.EofPosition =
        {
            line = myLines.Count
            char = 0
        }

    member _.IsAtSof (position: Position) =
        position.line = 0 && position.char = 0

    member _.IsAtLastEol (position: Position) =
           position.line = myLines.Count - 1
        && position.char = myLines[position.line].Length

    member _.IsAfterEof (position: Position) =
        position.line = myLines.Count

    // getting operations' specifications

    member _.GetInsertSpec target (lines: Lines) preferMove =
        let linesCount = lines.Count

        let endChars =
            if linesCount > 1 then
                lines[linesCount - 1].Length
            else
                0

        {
            target     = target
            startChars = lines[0].Length
            newLines   = linesCount - 1
            endChars   = endChars
            preferMove = preferMove
        }

    member this.GetDeleteSpec first last =
        let leftKept  = this.GetPrevChar first
        let rightKept = this.GetNextChar last

        {
            leftKept   = leftKept
            first      = first
            rightKept  = rightKept
        }

    // yanking

    member _.Yank line (from: int) (to_: int) =
        ImmutableArray.Create (myLines[line], from, to_ - from)

    member this.YankToEnd line from =
        this.Yank line from myLines[line].Length

    member _.YankEmpty () =
        Chars.Empty

    member _.YankLines from to_ =
        myLines.GetRange (from, to_ - from)

    // removing

    member this.Remove line (from: int) (to_: int) =
        if from <> to_ then
            myLines[line] <- myLines[line].RemoveRange (from, to_ - from)
            this.IsLinesModified <- true

    member this.RemoveToEnd line from =
        this.Remove line from myLines[line].Length

    member this.RemoveLines from to_ =
        if from <> to_ then
            myLines.RemoveRange (from, to_ - from)
            this.IsLinesModified <- true

    member this.CombineTwoLines first =
        myLines[first] <- myLines[first].AddRange myLines[first + 1]
        myLines.RemoveAt (first + 1)

        this.IsLinesModified <- true

    // inserting

    member this.InsertChar line char_ c =
        myLines[line] <- myLines[line].Insert (char_, c)
        this.IsLinesModified <- true

    member this.Insert line char_ (what: Chars) =
        if what.Length <> 0 then
            myLines[line] <- myLines[line].InsertRange (char_, what)
            this.IsLinesModified <- true

    member this.Append line (what: Chars) =
        if what.Length <> 0 then
            myLines[line] <- myLines[line].AddRange what
            this.IsLinesModified <- true

    member this.InsertLines line (what: Lines) =
        if what.Count <> 0 then
            myLines.InsertRange (line, what)
            this.IsLinesModified <- true

    member this.AppendLines (what: Lines) =
        if what.Count <> 0 then
            myLines.AddRange what
            this.IsLinesModified <- true

    member this.SplitLine line char_ =
        let chars = myLines[line]
        let left  = ImmutableArray.Create (chars, 0    , char_)
        let right = ImmutableArray.Create (chars, char_, chars.Length - char_)
        myLines[line] <- left
        myLines.Insert (line + 1, right)

        this.IsLinesModified <- true

    member this.SplitLineIndent line char_ =
        let chars = myLines[line]
        let left = ImmutableArray.Create (chars, 0    , char_)
        let right = ImmutableArray.Create (chars, char_, chars.Length - char_)
        let indent = getIndentChars chars char_
        myLines[line] <- left
        myLines.Insert (line + 1, indent.AddRange right)

        this.IsLinesModified <- true

        indent.Length

    // transforming

    member this.Transform line (from: int) (to_: int) (f: TransformFun) =
        let chars = myLines[line]
        let result = f line from to_ chars

        match result with
        | Some (transformed: Chars, outCommands) ->
            myLines[line] <-
                chars.RemoveRange(from, to_ - from)
                     .InsertRange(from, transformed)
            this.IsLinesModified <- true
            outCommands
        | None ->
            ModifyingOutCommands_EmptyReadOnly

    member this.TransformToEnd line from (f: TransformFun) =
        this.Transform line from myLines[line].Length f

    member this.TransformLines from to_ (f: TransformFun) =
        let outCommands = ModifyingOutCommands ()

        for line = from to to_ - 1 do
            let chars = myLines[line]
            let to_ = chars.Length
            let result = f line 0 to_ chars

            match result with
            | Some (transformed, outCommandsInner) ->
                myLines[line] <- transformed
                outCommands.AddRange outCommandsInner
                this.IsLinesModified <- true
            | None ->
                ()

        outCommands

    // processing

    /// Processes part of given line, doesn't add the new line character.
    /// Calls f and returns its return value.
    member _.Process line from to_ (f: ProcessFun) =
        let chars = myLines[line]
        f line from to_ false chars

    /// Processes part of given line, adds the new line character.
    /// Calls f and returns its return value.
    member _.ProcessToEnd line from (f: ProcessFun) =
        let chars = myLines[line]
        f line from myLines[line].Length true chars

    /// Processes given lines, adds the new line character.
    /// Breaks the iteration if f returned true.
    /// Returns true if the iteration was broken.
    member _.ProcessLines from to_ (f: ProcessFun) =
        let mutable toBreak = false
        let mutable line = from

        while (not toBreak) && line < to_ do
            let chars = myLines[line]
            let to_ = chars.Length
            toBreak <- f line 0 to_ true chars
            line <- line + 1

        toBreak
