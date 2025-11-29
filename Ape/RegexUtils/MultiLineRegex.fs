module MultiLineRegex

open System.Collections.Generic
open System.Runtime.InteropServices
open System.Text

open BinarySearch
open DataTypes
open Position
open Selection
open TextRanges
open UserMessages

type private Match = System.Text.RegularExpressions.Match
type private Regex = System.Text.RegularExpressions.Regex

type LineSpec = {
    startIndex: int
    startPos:   Position
}

// bumper for a possible lookbehind or lookahead
let private linesBumperLength = 100
// lines chunk length corresponds to lines count soft limit of a possible match
let private linesChunkLength  = 5000

// bumber for lookbehind, two chunks for a possible match, bumber for lookahead
let private lineSpecsCapacity =
    linesBumperLength + 2 * linesChunkLength + linesBumperLength

let private compareIndexTo index a =
    compareTo index a.startIndex

/// AbstractMultiLineRegex is common parent of classes performing multi-line
/// regex search for matches. Search is performed on chunks of concatenated lines.
/// Optionally, named groups can be handled separately from the main group.

[<AbstractClass>]
type AbstractMultiLineRegex (
    myUserMessages: UserMessages,
    myRegexObject:  Regex,
    inWithGroups:   bool
) as this_ =
    let myWithGroups = inWithGroups && RegexUtils.hasColoredGroups myRegexObject

    let myProcessChunk =
        if not myWithGroups
        then this_.ProcessChunkWithoutGroups
        else this_.ProcessChunkWithGroups

    let myStringBuilder = StringBuilder ()
    let myStringBuffer  = ResizeArray<char> ()

    let myLineSpecs = ResizeArray<LineSpec> lineSpecsCapacity

    // count of chars already removed from myStringBuilder
    let mutable myOffset       = 0
    // start of the current chunk in myStringBuilder
    let mutable myCurrentChunk = 0
    // start of the next possible chunk in myStringBuilder
    let mutable myNextChunk    = 0

    /// Called to register a match.
    abstract member RegisterMatch:
        first: Position -> last: Position -> unit

    /// Called to register a named match.
    abstract member RegisterNamedMatch:
        first: Position -> last: Position -> name: string -> unit

    /// Called to finish registering of matches and named matches.
    abstract member FinishRegistering:
        unit -> unit

    /// Adds given slice of chars to the internal string of lines. If there
    /// are enough lines for processing a chunk, performs the processing.
    member this.ProcessLine line from to_ toAddNewLine (chars: Chars) =
        if myLineSpecs.Count = lineSpecsCapacity then
            myProcessChunk ()
            this.StartNextChunk ()

        myLineSpecs.Add {
            startIndex = myOffset + myStringBuilder.Length
            startPos   = { line = line; char = from }
        }

        myStringBuilder.Append (
            if from <> 0 || to_ <> chars.Length then
                chars.AsSpan (from, to_ - from)
            else
                chars.AsSpan ()
        ) |> ignore

        if toAddNewLine then
            myStringBuilder.Append '\n' |> ignore

        // Don't break the iteration - no optimization for IsMatch child.
        false

    /// Finishes adding to the internal string of lines and processing of chunks.
    member this.FinishProcessing () =
        if myLineSpecs.Count > linesBumperLength + linesChunkLength then
            myProcessChunk ()
            this.StartNextChunk ()

        myProcessChunk ()

        this.FinishRegistering ()

    // auxiliary

    member private this.StartNextChunk () =
        let charsToRemove   = this.GetLineStartIndex linesChunkLength
        let currentChunkEnd = this.GetLineStartIndex (linesBumperLength + linesChunkLength)

        myStringBuilder.Remove (0, charsToRemove) |> ignore
        myOffset <- myOffset + charsToRemove
        myLineSpecs.RemoveRange (0, linesChunkLength)

        if myNextChunk <= currentChunkEnd then
            myCurrentChunk <- currentChunkEnd - charsToRemove
        else
            myCurrentChunk <- myNextChunk - charsToRemove

        myNextChunk <- myCurrentChunk

    member private this.ProcessChunkWithoutGroups () =
        let matchIndexLimit = this.GetMatchIndexLimit ()

        // Copy the data from myStringBuilder to myStringBuffer.
        myStringBuffer.Clear ()
        myStringBuffer.EnsureCapacity myStringBuilder.Length |> ignore
        for chunk in myStringBuilder.GetChunks () do
            myStringBuffer.AddRange (MemoryMarshal.ToEnumerable chunk)
        let span = CollectionsMarshal.AsSpan myStringBuffer

        let mutable enumerator = myRegexObject.EnumerateMatches (span, myCurrentChunk)

        try
            let mutable toContinue = true

            while toContinue && enumerator.MoveNext () do
                let match_ = enumerator.Current
                let index  = match_.Index
                let length = match_.Length

                if length <> 0 && index < matchIndexLimit then
                    this.ProcessMatch index length
                    myNextChunk <- index + length

                toContinue <- (index + length < matchIndexLimit)
        with
            | :? RegularExpressions.RegexMatchTimeoutException as ex ->
                raise (RegexMatchTimeoutException ex.Message)

    member private this.ProcessChunkWithGroups () =
        let matchIndexLimit = this.GetMatchIndexLimit ()

        // Copy the data from myStringBuilder to myStringBuffer.
        myStringBuffer.Clear ()
        myStringBuffer.EnsureCapacity myStringBuilder.Length |> ignore
        for chunk in myStringBuilder.GetChunks () do
            myStringBuffer.AddRange (MemoryMarshal.ToEnumerable chunk)
        let span = CollectionsMarshal.AsSpan myStringBuffer

        let matches = myRegexObject.Matches (span.ToString (), myCurrentChunk)
        let enumerator = matches.GetEnumerator ()

        try
            let mutable toContinue = true

            while toContinue && enumerator.MoveNext () do
                let match_ = enumerator.Current :?> Match
                let index  = match_.Index
                let length = match_.Length

                if length <> 0 && index < matchIndexLimit then
                    this.ProcessMatch index length
                    myNextChunk <- index + length

                    for group in match_.Groups do
                        this.ProcessGroup group

                toContinue <- (index + length < matchIndexLimit)
        with
            | :? RegularExpressions.RegexMatchTimeoutException as ex ->
                raise (RegexMatchTimeoutException ex.Message)

    member private this.ProcessGroup group =
        let gName = group.Name

        if TextRanges.isColoredGroup gName then
            for capture in group.Captures do
                let cIndex  = capture.Index
                let cLength = capture.Length

                if cLength <> 0 then
                    this.ProcessNamedMatch cIndex cLength gName

    member private this.ProcessMatch index length =
        let firstIndex = index
        let lastIndex  = index + (length - 1)

        let first = this.GetPosition firstIndex
        let last  = this.GetPosition lastIndex

        this.RegisterMatch first last

        if last.line - first.line >= linesChunkLength then
            myUserMessages.RegisterMessage
                WARNING_MATCH_LINES_COUNT_LIMIT_EXCEEDED

    member private this.ProcessNamedMatch index length name =
        let firstIndex = index
        let lastIndex  = index + (length - 1)

        let first = this.GetPosition firstIndex
        let last  = this.GetPosition lastIndex

        this.RegisterNamedMatch first last name

    member private _.GetLineStartIndex i =
        let lineSpec = myLineSpecs[i]
        lineSpec.startIndex - myOffset

    member private this.GetMatchIndexLimit () =
        if myLineSpecs.Count > linesBumperLength + linesChunkLength then
            this.GetLineStartIndex (linesBumperLength + linesChunkLength)
        else
            DataTypes.IntType.MaxValue

    member private _.GetPosition index =
        let index = myOffset + index

        let compareFun = compareIndexTo index
        let result = findLastEqOrLtInSortedArray compareFun myLineSpecs

        match result with
        | Some m ->
            let lineSpec = myLineSpecs[m]
            let startPos = lineSpec.startPos
            {
                line = startPos.line
                char = startPos.char + (index - lineSpec.startIndex)
            }
        | None ->
            invalidOp ""

// concrete classes

type AddMatchesAsTextRanges (
    inUserMessages: UserMessages,
    inRegexObject:  Regex,
    myTextRanges:   Dictionary<string, TextRanges>
) =
    inherit AbstractMultiLineRegex (inUserMessages, inRegexObject, true)

    override _.RegisterMatch first last =
        myTextRanges[mainGroupName].Add {
            first = first; last = last
        }

    override _.RegisterNamedMatch first last name =
        let ok, item = myTextRanges.TryGetValue name

        if ok then
            item.Add {
                first = first; last = last
            }
        else
            let item = TextRanges ()
            myTextRanges[name] <- item
            item.Add {
                first = first; last = last
            }

    override _.FinishRegistering () =
        sortRangesDictionary myTextRanges

type AddMatchesAsSelections (
    inUserMessages: UserMessages,
    inRegexObject:  Regex,
    mySelections:   ResizeArray<Selection>,
    myIsForward:    bool
) =
    inherit AbstractMultiLineRegex (inUserMessages, inRegexObject, false)

    override _.RegisterMatch first last =
        mySelections.Add {
            first     = first
            last      = last
            firstWC   = WantedColumns_Default
            lastWC    = WantedColumns_Default
            isForward = myIsForward
        }

    override _.RegisterNamedMatch _first _last _name =
        invalidOp ""

    override _.FinishRegistering () =
        ()

type IsMatch (
    inUserMessages: UserMessages,
    inRegexObject:  Regex
) =
    inherit AbstractMultiLineRegex (inUserMessages, inRegexObject, false)

    let mutable myIsMatch = false

    /// Is true if a match of regexObject was found in the internal string.
    member _.IsMatch = myIsMatch

    override _.RegisterMatch _first _last =
        myIsMatch <- true

    override _.RegisterNamedMatch _first _last _name =
        invalidOp ""

    override _.FinishRegistering () =
        ()
