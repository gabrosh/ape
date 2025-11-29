module SingleLineRegex

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Text

open DataTypes
open Position
open Selection
open TextRanges

type private Match = System.Text.RegularExpressions.Match
type private Regex = System.Text.RegularExpressions.Regex

/// AbstractSingleLineRegex is common parent of classes performing single-line
/// regex search for matches. Search is performed line by line. Optionally, named
/// groups can be handled separately from the main group.

[<AbstractClass>]
type AbstractSingleLineRegex (
    myRegexObject: Regex,
    inWithGroups:  bool
) as this_ =
    let myCanMatchNewLine = RegexUtils.canMatchNewLine (myRegexObject.ToString ())

    let myWithGroups = inWithGroups && RegexUtils.hasColoredGroups myRegexObject

    let myProcessLine =
        if not myWithGroups
        then this_.ProcessLineWithoutGroups
        else this_.ProcessLineWithGroups

    let myStringBuffer = ResizeArray<char> ()

    /// Called to register a match.
    abstract member RegisterMatch:
        first: Position -> last: Position -> bool

    /// Called to register a named match.
    abstract member RegisterNamedMatch:
        first: Position -> last: Position -> name: string -> unit

    /// Called to finish registering of matches and named matches.
    abstract member FinishRegistering:
        unit -> unit

    /// Performs processing of given slice of chars.
    member _.ProcessLine line from to_ toAddNewLine (chars: Chars) =
        myProcessLine line from to_ toAddNewLine chars

    member private this.ProcessLineWithoutGroups line from to_ toAddNewLine (chars: Chars) =

        let span: ReadOnlySpan<char> =
            if myCanMatchNewLine && toAddNewLine then
                // If toAddNewLine = true, then to_ = chars.Length.
                this.PrepareBuffer chars
                let span = CollectionsMarshal.AsSpan myStringBuffer
                Span<char>.op_Implicit (span.Slice from)
            else
                let span = chars.AsSpan ()
                span.Slice (from, to_ - from)

        let mutable enumerator = myRegexObject.EnumerateMatches span

        try
            let mutable toBreak = false

            while (not toBreak) && enumerator.MoveNext () do
                let match_ = enumerator.Current
                let first  = from + match_.Index
                let length = match_.Length

                if length <> 0 then
                    toBreak <- this.ProcessMatch line first length

            // Return true to break the iteration - optimization for IsMatch child.
            toBreak
        with
            | :? RegularExpressions.RegexMatchTimeoutException as ex ->
                raise (RegexMatchTimeoutException ex.Message)

    member private this.ProcessLineWithGroups line from to_ toAddNewLine (chars: Chars) =

        let span: ReadOnlySpan<char> =
            if myCanMatchNewLine && toAddNewLine then
                // If toAddNewLine = true, then to_ = chars.Length.
                this.PrepareBuffer chars
                let span = CollectionsMarshal.AsSpan myStringBuffer
                Span<char>.op_Implicit (span.Slice from)
            else
                let span = chars.AsSpan ()
                span.Slice (from, to_ - from)

        let matches = myRegexObject.Matches (span.ToString ())
        let enumerator = matches.GetEnumerator ()

        try
            let mutable toBreak = false

            while (not toBreak) && enumerator.MoveNext () do
                let match_ = enumerator.Current :?> Match
                let first  = from + match_.Index
                let length = match_.Length

                if length <> 0 then
                    toBreak <- this.ProcessMatch line first length

                    for group in match_.Groups do
                        this.ProcessGroup line from group

            // Return true to break the iteration - optimization for IsMatch child.
            toBreak
        with
            | :? RegularExpressions.RegexMatchTimeoutException as ex ->
                raise (RegexMatchTimeoutException ex.Message)

    member private this.ProcessGroup line from group =
        let gName = group.Name

        if TextRanges.isColoredGroup gName then
            for capture in group.Captures do
                let cFirst  = from + capture.Index
                let cLength = capture.Length

                if cLength <> 0 then
                    this.ProcessNamedMatch line cFirst cLength gName

    member private this.ProcessMatch line firstChar length =
        let first = { line = line; char = firstChar              }
        let last  = { line = line; char = firstChar + length - 1 }

        this.RegisterMatch first last

    member private this.ProcessNamedMatch line firstChar length name =
        let first = { line = line; char = firstChar              }
        let last  = { line = line; char = firstChar + length - 1 }

        this.RegisterNamedMatch first last name

    /// Finishes processing of given slices of chars.
    member this.FinishProcessing () =
        this.FinishRegistering ()

    // auxiliary

    member private _.PrepareBuffer (chars: Chars) =
        myStringBuffer.Clear ()
        myStringBuffer.AddRange chars
        myStringBuffer.Add '\n'

// concrete classes

type AddMatchesAsTextRanges (
    inRegexObject: Regex
) =
    inherit AbstractSingleLineRegex (inRegexObject, true)

    let mutable myTextRanges: Dictionary<string, TextRanges> = null

    member _.Init textRanges =
        myTextRanges <- textRanges

    override _.RegisterMatch first last =
        myTextRanges[mainGroupName].Add {
            first = first; last = last
        }

        // Don't break the iteration.
        false

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
    inRegexObject: Regex
) =
    inherit AbstractSingleLineRegex (inRegexObject, false)

    let mutable myIsForward = false

    let mutable mySelections: ResizeArray<Selection> = null

    member _.Init isForward selections =
        myIsForward  <- isForward
        mySelections <- selections

    override _.RegisterMatch first last =
        mySelections.Add {
            first     = first
            last      = last
            firstWC   = WantedColumns_Default
            lastWC    = WantedColumns_Default
            isForward = myIsForward
        }

        // Don't break the iteration.
        false

    override _.RegisterNamedMatch _first _last _name =
        invalidOp ""

    override _.FinishRegistering () =
        ()

type IsMatch (
    inRegexObject: Regex
) =
    inherit AbstractSingleLineRegex (inRegexObject, false)

    let mutable myIsMatch = false

    /// Is true if a match of regexObject was found in the internal string.
    member _.IsMatch = myIsMatch

    member _.Init () =
        myIsMatch <- false

    override _.RegisterMatch _first _last =
        myIsMatch <- true

        // Break the iteration.
        true

    override _.RegisterNamedMatch _first _last _name =
        invalidOp ""

    override _.FinishRegistering () =
        ()
