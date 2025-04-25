module SimpleRegex

open System
open System.Collections.Generic
open System.Text

open DataTypes
open Position

type private Regex = System.Text.RegularExpressions.Regex

/// AbstractTrivialRegex is common parent of classes performing trivial
/// regex search for matches. Search is performed line by line.

[<AbstractClass>]
type AbstractSimpleRegex (
    myRegexObject: Regex,
    myToSkip:      HashSet<Position>
) =
    /// Called to register a match.
    abstract member RegisterMatch:
        span: ReadOnlySpan<char> -> bool

    /// Called to finish registering of matches.
    abstract member FinishRegistering:
        unit -> unit

    /// Performs processing of given slice of chars.
    member this.ProcessLine (line: int) (chars: Chars) =
        let span: ReadOnlySpan<char> = chars.AsSpan ()

        let mutable enumerator = myRegexObject.EnumerateMatches span

        try
            let mutable toBreak = false

            while (not toBreak) && enumerator.MoveNext () do
                let match_ = enumerator.Current
                let first  = match_.Index
                let length = match_.Length

                if length <> 0 && not (
                    myToSkip.Contains { line = line; char = first }
                )
                then
                    let span' = span.Slice (first, length)
                    toBreak <- this.RegisterMatch span'

            // Return true to break the iteration - optimization for IsMatch child.
            toBreak
        with
            | :? RegularExpressions.RegexMatchTimeoutException as ex ->
                raise (RegexMatchTimeoutException ex.Message)

    /// Finishes processing of given slices of chars.
    member this.FinishProcessing () =
        this.FinishRegistering ()

// concrete classes

type AddMatchesAsStringsSet (
    inRegexObject: Regex,
    inToSkip:      HashSet<Position>
) =
    inherit AbstractSimpleRegex (inRegexObject, inToSkip)

    let mutable myStringsSet: HashSet<string> = null

    member _.Init stringsSet =
        myStringsSet <- stringsSet

    override _.RegisterMatch (span: ReadOnlySpan<char>) =
        myStringsSet.Add (span.ToString ()) |> ignore

        // Don't break the iteration.
        false

    override _.FinishRegistering () =
        ()
