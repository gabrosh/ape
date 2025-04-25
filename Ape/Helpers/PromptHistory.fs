module PromptHistory

open DataTypes

/// PromptHistory maintains history for one category of prompts.
/// The history itself is changed only when leaving the prompt.

type PromptHistory () =
    let myLines = Lines ()

    let mutable myCurrent = 0

    /// true if the current line comes from the history.
    member _.IsCurrentFromHistory =
        myCurrent <> myLines.Count

    // only for testing purposes
    member _.Lines = myLines

    /// If toOverwrite is true, it removes current line from the history, then
    /// if line is not empty, it adds line to the history and removes history duplicates.
    /// Finally, it sets the non-existing empty line after the end of the history as
    /// current line.
    member this.WhenLeaving (line: Chars) toOverwrite =
        if toOverwrite then
            if this.IsCurrentFromHistory then
                myLines.RemoveAt myCurrent

        if not line.IsEmpty then
            myLines.Add line
            this.RemoveDuplicates ()

        myCurrent <- myLines.Count

    /// Returns the previous line from the history.
    member _.GetPrevious () =
        if myCurrent > 0 then
            myCurrent <- myCurrent - 1
            Some myLines[myCurrent]
        else
            None

    /// Returns the next line from the history.
    member _.GetNext () =
        let count = myLines.Count

        if myCurrent < count - 1 then
            myCurrent <- myCurrent + 1
            Some myLines[myCurrent]
        elif myCurrent = count - 1 then
            myCurrent <- myCurrent + 1
            Some Chars.Empty
        else
            None

    member private _.RemoveDuplicates () =
        let distinct =
            myLines |> Seq.rev |> Seq.distinct |> Seq.rev |> Seq.toList
        myLines.Clear ()
        myLines.AddRange distinct
