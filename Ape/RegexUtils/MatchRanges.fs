module MatchRanges

open DataTypes
open TextRanges
open UserMessages

/// MatchRanges holds results of searching for regex matches as a dictionary of
/// TextRanges for the main group and, optionally, several named groups. It provides
/// information about the groups, and can perform clearing or re-searching with
/// the last used regex.

type MatchRanges (
    myUserMessages: UserMessages,
    myLines:        Lines
) =
    let mutable myLastRegex  = None
    let mutable myWasCleared = false
    let mutable myTextRanges = makeTextRangesGroups ()

    member _.LastRegex  = myLastRegex
    member _.WasCleared = myWasCleared

    /// Returns count of text ranges in the main group.
    member _.GetMainGroupCount () =
        myTextRanges[mainGroupName].Count

    /// Returns all text ranges from the main group.
    member _.GetAllFromMainGroup () =
        myTextRanges[mainGroupName]

    /// Returns text ranges from all groups in interval <startLine, endLine>.
    member _.GetInIntervalFromAllGroups startLine endLine =
        myTextRanges |> Seq.map (
            fun item -> (RegexUtils.getColorIndex item.Key, item.Value)
        )
        |> Seq.map (
            fun (_colorIndex, textRanges) ->
                (_colorIndex, getFromInterval textRanges startLine endLine)
        )
        |> Seq.toArray |> Array.sortBy fst

    /// Searches for regex going through the whole myLines.
    member this.Search regex =
        myLastRegex  <- Some regex
        myWasCleared <- false

        let regexObject = RegexUtils.makeRegexObject regex

        if RegexUtils.isMultiLineRegex regex then
            this.SearchMultiLine regexObject
        else
            this.SearchSingleLine regexObject

    /// Re-searches for the regex used in the last call to Search.
    member this.ReSearch () =
        match myLastRegex with
        | Some regex -> this.Search regex
        | None       -> ()

        myWasCleared <- false

    /// Clears text ranges from the last call to Search.
    member _.Clear () =
        myTextRanges <- makeTextRangesGroups ()
        myWasCleared <- true

    // auxiliary

    member private _.SearchSingleLine regexObject =
        myTextRanges <- makeTextRangesGroups ()

        let slr = SingleLineRegex.AddMatchesAsTextRanges regexObject
        slr.Init myTextRanges

        for i, chars in myLines |> Seq.indexed do
            slr.ProcessLine i 0 chars.Length true chars
                |> ignore
        slr.FinishProcessing ()

    member private _.SearchMultiLine regexObject =
        myTextRanges <- makeTextRangesGroups ()

        let mlr = MultiLineRegex.AddMatchesAsTextRanges (
            myUserMessages, regexObject, myTextRanges
        )

        for i, chars in myLines |> Seq.indexed do
            mlr.ProcessLine i 0 chars.Length true chars
                |> ignore
        mlr.FinishProcessing ()
