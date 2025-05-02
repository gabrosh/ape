module MatchRanges

open System.Collections.Generic

open DataTypes
open IMatchRanges
open TextRanges
open UserMessages

type private Regex = System.Text.RegularExpressions.Regex

/// MatchRanges holds results of searching for regex matches as a dictionary of
/// TextRanges for the main group and, optionally, several named groups. It provides
/// information about the groups, and can perform clearing or re-searching with
/// the last used regex.

type MatchRanges (
    myUserMessages: UserMessages,
    myLines:        Lines,
    inLastRegex:    string option,
    inWasCleared:   bool,
    inTextRanges:   Dictionary<string, TextRanges>
) =
    let mutable myLastRegex  = inLastRegex
    let mutable myWasCleared = inWasCleared
    let mutable myTextRanges = inTextRanges

    new (inUserMessages: UserMessages, inLines: Lines) =
        MatchRanges(
            inUserMessages, inLines, None, false, makeTextRangesGroups ()
        )

    // public properties

    member _.SearchedLines = myLines

    member _.LastRegex  = myLastRegex
    member _.WasCleared = myWasCleared

    // internal properties

    member internal _.TextRanges
        with get ()    = myTextRanges
        and  set value = myTextRanges <- value

    /// Creates an extract version of this instance using constructor constr.
    member _.CreateExtract constr (linesExtract: Lines) =
        constr (
            myUserMessages, myLines, linesExtract, myLastRegex, myWasCleared, myTextRanges
        )

    /// Returns count of text ranges in the main group.
    member _.GetMainGroupCount () =
        myTextRanges[mainGroupName].Count

    /// Returns all text ranges from the main group.
    member _.GetAllFromMainGroup () =
        myTextRanges[mainGroupName]

    /// Returns all text ranges from all groups.
    member _.GetAllFromAllGroups () =
        myTextRanges |> Seq.map (
            fun item -> (RegexUtils.getColorIndex item.Key, item.Value)
        )
        |> Seq.toArray |> Array.sortBy fst

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

        if this.GetMainGroupCount () = 0 then
            myUserMessages.RegisterMessage WARNING_NO_MATCH_FOUND            

    /// Re-searches for the regex used in the last call to Search.
    member this.ReSearch () =
        match myLastRegex with
        | Some regex -> this.Search regex
        | None       -> ()

        myWasCleared <- false

    // virtual

    /// Searches the lines for given single-line regular expression.
    abstract member SearchSingleLine:
        regexObject: Regex
     -> unit

    override _.SearchSingleLine (regexObject: Regex) =
        myTextRanges <- makeTextRangesGroups ()

        let slr = SingleLineRegex.AddMatchesAsTextRanges regexObject
        slr.Init myTextRanges

        for i, chars in myLines |> Seq.indexed do
            slr.ProcessLine i 0 chars.Length true chars
                |> ignore
        slr.FinishProcessing ()

    /// Searches the lines for given multi-line regular expression.
    abstract member SearchMultiLine:
        regexObject: Regex
     -> unit

    override _.SearchMultiLine (regexObject: Regex) =
        myTextRanges <- makeTextRangesGroups ()

        let mlr = MultiLineRegex.AddMatchesAsTextRanges (
            myUserMessages, regexObject, myTextRanges
        )

        for i, chars in myLines |> Seq.indexed do
            mlr.ProcessLine i 0 chars.Length true chars
                |> ignore
        mlr.FinishProcessing ()

    /// Clears text ranges from the last call to Search.
    abstract member Clear:
        unit -> unit

    override _.Clear () =
        myTextRanges <- makeTextRangesGroups ()
        myWasCleared <- true

    // IMatchRanges

    interface IMatchRanges with

        member this.GetInIntervalFromAllGroups startLine endLine =
            this.GetInIntervalFromAllGroups startLine endLine

        member this.GetMainGroupCount ()   = this.GetMainGroupCount ()
        member this.GetAllFromMainGroup () = this.GetAllFromMainGroup ()
        member this.ReSearch ()            = this.ReSearch ()
