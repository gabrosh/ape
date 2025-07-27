module MatchRanges

open System.Collections.Generic

open DataTypes
open IMatchRanges
open RegexUtils
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
    inIsCleared:    bool,
    inTextRanges:   Dictionary<string, TextRanges>
) =
    let mutable myLastRegex  = inLastRegex
    let mutable myIsCleared  = inIsCleared
    let mutable myTextRanges = inTextRanges

    let mutable myWarnIfNoMatchFound = true

    new (inUserMessages: UserMessages, inLines: Lines) =
        MatchRanges(
            inUserMessages, inLines, None, true, makeTextRangesGroups ()
        )

    // public properties

    member _.SearchedLines = myLines

    member _.LastRegex = myLastRegex
    member _.IsCleared = myIsCleared

    // internal properties

    member internal _.TextRanges
        with get ()    = myTextRanges
        and  set value = myTextRanges <- value

    /// Runs given function with or without a warning if no match is found.
    member _.RunWithSetWarnIfNoMatchFound warnIfNoMatchFound f =
        let prevValue = myWarnIfNoMatchFound
        try
            myWarnIfNoMatchFound <- warnIfNoMatchFound
            f ()
        finally
            myWarnIfNoMatchFound <- prevValue

    /// Creates an extract version of this instance using constructor constr.
    member _.CreateExtract constr (linesExtract: Lines) (extractOnConstr: bool) =
        constr (
            myUserMessages, myLines,
            myLastRegex, myIsCleared, myTextRanges,
            linesExtract, extractOnConstr
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
            fun item -> (getColorIndex item.Key, item.Value)
        )
        |> Seq.toArray |> Array.sortBy fst

    /// Returns text ranges from all groups in interval <startLine, endLine>.
    member _.GetInIntervalFromAllGroups startLine endLine =
        myTextRanges |> Seq.map (
            fun item -> (getColorIndex item.Key, item.Value)
        )
        |> Seq.map (
            fun (_colorIndex, textRanges) ->
                (_colorIndex, getFromInterval textRanges startLine endLine)
        )
        |> Seq.toArray |> Array.sortBy fst

    /// Searches for regex/regexObject going through lines.
    member internal this.SearchNormal lines regex regexObject =
        myLastRegex <- Some regex
        myIsCleared <- false

        this.SearchAux lines regex regexObject

    member internal this.SearchAux lines regex regexObject =
        if isMultiLineRegex regex then
            this.SearchMultiLine lines regexObject
        else
            this.SearchSingleLine lines regexObject

        if myWarnIfNoMatchFound && this.GetMainGroupCount () = 0 then
            myUserMessages.RegisterMessage WARNING_NO_MATCH_FOUND

    member private _.SearchSingleLine (lines: Lines) (regexObject: Regex) =
        myTextRanges <- makeTextRangesGroups ()

        let slr = SingleLineRegex.AddMatchesAsTextRanges regexObject
        slr.Init myTextRanges

        for i, chars in lines |> Seq.indexed do
            slr.ProcessLine i 0 chars.Length true chars
                |> ignore
        slr.FinishProcessing ()

    member private _.SearchMultiLine (lines: Lines) (regexObject: Regex) =
        myTextRanges <- makeTextRangesGroups ()

        let mlr = MultiLineRegex.AddMatchesAsTextRanges (
            myUserMessages, regexObject, myTextRanges
        )

        for i, chars in lines |> Seq.indexed do
            mlr.ProcessLine i 0 chars.Length true chars
                |> ignore
        mlr.FinishProcessing ()

    /// Searches for the regex used in the last call to SearchNormal going through lines.
    member internal this.ReSearchNormal lines =
        match myLastRegex with
        | Some regex ->
            myIsCleared <- false

            this.SearchAux lines regex (makeRegexObject regex)
        | None ->
            myUserMessages.RegisterMessage ERROR_NOTHING_TO_SEARCH_FOR

    /// Clears text ranges from the last call to Search.
    member internal this.ClearSearchNormal () =
        myIsCleared <- true

        this.ClearSearchAux ()

    member internal _.ClearSearchAux () =
        myTextRanges <- makeTextRangesGroups ()

    /// Updates text ranges after reloading the file, according the previous state.
    member this.UpdateAfterReload () =
        if not this.IsCleared then
            this.ReSearch ()

    // virtual

    /// Searches for regex.
    abstract member Search:
        regex: string
     -> unit

    override this.Search (regex: string) =
        match tryMakeRegexObject regex with
        | Ok regexObject ->
            this.SearchNormal myLines regex regexObject

        | Error e ->
            myUserMessages.RegisterMessage (makeErrorMessage e)

    /// Searches for the regex used in the last call to Search (or Extract).
    abstract member ReSearch:
        unit -> unit

    override this.ReSearch () =
        this.ReSearchNormal myLines

    /// Clears text ranges from the last call to Search.
    abstract member ClearSearch:
        unit -> unit

    override this.ClearSearch () =
        this.ClearSearchNormal ()

    // IMatchRanges

    interface IMatchRanges with

        member this.GetInIntervalFromAllGroups startLine endLine =
            this.GetInIntervalFromAllGroups startLine endLine

        member this.GetMainGroupCount ()   = this.GetMainGroupCount ()
        member this.GetAllFromMainGroup () = this.GetAllFromMainGroup ()
