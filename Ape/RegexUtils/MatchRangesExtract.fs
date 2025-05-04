module MatchRangesExtract

open System.Collections.Generic

open DataTypes
open IMatchRanges
open LineMatchClassifier
open MatchRanges
open Position
open TextRanges
open UserMessages

/// MatchRangesExtract adds the "extract" functionality to MatchRanges.

type MatchRangesExtract (
    myUserMessages: UserMessages,
    myLines:        Lines,
    myLinesExtract: Lines,
    inLastRegex:    string option,
    inWasCleared:   bool,
    inTextRanges:   Dictionary<string, TextRanges>
) =
    inherit MatchRanges (
        myUserMessages, myLines, inLastRegex, inWasCleared, inTextRanges
    )

    /// Translates index in inLines to index in myLinesExtract.
    let myLineToLineExtract = Dictionary<int, int> ()

    let mutable myLastRegexExtract  = None
    let mutable myWasClearedExtract = true

    // public properties

    member _.LastRegexExtract  = myLastRegexExtract
    member _.WasClearedExtract = myWasClearedExtract

    /// Initializes the instance after its construction.
    member this.Init () =
        myLastRegexExtract  <- this.LastRegex
        myWasClearedExtract <- this.WasCleared

        this.Update ()

    // virtual

    override this.Search (regex: string) =
        this.SearchNormal myLinesExtract regex

    override this.ReSearch () =
        this.ReSearchNormal myLinesExtract

    override this.ClearSearch () =
        this.ClearSearchNormal ()

    // extract

    member this.Extract (regex: string) =
        myLastRegexExtract  <- Some regex
        myWasClearedExtract <- false

        this.SearchNormal myLines regex
        this.Update ()

    member this.ReExtract () =
        match this.LastRegex with
        | Some regex ->
            myLastRegexExtract  <- Some regex
            myWasClearedExtract <- false

            this.SearchNormal myLines regex
            this.Update ()
        | None ->
            myUserMessages.RegisterMessage ERROR_NOTHING_TO_SEARCH_FOR

    member this.ReExtractBeforeReSearch () =
        match this.LastRegexExtract with
        | Some regex ->
            myLastRegexExtract  <- Some regex
            myWasClearedExtract <- false

            this.SearchAux myLines regex
            this.Update ()
        | None ->
            myUserMessages.RegisterMessage ERROR_NOTHING_TO_SEARCH_FOR            

    member this.ClearExtract () =
        myWasClearedExtract <- true

        this.ClearSearchAux ()
        this.Update ()

        if not this.WasCleared then
            match this.LastRegex with
            | Some regex -> 
                this.SearchNormal myLinesExtract regex
            | None ->
                myUserMessages.RegisterMessage ERROR_NOTHING_TO_SEARCH_FOR            

    // auxiliary

    member private this.Update () =
        if this.GetMainGroupCount () <> 0 then
            this.UpdateForSomeMatches ()

            this.TextRanges <- this.GetTextRangesExtract ()
        else
            this.UpdateForNoMatches ()

    member private this.UpdateForSomeMatches () =
        myLinesExtract.Clear ()
        myLineToLineExtract.Clear ()

        let matchRanges = this.GetAllFromAllGroups ()

        let classifier = LineMatchClassifier matchRanges

        for line, chars in this.SearchedLines |> Seq.indexed do
            let lineClass = classifier.Classify line
            match lineClass with
            | LineMatch _ ->
                let lineExtract = myLinesExtract.Count
                myLineToLineExtract[line] <- lineExtract
                myLinesExtract.Add chars
            | LineOther   ->
                ()

    member private this.UpdateForNoMatches () =
        myLinesExtract.Clear ()
        myLineToLineExtract.Clear ()

        myLinesExtract.AddRange this.SearchedLines

    /// Returns TextRanges corresponding to myLinesExtract.
    member private this.GetTextRangesExtract () =
        let result = Dictionary<string, TextRanges> ()

        for item in this.TextRanges do
            let name, textRanges = item.Key, item.Value
            let newTextRanges = TextRanges textRanges.Count
            result[name] <- newTextRanges

            for textRange in textRanges do
                newTextRanges.Add (this.TransformTextRange textRange)

        result

    member private _.TransformTextRange textRange =
        {
            first = {
                line = myLineToLineExtract[textRange.first.line]
                char = textRange.first.char
            }
            last  = {
                line = myLineToLineExtract[textRange.last.line]
                char = textRange.last.char
            }
        }

    // IMatchRanges

    interface IMatchRanges with

        member this.GetInIntervalFromAllGroups startLine endLine =
            this.GetInIntervalFromAllGroups startLine endLine

        member this.GetMainGroupCount ()   = this.GetMainGroupCount ()
        member this.GetAllFromMainGroup () = this.GetAllFromMainGroup ()
