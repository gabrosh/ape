module MatchRangesExtract

open System.Collections.Generic

open BinarySearch
open DataTypes
open IMatchRanges
open LineMatchClassifier
open MatchRanges
open Position
open RegexUtils
open TextRanges
open UserMessages

/// MatchRangesExtract adds the "extract" functionality to MatchRanges.

let private lineToExtractLine (lineExtractToLine: ResizeArray<int>) line =
    let result =
        findFirstEqOrGtInSortedArray (compareTo line) lineExtractToLine

    match result with
    | Some m -> m
    | _      -> lineExtractToLine.Count - 1

type MatchRangesExtract (
    myUserMessages:    UserMessages,
    myLines:           Lines,
    inLastRegex:       string option,
    inIsCleared:       bool,
    inTextRanges:      Dictionary<string, TextRanges>,
    myLinesExtract:    Lines,
    inExtractOnConstr: bool
) as this =
    inherit MatchRanges (
        myUserMessages, myLines, inLastRegex, inIsCleared, inTextRanges
    )

    /// Translates index in myLinesExtract to index in myLines.
    let myLineExtractToLine = ResizeArray<int> ()

    let mutable myLastRegexExtract =
        if inExtractOnConstr then this.LastRegex else None

    let mutable myIsClearedExtract =
        if inExtractOnConstr then this.IsCleared else true

    let mutable myIsSearchInExtract =
        false

    // public properties

    member _.LastRegexExtract = myLastRegexExtract
    member _.IsClearedExtract = myIsClearedExtract

    // init mechanism

    /// Initializes the instance after its construction.
    member this.Init () =
        this.Update ()

    // virtual

    override this.Search (regex: string) =
        match tryMakeRegexObject regex with
        | Ok regexObject ->
            myIsSearchInExtract <- true

            this.SearchNormal myLinesExtract regex regexObject

        | Error e ->
            myUserMessages.RegisterMessage (makeErrorMessage e)

    override this.ReSearch () =
        myIsSearchInExtract <- true

        this.ReSearchNormal myLinesExtract

    override this.ClearSearch () =
        this.ClearSearchNormal ()

    // extract

    /// Extracts lines from myLines according to given regex.
    member this.Extract (regex: string) =
        match tryMakeRegexObject regex with
        | Ok regexObject ->
            myLastRegexExtract  <- Some regex
            myIsClearedExtract  <- false
            myIsSearchInExtract <- false

            this.SearchNormal myLines regex regexObject
            this.Update ()

        | Error e ->
            myUserMessages.RegisterMessage (makeErrorMessage e)

    /// Extracts lines from myLines according to the regex used in the last call to Search or Extract.
    member this.ReExtract () =
        match this.LastRegex with
        | Some regex ->
            myLastRegexExtract  <- Some regex
            myIsClearedExtract  <- false
            myIsSearchInExtract <- false

            this.SearchNormal myLines regex (makeRegexObject regex)
            this.Update ()

        | None ->
            myUserMessages.RegisterMessage ERROR_NOTHING_TO_SEARCH_FOR

    /// Clears the extract and searches for the regex used in the last call to Search or Extract.
    member this.ClearExtract () =
        myIsClearedExtract <- true

        this.ClearSearchAux ()
        this.Update ()

        if not this.IsCleared then
            match this.LastRegex with
            | Some regex ->
                this.SearchAux myLinesExtract regex (makeRegexObject regex)

            | None ->
                // (not IsCleared) => (LastRegex <> None)
                invalidOp "Broken invariant"

    /// Updates the extract after reloading the file, according to the previous state.
    member this.UpdateAfterReload () =
        if not this.IsClearedExtract then
            match this.LastRegexExtract with
            | Some regex ->
                this.SearchAux myLines regex (makeRegexObject regex)
                this.Update ()

                if not this.IsCleared then
                    if myIsSearchInExtract then
                        this.ReSearch ()
                else
                    this.ClearSearchAux ()

            | None ->
                // (not IsClearedExtract) => (LastRegexExtract <> None)
                invalidOp "Broken invariant"
        else
            this.ClearSearchAux ()
            this.Update ()

            if not this.IsCleared then
                this.ReSearch ()

    // auxiliary

    member private this.Update () =
        if not this.IsClearedExtract && this.GetMainGroupCount () <> 0 then
            this.UpdateForSomeMatches ()

            this.TextRanges <- this.GetTextRangesExtract ()
        else
            this.UpdateForNoMatches ()

    member private this.UpdateForSomeMatches () =
        myLinesExtract.Clear ()
        myLineExtractToLine.Clear ()

        let matchRanges = this.GetAllFromAllGroups ()

        let classifier = LineMatchClassifier matchRanges

        for line, chars in this.SearchedLines |> Seq.indexed do
            let lineClass = classifier.Classify line
            match lineClass with
            | LineMatch _ ->
                myLinesExtract.Add chars
                myLineExtractToLine.Add line
            | LineOther   ->
                ()

    member private this.UpdateForNoMatches () =
        myLinesExtract.Clear ()
        myLineExtractToLine.Clear ()

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

    member private this.TransformTextRange textRange =
        {
            first = {
                line = this.LineToLineExtract textRange.first.line
                char = textRange.first.char
            }
            last  = {
                line = this.LineToLineExtract textRange.last.line
                char = textRange.last.char
            }
        }

    member _.LineExtractToLine line =
        if myLineExtractToLine.Count <> 0 then
            myLineExtractToLine[line]
        else
            line

    member _.LineToLineExtract line =
        if myLineExtractToLine.Count <> 0 then
            lineToExtractLine myLineExtractToLine line
        else
            line

    // IMatchRanges

    interface IMatchRanges with

        member this.GetInIntervalFromAllGroups startLine endLine =
            this.GetInIntervalFromAllGroups startLine endLine

        member this.GetMainGroupCount ()   = this.GetMainGroupCount ()
        member this.GetAllFromMainGroup () = this.GetAllFromMainGroup ()
