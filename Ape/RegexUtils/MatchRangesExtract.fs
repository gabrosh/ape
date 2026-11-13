module MatchRangesExtract

open System.Collections.Generic

open BinarySearch
open DataTypes
open LineMatchClassifier
open MatchRanges
open Position
open RegexUtils
open TextRanges
open UserMessages

type ExtractState = {
    mutable lastRegex: string option
    mutable isCleared: bool
    mutable isSearchInExtract: bool
}

type ExtractStates = {
    states: ExtractState array
    parent: SearchStates
}
with
    member this.Item
        with get index       = this.states[index]
        and  set index value = this.states[index] <- value

    member this.ActiveState = this.states[this.parent.activeStateInd]
          
    member this.AreAllCleared =
        this.states[0].isCleared && this.states[1].isCleared

let ExtractStates_Default parent = {
    states = [|
        {
            lastRegex = None
            isCleared = true
            isSearchInExtract = false
        }
        {
            lastRegex = None
            isCleared = true
            isSearchInExtract = false
        }
    |]
    parent = parent
}

/// MatchRangesExtract adds the "extract" functionality to MatchRanges.

let private lineToExtractLine (lineExtractToLine: ResizeArray<int>) line =
    let result =
        findFirstEqOrGtInSortedArray (compareTo line) lineExtractToLine

    match result with
    | Some m -> m
    | _      -> lineExtractToLine.Count - 1

type MatchRangesExtract (
    myUserMessages: UserMessages,
    myLines:        Lines,
    myLinesExtract: Lines,
    inSearchStates: SearchStates
) as thisCtor =
    inherit MatchRanges (myUserMessages, myLines, inSearchStates)

    let myExtractStates = ExtractStates_Default inSearchStates

    /// Translates index in myLinesExtract to index in myLines.
    let myLineExtractToLine = ResizeArray<int> ()

    do
        thisCtor.Update ()

    new (
        inUserMessages: UserMessages, inLines: Lines, inLinesExtract: Lines
    ) =
        MatchRangesExtract (
            inUserMessages, inLines, inLinesExtract,
            SearchStates_Default ()
        )

    new (
        inUserMessages: UserMessages, inLines: Lines, inLinesExtract: Lines,
        inMatchRanges: MatchRanges
    ) =
        MatchRangesExtract (
            inUserMessages, inLines, inLinesExtract,
            inMatchRanges.SearchStates.Clone ()
        )

    // public properties

    member _.AreAllClearedExtract = myExtractStates.AreAllCleared

    // virtual

    override this.Search (regex: string) =
        match tryMakeRegexObject regex with
        | Ok regexObject ->
            myExtractStates.ActiveState.isSearchInExtract <- true

            this.SearchNormal this.SearchStates.ActiveState myLinesExtract regex regexObject
        | Error e ->
            myUserMessages.RegisterMessage (makeErrorMessage e)

    override this.ClearSearch () =
        myExtractStates.ActiveState.isSearchInExtract <- true

        this.ClearSearchNormal this.SearchStates.ActiveState
    
    override this.ReSearch () =
        myExtractStates.ActiveState.isSearchInExtract <- true

        this.ReSearchNormal this.SearchStates.ActiveState myLinesExtract
    
    override this.UpdateAfterChange () =
        if not this.AreAllClearedExtract then
            this.HandleSearchBeforeUpdate (this.GetStates 0)
            this.HandleSearchBeforeUpdate (this.GetStates 1)

            this.Update ()
            
            this.HandleSearchAfterUpdate (this.GetStates 0)
            this.HandleSearchAfterUpdate (this.GetStates 1)
        else
            this.ClearSearchAux this.SearchStates[0]
            this.ClearSearchAux this.SearchStates[1]

            this.Update ()

            this.SearchAuxIfNotCleared this.SearchStates[0] myLinesExtract
            this.SearchAuxIfNotCleared this.SearchStates[1] myLinesExtract

    // extract

    /// Extracts lines from myLines according to given regex.
    member this.Extract (regex: string) =
        match tryMakeRegexObject regex with
        | Ok regexObject ->
            this.SearchStates.ActiveState.lastRegex <- Some regex
            this.SearchStates.ActiveState.isCleared <- false

            this.SearchAuxForExtract
                (this.GetStates this.SearchStates.activeStateInd) myLines
                regex regexObject                
            this.SearchAuxIfNotClearedForExtract
                (this.GetStates this.SearchStates.inactiveStateInd) myLines
            
            this.Update ()
        | Error e ->
            myUserMessages.RegisterMessage (makeErrorMessage e)
            
    /// Extracts lines from myLines according to the regex used in the last call to Search or Extract.
    member this.ReExtract () =
        if not this.AreAllClearedSearch then
            this.SearchAuxIfNotClearedForExtract (this.GetStates 0) myLines
            this.SearchAuxIfNotClearedForExtract (this.GetStates 1) myLines

            this.Update ()
        else
            myUserMessages.RegisterMessage ERROR_NOTHING_TO_SEARCH_FOR            

    /// Clears the extract and searches for the regex used in the last calls to Search or Extract.
    member this.ClearExtract () =
        myExtractStates[0].isCleared <- true
        myExtractStates[1].isCleared <- true

        this.ClearSearchAux this.SearchStates[0]
        this.ClearSearchAux this.SearchStates[1]

        this.Update ()

        this.SearchAuxIfNotCleared this.SearchStates[0] myLinesExtract
        this.SearchAuxIfNotCleared this.SearchStates[1] myLinesExtract

    // auxiliary

    member private this.GetStates statInd =
        this.SearchStates[statInd], myExtractStates[statInd]
            
    member private this.SearchAuxIfNotClearedForExtract (searchState, extractState) lines =
        if not searchState.isCleared then
            // (not isCleared) => (lastRegex <> None)
            let regex = searchState.lastRegex |> Option.get
            let regexObject = makeRegexObject regex

            extractState.lastRegex <- Some regex
            extractState.isCleared <- false
            extractState.isSearchInExtract <- false

            this.SearchAux searchState lines regex regexObject
            
    member private this.SearchAuxForExtract (searchState, extractState) lines regex regexObject =
        extractState.lastRegex <- Some regex
        extractState.isCleared <- false
        extractState.isSearchInExtract <- false

        this.SearchAux searchState lines regex regexObject
            
    member private this.HandleSearchBeforeUpdate (searchState, extractState) =
        if not extractState.isCleared then
            // (not isCleared) => (lastRegex <> None)
            let regex = extractState.lastRegex |> Option.get

            this.SearchAux searchState myLines regex (makeRegexObject regex)
        else
            this.ClearSearchAux searchState
    
    member private this.HandleSearchAfterUpdate (searchState, extractState) =
        if extractState.isSearchInExtract then
            if not searchState.isCleared then
                // (not isCleared) => (lastRegex <> None)
                let regex = searchState.lastRegex |> Option.get

                this.SearchAux searchState myLinesExtract regex (makeRegexObject regex)
            else
                this.ClearSearchAux searchState
                        
    member private this.Update () =
        if not this.AreAllClearedExtract && (
            this.GetMainGroupCountAux this.SearchStates[0] <> 0 ||
            this.GetMainGroupCountAux this.SearchStates[1] <> 0
        ) then
            this.UpdateForSomeMatches ()

            this.SearchStates[0].textRanges <-
                this.GetTextRangesForExtract this.SearchStates[0].textRanges
            this.SearchStates[1].textRanges <-
                this.GetTextRangesForExtract this.SearchStates[1].textRanges
        else
            this.UpdateForNoMatches ()

    member private this.UpdateForSomeMatches () =
        myLinesExtract.Clear ()
        myLineExtractToLine.Clear ()

        let matchRanges = this.GetAllFromAllGroups ()

        let classifier = LineMatchClassifier matchRanges

        for line, chars in myLines |> Seq.indexed do
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

        myLinesExtract.AddLines myLines

    /// Gets TextRanges corresponding to myLinesExtract.
    member private this.GetTextRangesForExtract textRanges =
        let result = Dictionary<string, TextRanges> ()

        for item in textRanges do
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
