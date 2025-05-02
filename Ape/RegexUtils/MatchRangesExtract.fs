module MatchRangesExtract

open System.Collections.Generic

open DataTypes
open IMatchRanges
open LineMatchClassifier
open MatchRanges
open Position
open TextRanges
open UserMessages

type private Regex = System.Text.RegularExpressions.Regex

/// MatchRangesExtract adds the "extract" functionality to MatchRanges.

type MatchRangesExtract (
    inUserMessages: UserMessages,
    inLines:        Lines,
    myLinesExtract: Lines,
    inLastRegex:    string option,
    inWasCleared:   bool,
    inTextRanges:   Dictionary<string, TextRanges>
) =
    inherit MatchRanges (
        inUserMessages, inLines, inLastRegex, inWasCleared, inTextRanges
    )

    /// Translates index in inLines to index in myLinesExtract.
    let myLineToLineExtract = Dictionary<int, int> ()

    /// Initializes the instance after its construction.
    member this.Init () =
        this.Update ()

    // virtual

    override this.SearchSingleLine (regexObject: Regex) =
        base.SearchSingleLine regexObject
        this.Update ()

    override this.SearchMultiLine (regexObject: Regex) =
        base.SearchMultiLine regexObject
        this.Update ()

    override this.Clear () =
        base.Clear ()
        this.Update ()

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
