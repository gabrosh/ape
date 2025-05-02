module LineMatchClassifier

open TextRange
open TextRanges

type LineClass =
    | LineMatch of group: int
    | LineOther

let private isRangeBeforeLine (range: TextRange) (line: int) =
    range.last.line < line

let private isLineInRange (line: int) (range: TextRange) =
    line >= range.first.line && line <= range.last.line

/// LineMatchClassifier classifies given lines into corresponding line classes.
/// Individual classification methods are defined as public for testing purposes.

type LineMatchClassifier (
    myMatchRanges: (int * TextRanges) array
) =
    let myMatchIndexes = Array.init myMatchRanges.Length (fun _ -> 0)

    /// Returns the class of the character on given line.
    member this.Classify line =
        match this.IsInMatch line with
        | Some i -> LineMatch i
        | None   -> LineOther

    /// If given line is within any match of object's match groups,
    /// it returns index of the match group, otherwise it returns -1. 
    /// Lines in subsequent calls to this method must not decrease.
    member this.IsInMatch line =
        // groupIndex
        let mutable i = myMatchRanges.Length - 1
        
        while i >= 0 && not (this.IsInMatchGroup i line) do
            i <- i - 1
            
        if i <> -1 then
            Some (fst myMatchRanges[i])
        else
            None

    /// Returns true if given line is within any match of given group.
    /// Lines in subsequent calls to this method must not decrease.
    member private _.IsInMatchGroup groupIndex line =
        let matchRanges = snd myMatchRanges[groupIndex]

        let stopIndex = matchRanges.Count
        let mutable i = myMatchIndexes[groupIndex]

        while i < stopIndex && isRangeBeforeLine matchRanges[i] line do
            i <- i + 1

        myMatchIndexes[groupIndex] <- i

        i < stopIndex && isLineInRange line matchRanges[i]
