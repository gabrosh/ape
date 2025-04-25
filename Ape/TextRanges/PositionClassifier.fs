module PositionClassifier

open Position
open TextRange
open TextRanges

type PositionClass =
    | PositionMainCursor
    | PositionNonMainCursor
    | PositionSelection
    | PositionMatch of group: int
    | PositionOther

let private isRangeBeforePosition (range: TextRange) (position: Position) =
    range.last < position

let private isPositionInRange (position: Position) (range: TextRange) =
    position >= range.first && position <= range.last

/// PositionClassifier classifies given positions into corresponding position classes.
/// Individual classification methods are defined as public for testing purposes.

type PositionClassifier (
    myMainCursor:      Position,
    myCursors:         ResizeArray<Position>,
    mySelectionRanges: ResizeArray<TextRange>,
    myMatchRanges:     (int * TextRanges) array
) =
    let mutable myCursor         = 0
    let mutable mySelectionIndex = 0

    let myMatchIndexes = Array.init myMatchRanges.Length (fun _ -> 0)

    /// Returns the class of the character on given position.
    member this.Classify line char_ =
        let position = { line = line; char = char_ }

        if   this.IsAtMainCursor position then PositionMainCursor
        elif this.IsAtCursor     position then PositionNonMainCursor
        elif this.IsInSelection  position then PositionSelection
        else
            match this.IsInMatch position with
            | Some i -> PositionMatch i
            | None   -> PositionOther

    /// Returns true if given position is at the main cursor position.
    member _.IsAtMainCursor position =
        position = myMainCursor

    /// Returns true if given position is cursor position.
    /// Positions in subsequent calls to this method must not decrease.
    member _.IsAtCursor position =
        let stopIndex = myCursors.Count
        let mutable i = myCursor

        while i < stopIndex && myCursors[i] < position do
            i <- i + 1

        myCursor <- i

        i < stopIndex && position = myCursors[i]

    /// Returns true if given position is within any selection.
    /// Positions in subsequent calls to this method must not decrease.
    member _.IsInSelection position =
        let stopIndex = mySelectionRanges.Count
        let mutable i = mySelectionIndex

        while i < stopIndex && isRangeBeforePosition mySelectionRanges[i] position do
            i <- i + 1

        mySelectionIndex <- i

        i < stopIndex && isPositionInRange position mySelectionRanges[i]

    /// If given position is within any match of object's match groups,
    /// it returns index of the match group, otherwise it returns -1. 
    /// Positions in subsequent calls to this method must not decrease.
    member this.IsInMatch position =
        let mutable i = myMatchRanges.Length - 1
        
        while i >= 0 && not (this.IsInMatchGroup i position) do
            i <- i - 1
            
        if i <> -1 then
            Some (fst myMatchRanges[i])
        else
            None

    /// Returns true if given position is within any match of given group.
    /// Positions in subsequent calls to this method must not decrease.
    member private _.IsInMatchGroup groupIndex position =
        let matchRanges = snd myMatchRanges[groupIndex]

        let stopIndex = matchRanges.Count
        let mutable i = myMatchIndexes[groupIndex]

        while i < stopIndex && isRangeBeforePosition matchRanges[i] position do
            i <- i + 1

        myMatchIndexes[groupIndex] <- i

        i < stopIndex && isPositionInRange position matchRanges[i]
