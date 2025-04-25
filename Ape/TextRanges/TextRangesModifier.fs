module TextRangesModifier

open Position

type ITextRange =
    abstract member First: Position with get
    abstract member Last:  Position with get

    abstract member PrepareItem: newLinesDelta: int
        -> ITextRange

    abstract member ApplyInsert: insertSpec: InsertSpec
        -> ITextRange
    abstract member ApplyDelete: deleteSpec: DeleteSpec
        -> bool * ITextRange
    abstract member ApplyOneLineInsert: insertSpec: OneLineInsertSpec
        -> ITextRange
    abstract member ApplyOneLineDelete: deleteSpec: OneLineDeleteSpec
        -> bool * ITextRange

/// TextRangesModifier holds ITextRange items and modifies them one by
/// one according to given modifying specifications. Before starting
/// the modification process, the items must be sorted.

type TextRangesModifier<'T when 'T :> ITextRange> (myItems: ResizeArray<'T>) =

    // reset by StartApplyingModifications
    let mutable myStartIndex    = 0
    let mutable myStopIndex     = -1
    let mutable myNewLinesDelta = 0
    let mutable myAreEliminated = Array.empty

    /// Returns all items in repository.
    member _.Items = myItems

    /// Starts the process of applying modifications.
    member _.StartApplyingModifications () =
        myStartIndex    <- 0
        myStopIndex     <- -1
        myNewLinesDelta <- 0
        myAreEliminated <- Array.zeroCreate myItems.Count

    /// Stops the process of applying modifications.
    member this.StopApplyingModifications () =
        this.AssureItemsPreparedAll ()

        myStartIndex    <- 0
        myStopIndex     <- -1
        myNewLinesDelta <- 0
        myAreEliminated <- Array.empty

    /// Returns true if item with given index was eliminated after calling StartApplying.
    member _.IsEliminated index =
        myAreEliminated[index]

    /// Applies insert operation to all items in repository.
    member this.ApplyInsert (insertSpec: InsertSpec) =
        let specFirst    = insertSpec.target
        let specLastLine = insertSpec.target.line + insertSpec.newLines

        let start, stop = this.GetStartAndStop specFirst specLastLine

        for i = start to stop do
            myItems[i] <-
                (myItems[i].ApplyInsert insertSpec) :?> 'T

        myNewLinesDelta <- myNewLinesDelta + insertSpec.newLines

    /// Applies delete operation to all items in repository.
    member this.ApplyDelete (deleteSpec: DeleteSpec) =
        let specFirst    = deleteSpec.first
        let specLastLine = deleteSpec.rightKept.line

        let start, stop = this.GetStartAndStop specFirst specLastLine

        for i = start to stop do
            let isEliminated, item =
                myItems[i].ApplyDelete deleteSpec
            myItems[i]         <- item :?> 'T
            myAreEliminated[i] <- myAreEliminated[i] || isEliminated

        myNewLinesDelta <- myNewLinesDelta - deleteSpec.NewLines

    /// Applies one-line insert operation to all items in repository.
    member this.ApplyOneLineInsert (insertSpec: OneLineInsertSpec) =
        let specFirst    = { line = insertSpec.line; char = insertSpec.target }
        let specLastLine = insertSpec.line

        let start, stop = this.GetStartAndStop specFirst specLastLine

        for i = start to stop do
            myItems[i] <-
                (myItems[i].ApplyOneLineInsert insertSpec) :?> 'T

    /// Applies one-line delete operation to all items in repository.
    member this.ApplyOneLineDelete (deleteSpec: OneLineDeleteSpec) =
        let specFirst    = { line = deleteSpec.line; char = deleteSpec.first }
        let specLastLine = deleteSpec.line

        let start, stop = this.GetStartAndStop specFirst specLastLine

        for i = start to stop do
            let isEliminated, item =
                myItems[i].ApplyOneLineDelete deleteSpec
            myItems[i]         <- item :?> 'T
            myAreEliminated[i] <- myAreEliminated[i] || isEliminated

    member private this.GetStartAndStop specFirst specLastLine =
        this.UpdateStopIndex specLastLine
        this.UpdateStartIndex specFirst

        (myStartIndex, myStopIndex)

    member private this.UpdateStopIndex specLastLine =
        let maxIndex = myItems.Count - 1
        let m = specLastLine - myNewLinesDelta

        let mutable i = myStopIndex
        while i < maxIndex && myItems[i + 1].First.line <= m do
            i <- i + 1
            this.PrepareItem i
        // This is the last item with first.line <= specLastLine.
        // Searching starts at myStopIndex.
        myStopIndex <- i

    member private _.UpdateStartIndex specFirst =
        let minIndex = 0
        let maxIndex = myStopIndex

        let mutable i = myStartIndex
        while i > minIndex && myItems[i].Last >= specFirst do
            i <- i - 1
        while i < maxIndex && myItems[i].Last < specFirst do
            i <- i + 1
        // This is the first item with last >= specFirst.
        // Searching starts at myStartIndex and stops at 0 or myStopIndex.
        myStartIndex <- i

    /// Assures that items up to currentIndex are prepared to be used.
    member this.AssureItemsPreparedUpTo currentIndex =
        let maxIndex = myItems.Count - 1

        let mutable i = myStopIndex
        while i < maxIndex && i < currentIndex do
            i <- i + 1
            this.PrepareItem i
        // Index of this item is currentIndex or higher.
        myStopIndex <- i

    /// Assures that all items are prepared to be used.
    member this.AssureItemsPreparedAll () =
        this.AssureItemsPreparedUpTo (myItems.Count - 1)

    member private _.PrepareItem index =
        myItems[index] <-
            (myItems[index].PrepareItem myNewLinesDelta) :?> 'T
