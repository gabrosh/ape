module Lines

open System.Collections.Immutable

open SkipList

type Chars = ImmutableArray<char>

type UndoNodes = ResizeArray<ResizeArray<Chars>>

let private levelsCount = 10
let private maxSize = 1000

type Lines () =
    inherit SkipList<Chars> (levelsCount, maxSize)

    /// Creates a Lines instance from items.
    new (items: Chars seq) as this =
        Lines ()
        then
            let items = ResizeArray<Chars> items
            if items.Count > 0 then
                this.AddItems items items.Count

    /// Creates a Lines instance from items with given itemsCount.
    new (items: Chars seq, itemsCount: int) as this =
        Lines ()
        then
            if itemsCount > 0 then
                this.AddItems items itemsCount
    
    /// Adds items to the end.
    member this.AddLines (items: Lines) =
        if items.Count > 0 then
            this.AddItems items items.Count
    
    /// Adds items to the end.
    member this.AddSeq (items: Chars seq) =
        let items = ResizeArray<Chars> items
        if items.Count > 0 then
            this.AddItems items items.Count
    
    /// Inserts items at index.
    member this.InsertLines (index: int) (items: Lines) =
        if items.Count > 0 then
            this.InsertItems index items items.Count
    
    /// Inserts items at index.
    member this.InsertSeq (index: int) (items: Chars seq) =
        let items = ResizeArray<Chars> items
        if items.Count > 0 then
            this.InsertItems index items items.Count
    
    /// Returns count items starting at index as a new Lines instance.
    member this.GetRange (index: int) (count: int) =
        Lines (this.GetRangeSeq index count, count)
