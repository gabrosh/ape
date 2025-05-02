module IMatchRanges

open TextRanges

type IMatchRanges =

    abstract member WasCleared: bool
        with get

    abstract member GetInIntervalFromAllGroups:
        startLine: int -> endLine: int
     -> (int * TextRanges) array

    abstract member GetMainGroupCount:
        unit
     -> int

    abstract member GetAllFromMainGroup:
        unit
     -> TextRanges

    abstract member ReSearch:
        unit -> unit
