module NoWrapLinesBuilder

open DataTypes

/// Character in context of a rendered line.
type LineChar = {
    cOpt:   CharOption
    char:   int
    column: int
}

type NoWrapLinesBuilder () =
    let mutable myChar   = 0
    let mutable myColumn = 0

    member _.Column = myColumn

    member this.GetNextItems cOpt count =
        let items = this.MakeArray cOpt count
        myColumn <- myColumn + count
        items

    member _.MakeArray cOpt count =
        Array.init count (
            fun i -> {
                cOpt   = cOpt
                char   = myChar
                column = myColumn + i
            }
        )

    member _.NextChar () =
        myChar <- myChar + 1
