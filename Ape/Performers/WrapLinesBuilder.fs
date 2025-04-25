module WrapLinesBuilder

open DataTypes

/// Character in context of a rendered line.
type LineChar = {
    cOpt:       CharOption
    char:       int
    lineColumn: int
    row:        int
    column:     int
}

type WrapLinesBuilder (myTextWidth: int) =
    let mutable myChar       = 0
    let mutable myLineColumn = 0
    let mutable myRow        = 0
    let mutable myColumn     = 0

    member _.Char       = myChar
    member _.LineColumn = myLineColumn
    member _.Column     = myColumn

    member this.GetNextItems cOpt count =
        if myColumn + count <= myTextWidth then
            // ordinary character or 1-space tabulator
            let items  = this.MakeArray cOpt count
            myLineColumn <- myLineColumn + count
            myColumn     <- myColumn + count

            items
        else
            let count1 = myTextWidth - myColumn
            let count2 = count - count1

            // 1st group of tabulator spaces
            let items1 = this.MakeArray cOpt count1
            myLineColumn <- myLineColumn + count1
            myRow        <- myRow + 1
            myColumn     <- 0
            // 2nd group of tabulator spaces
            let items2 = this.MakeArray cOpt count2
            myLineColumn <- myLineColumn + count2
            myColumn     <- myColumn + count2

            Array.append items1 items2

    member _.MakeArray cOpt count =
        Array.init count (
            fun i -> {
                cOpt       = cOpt
                char       = myChar
                lineColumn = myLineColumn + i
                row        = myRow
                column     = myColumn     + i
            }
        )

    member _.MakeFillArray cOpt count =
        Array.init count (
            fun i -> {
                cOpt       = cOpt
                char       = myChar       - 1
                lineColumn = myLineColumn - 1
                row        = myRow
                column     = myColumn     + i
            }
        )

    member _.NextChar () =
        myChar <- myChar + 1

    member _.NextRow () =
        myRow    <- myRow + 1
        myColumn <- 0
