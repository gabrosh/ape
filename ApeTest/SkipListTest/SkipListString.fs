module SkipListString

open System
open System.Text

open SkipList
open SkipListAux

let private printNodeFun level accSize (node: Node<char>) =
    let size     = node.sizes[level]
    let chunkStr = String.Concat node.chunk
    Console.WriteLine (
        $"level {level,-2} accSize {accSize,-2} " +
        $"size {size,-2} : {chunkStr}"
    )

let private emptyPrintNodeFun _level _accSize (_node: Node<char>) =
    ()

type SkipListString (levelsCount: int, maxSize: int) =
    inherit SkipList<char> (levelsCount, maxSize)

    let maxLevel = levelsCount - 1

    member this.InsertString index (s: string) =
        let array = (ResizeArray s)
        this.InsertItems index array array.Count

    member this.AddString (s: string) =
        let array = (ResizeArray s)
        this.AddItems array array.Count

    member this.AsString () =
        String.Concat (this.GetRangeSeq 0 this.Count)

    member this.GetRepr () =
        let sb = StringBuilder ()

        let mutable node = ValueSome this.Start
        let mutable accSize = 0

        while node.IsSome do
            let node' = node.Value

            sb.Append $"accSize {accSize,-2} : "
                |> ignore

            for level = maxLevel downto 0 do
                sb.Append (
                    if node'.nexts[level].IsSome then
                        "+"
                    else
                        " "
                ) |> ignore

                let size = node'.sizes[level]
                sb.Append $"{size:D2} "
                    |> ignore

            let chunkStr = String.Concat node'.chunk
            sb.AppendLine $": {chunkStr}"
                |> ignore

            node    <- node'.nexts[0]
            accSize <- accSize + node'.sizes[0]

        sb.ToString ()

    member this.CheckConsistency () =
        Console.WriteLine (this.GetRepr ())

        let startNode, endNode = ValueSome this.Start, ValueNone
        checkConsistency startNode 0 endNode this.Count maxLevel
            emptyPrintNodeFun

        checkNodesCount this.Start this.NodesCount

        Console.WriteLine ()
