module CompletionUtils

open System.Text

open DataTypes
open Position

type Completion =
    | Complete of string  // use the string to complete given input
    | ListOnly of string  // only include the string in completion list

let noCompletions = ResizeArray<Completion> ()

type GetCompletionsFun =
    // itemsToComplete -> (prefixToComplete * completions)/error
    (Chars * Position) seq
        -> Result<string * ResizeArray<Completion>, string>

let getStringForCompletionList completion =
    match completion with
    | Complete x -> x
    | ListOnly x -> x

let getStringFromComplete completion =
    match completion with
    | Complete x -> x
    | ListOnly _ -> invalidOp "Must be Completion.Complete"

let private appendSpaces (sb: StringBuilder) (n: int) =
    for _ = 0 to n - 1 do
        sb.Append " " |> ignore

/// Returns string with all possible completions and offsets
/// and lengths of the completions in this string.
let getItemsStringAndIndices lineWidth (items: ResizeArray<string>) =
    let strings = StringBuilder ()
    let indices = ResizeArray<int * int> (items.Count - 1)

    let mutable lineOffset = 0
    let mutable offset     = 0

    // The first item in items is skipped in the iteration.
    let lastItemIndex = items.Count - 1 - 1

    for i, item in items |> Seq.skip 1 |> Seq.indexed do
        let isFirstItem = (i = 0)
        let isLastItem  = (i = lastItemIndex)

        // 1 stands for " "
        let prefixLength = if isFirstItem then 0 else 1
        // 2 stands for " ~"
        let suffixLength = if isLastItem  then 0 else 2

        let maxLengthToAppend = lineWidth - offset
        let maxItemLength = maxLengthToAppend - suffixLength

        let toNextLine =
            (not isFirstItem) && (prefixLength + item.Length > maxItemLength)

        if toNextLine then
            appendSpaces strings (lineWidth - 1 - offset)
            strings.Append "~~" |> ignore
            lineOffset <- lineOffset + lineWidth
            offset     <- 1

        if not isFirstItem then
            strings.Append " " |> ignore
            offset <- offset + 1

        let maxLengthToAppend = lineWidth - offset
        let maxItemLength = maxLengthToAppend - suffixLength

        let itemLength = min maxItemLength item.Length

        let itemToAppend = item.Substring (0, itemLength)

        strings.Append itemToAppend |> ignore
        indices.Add (lineOffset + offset, itemLength)
        offset <- offset + itemToAppend.Length

    (strings.ToString (), indices)
