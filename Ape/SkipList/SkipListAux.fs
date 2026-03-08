module SkipListAux

open System

type Node<'T> = {
    mutable nexts:    Node<'T> voption array
    mutable sizes:    int array
    mutable chunk:    ResizeArray<'T>
    mutable isInUndo: bool
}

// randomization ---------------------------------------------------------------

let private random = Random ()

let getNewNodeLevel maxLevel =
    let threshold = Int32.MaxValue / 2

    let mutable level = 0

    while level < maxLevel && random.Next () < threshold do
        level <- level + 1

    level

// creating new chunks ---------------------------------------------------------

/// Inserts arr2 into arr1 at index. Returns sequence of items.
let private insertItems<'T> (arr1: ResizeArray<'T>) (arr2: 'T seq) index =
    seq {
        for i = 0 to index - 1 do
            yield arr1[i]
        for item in arr2 do
            yield item
        for i = index to arr1.Count - 1 do
            yield arr1[i]
    }

/// Inserts item into arr at index. Returns sequence of items.
let private insertItem<'T> (arr: ResizeArray<'T>) (item: 'T) index =
    seq {
        for i = 0 to index - 1 do
            yield arr[i]
        yield item
        for i = index to arr.Count - 1 do
            yield arr[i]
    }

/// Inserts arr2 into arr1 at index. Splits resulting sequence into a sequence
/// of ResizeArrays. The length of each ResizeArray is equal to maxSize, except for
/// the first one, the length of which is firstMaxSize at most, and the last one,
/// which can be shorter than maxSize.
let combineWithItems<'T>
    (arr1: ResizeArray<'T>) index (arr2: 'T seq) firstMaxSize maxSize =

    seq {
        let items = ResizeArray<'T> ()

        let mutable arraySize = firstMaxSize

        for item in insertItems arr1 arr2 index do
            items.Add item

            if items.Count = arraySize then
                yield ResizeArray<'T> items
                items.Clear ()

            arraySize <- maxSize

        if items.Count <> 0 then
            yield ResizeArray<'T> items
            items.Clear ()
    }

/// Inserts item into arr at index. Splits resulting sequence into a sequence
/// of ResizeArrays. The length of each ResizeArray is equal to maxSize, except for
/// the first one, the length of which is firstMaxSize at most, and the last one,
/// which can be shorter than maxSize.
let combineWithItem<'T>
    (arr: ResizeArray<'T>) index (item: 'T) firstMaxSize maxSize =

    seq {
        let items = ResizeArray<'T> ()

        let mutable arraySize = firstMaxSize

        for item in insertItem arr item index do
            items.Add item

            if items.Count = arraySize then
                yield ResizeArray<'T> items
                items.Clear ()

            arraySize <- maxSize

        if items.Count <> 0 then
            yield ResizeArray<'T> items
            items.Clear ()
    }

/// Returns items of arr in a sequence of ResizeArrays. The length of each ResizeArray
/// is equal to maxSize, except for the last one, which can be shorter than maxSize.
let getChunksWithItems<'T> (arr: 'T seq) maxSize =
    seq {
        let items = ResizeArray<'T> ()

        for item in arr do
            items.Add item

            if items.Count = maxSize then
                yield ResizeArray<'T> items
                items.Clear ()

        if items.Count <> 0 then
            yield ResizeArray<'T> items
            items.Clear ()
    }

// Returns item in a sequence of ResizeArrays.
let getChunksWithItem<'T> (item: 'T) =
    seq {
        let items = ResizeArray<'T> ()

        items.Add item

        yield items
    }

/// Returns item in a ResizeArray.
let getChunkWithItem<'T> (item: 'T) =
    let items = ResizeArray<'T> ()
    items.Add item
    items

// consistency check -----------------------------------------------------------

/// Checks span of given node and level for consistency.
/// Throws an exception if some inconsistency is found.
let rec checkConsistency
    (node: Node<'T> voption) accSize (endNode: Node<'T> voption) endAccSize level
    printNodeFun
  =
    let funName = "checkConsistency"

    if node = endNode then
        if accSize <> endAccSize then
            invalidOp funName
    else
        match node with
        | ValueSome node' ->
            let nextNode    = node'.nexts[level]
            let nextAccSize = accSize + node'.sizes[level]

            printNodeFun level accSize node'

            if level > 0 then
                checkConsistency node accSize nextNode nextAccSize (level - 1)
                    printNodeFun
            elif level = 0 then
                if node'.sizes[0] <> node'.chunk.Count then
                    invalidOp funName
            else
                invalidOp funName

            checkConsistency nextNode nextAccSize endNode endAccSize level
                printNodeFun

        | ValueNone ->
            invalidOp funName

/// Check count of nodes in the data structure =
let checkNodesCount (startNode: Node<'T>) countExpected =
    let funName = "checkNodesCount"

    let rec loop node accCount =
        match node.nexts[0] with
        | ValueSome nextNode ->
            loop nextNode (accCount + 1)
        | ValueNone ->
            accCount

    let count = loop startNode 1

    if count <> countExpected then
        invalidOp $"{funName}: {count} <> {countExpected}"
