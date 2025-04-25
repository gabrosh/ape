module TextRanges

open System.Collections.Generic

open BinarySearch
open DataTypes
open TextRange

// type alias used in this file and in data containers
type TextRanges = ResizeArray<TextRange>

// producing of sorted, non-overlapping text ranges

let private isWithinLast (ranges: TextRanges) (range: TextRange) =
    ranges.Count <> 0 &&
        // Dependently on this condition, also ranges[...].first <= range.first.
        ranges[ranges.Count - 1].last >= range.last

let private isAfterLast (ranges: TextRanges) (range: TextRange) =
    ranges.Count = 0 ||
        // Independently on this condition, also ranges[...].first < range.first.
        ranges[ranges.Count - 1].last < range.first

let private findFirstOverlapping (ranges: TextRanges) (range: TextRange) =
    ranges.FindIndex (
        fun x -> (x.last >= range.first && x.first <= range.last)
    )

let private findLastOverlapping (ranges: TextRanges) (range: TextRange) =
    ranges.FindLastIndex (
        fun x -> (x.last >= range.first && x.first <= range.last)
    )

let private findFirstGreaterThan (ranges: TextRanges) (range: TextRange) =
    ranges.FindIndex (
        fun x -> (x.first > range.last )
    )

let private insertOverlappingRange
    (ranges: TextRanges) (range: TextRange) firstIndex lastIndex =

    let firstItem = ranges[firstIndex]
    let lastItem  = ranges[lastIndex]
    let newRange = {
        first = min firstItem.first range.first
        last  = max lastItem.last range.last
    }
    ranges.RemoveRange (firstIndex, lastIndex - firstIndex)
    ranges[firstIndex] <- newRange

/// Incorporates given text range into sorted, non-overlapping text ranges.
let incorporateRange (ranges: TextRanges) (range: TextRange) =
    if isWithinLast ranges range then
        ()
    elif isAfterLast ranges range then
        ranges.Add range
    else
        let firstIndex = findFirstOverlapping ranges range

        if firstIndex <> -1 then
            let lastIndex = findLastOverlapping ranges range

            insertOverlappingRange ranges range firstIndex lastIndex
        else
            let insertIndex = findFirstGreaterThan ranges range

            if insertIndex <> -1 then
                ranges.Insert (insertIndex, range)
            else
                ranges.Add range

// text ranges as a result of search

/// Name of the main group as provided by Regex class.
let mainGroupName = "0"

/// Returns true if given group name represents the main group.
let isMainGroup (s: string) =
    s = mainGroupName

/// Returns true if given group name represents colored group.
let isColoredGroup (s: string) =
    s.Length > 0 && System.Char.IsAsciiLetter s[0]

/// Returns a new text ranges groups with single empty main group.
let makeTextRangesGroups () =
    Dictionary<string, TextRanges> [
        KeyValuePair (mainGroupName, TextRanges ())
    ]

/// Returns sorted, non-overlapping text ranges corresponding to given text ranges.
let private sortRanges (ranges: TextRanges) : TextRanges =
    let result = TextRanges ()

    for range in ranges do
        incorporateRange result range

    result

/// Makes given dictionary's text ranges sorted and non-overlapping.
/// Text ranges in the main group are kept without any changes.
let sortRangesDictionary (ranges: Dictionary<string, TextRanges>) =
    for keyValue in ranges do
        if not (isMainGroup keyValue.Key) then
            ranges[keyValue.Key] <- sortRanges keyValue.Value

let private compareFirstTo first (a: TextRange) =
    compareTo first a.first

let private compareLastTo last (a: TextRange) =
    compareTo last a.last

/// Returns text ranges in interval <startLine, endLine>.
let getFromInterval (textRanges: TextRanges) startLine endLine : TextRanges =

    let compareFun = compareLastTo { line = startLine; char = IntType.MinValue }
    let r1 = findFirstGreaterInSortedArray compareFun textRanges

    let compareFun = compareFirstTo { line = endLine; char = IntType.MaxValue }
    let r2 = findLastLowerInSortedArray compareFun textRanges

    if r1.IsSome && r2.IsSome then
        let m1, m2 = r1.Value, r2.Value

        if m1 <= m2 then
            textRanges.GetRange (m1, m2 - m1 + 1)
        else
            TextRanges ()
    else
        TextRanges ()
