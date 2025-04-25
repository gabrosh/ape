module PositionClassifierTest

open NUnit.Framework

open Position
open PositionClassifier
open Selection
open Selections
open TextRange
open TextRanges

[<TestFixture>]
type PositionClassifierTest () =

    let mySelections = Selections (ResizeArray<Selection> ())

    let position line char =
        {
            line = line
            char = char
        }

    let selection (firstLine, firstChar) (lastLine, lastChar) isForward =
        {
            first     = position firstLine firstChar
            last      = position lastLine  lastChar
            firstWC   = WantedColumns_Default
            lastWC    = WantedColumns_Default
            isForward = isForward
        }

    let match_ (firstLine, firstChar) (lastLine, lastChar) =
        {
            first = position firstLine firstChar
            last  = position lastLine  lastChar
        }

    let setSelections selections =
        mySelections.Clear ()
        selections |> Seq.iter mySelections.Add

    // IsAtCursor --------------------------------------------------------------

    [<Test>]
    member _.IsAtCursor_FromStart () =
        setSelections [
            selection (0, 2) (0, 3) true
            selection (0, 6) (0, 7) true
        ]

        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors 0 0
        let matchRanges = [|
            Colors.mainGroupColor, TextRanges ()
        |]

        Assert.AreEqual (2, cursors.Count)

        let pc = PositionClassifier (mainCursor, cursors, selectionRanges, matchRanges)

        Assert.AreEqual (false, pc.IsAtCursor (position 0 0))
        Assert.AreEqual (false, pc.IsAtCursor (position 0 1))
        Assert.AreEqual (false, pc.IsAtCursor (position 0 2))
        Assert.AreEqual (true , pc.IsAtCursor (position 0 3))
        Assert.AreEqual (false, pc.IsAtCursor (position 0 4))
        Assert.AreEqual (false, pc.IsAtCursor (position 0 5))
        Assert.AreEqual (false, pc.IsAtCursor (position 0 6))
        Assert.AreEqual (true , pc.IsAtCursor (position 0 7))
        Assert.AreEqual (false, pc.IsAtCursor (position 0 8))
        Assert.AreEqual (false, pc.IsAtCursor (position 0 9))

    [<Test>]
    member _.IsAtCursor_FromMiddle_False () =
        setSelections [
            selection (0, 2) (0, 3) true
            selection (0, 6) (0, 7) true
        ]

        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors 0 0
        let matchRanges = [|  
            Colors.mainGroupColor, TextRanges ()
        |]

        Assert.AreEqual (2, cursors.Count)

        let pc = PositionClassifier (mainCursor, cursors, selectionRanges, matchRanges)

        Assert.AreEqual (false, pc.IsAtCursor (position 0 6))
        Assert.AreEqual (true , pc.IsAtCursor (position 0 7))

    [<Test>]
    member _.IsAtCursor_FromMiddle_True () =
        setSelections [
            selection (0, 2) (0, 3) true
            selection (0, 6) (0, 7) true
        ]

        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors 0 0
        let matchRanges = [|
            Colors.mainGroupColor, TextRanges ()
        |]

        Assert.AreEqual (2, cursors.Count)

        let pc = PositionClassifier (mainCursor, cursors, selectionRanges, matchRanges)

        Assert.AreEqual (true , pc.IsAtCursor (position 0 7))
        Assert.AreEqual (false, pc.IsAtCursor (position 0 8))

    // IsInSelection -----------------------------------------------------------

    [<Test>]
    member _.IsInSelection_FromStart () =
        setSelections [
            selection (0, 2) (0, 3) true
            selection (0, 6) (0, 7) true
        ]

        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors 0 0
        let matchRanges = [|
            Colors.mainGroupColor, TextRanges ()
        |]

        Assert.AreEqual (2, selectionRanges.Count)

        let pc = PositionClassifier (mainCursor, cursors, selectionRanges, matchRanges)

        Assert.AreEqual (false, pc.IsInSelection (position 0 0))
        Assert.AreEqual (false, pc.IsInSelection (position 0 1))
        Assert.AreEqual (true , pc.IsInSelection (position 0 2))
        Assert.AreEqual (true , pc.IsInSelection (position 0 3))
        Assert.AreEqual (false, pc.IsInSelection (position 0 4))
        Assert.AreEqual (false, pc.IsInSelection (position 0 5))
        Assert.AreEqual (true , pc.IsInSelection (position 0 6))
        Assert.AreEqual (true , pc.IsInSelection (position 0 7))
        Assert.AreEqual (false, pc.IsInSelection (position 0 8))
        Assert.AreEqual (false, pc.IsInSelection (position 0 9))

    [<Test>]
    member _.IsInSelection_FromMiddle_False () =
        setSelections [
            selection (0, 2) (0, 3) true
            selection (0, 6) (0, 7) true
        ]

        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors 0 0
        let matchRanges = [|
            Colors.mainGroupColor, TextRanges ()
        |]

        Assert.AreEqual (2, selectionRanges.Count)

        let pc = PositionClassifier (mainCursor, cursors, selectionRanges, matchRanges)

        Assert.AreEqual (false, pc.IsInSelection (position 0 5))
        Assert.AreEqual (true , pc.IsInSelection (position 0 6))

    [<Test>]
    member _.IsInSelection_FromMiddle_True () =
        setSelections [
            selection (0, 2) (0, 3) true
            selection (0, 6) (0, 7) true
        ]

        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors 0 0
        let matchRanges = [|
            Colors.mainGroupColor, TextRanges ()
        |]

        Assert.AreEqual (2, selectionRanges.Count)

        let pc = PositionClassifier (mainCursor, cursors, selectionRanges, matchRanges)

        Assert.AreEqual (true , pc.IsInSelection (position 0 7))
        Assert.AreEqual (false, pc.IsInSelection (position 0 8))

    // IsInMatch -----------------------------------------------------------

    [<Test>]
    member _.IsInMatch_FromStart () =
        setSelections [
            selection (0, 0) (0, 0) true
        ]

        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors 0 0
        let matchRanges = [|
            Colors.mainGroupColor, TextRanges [
                match_ (0, 2) (0, 3)
                match_ (0, 6) (0, 7)
            ]
        |]

        Assert.AreEqual (2, (snd matchRanges[0]).Count)

        let pc = PositionClassifier (mainCursor, cursors, selectionRanges, matchRanges)

        Assert.AreEqual (None  , pc.IsInMatch (position 0 0))
        Assert.AreEqual (None  , pc.IsInMatch (position 0 1))
        Assert.AreEqual (Some 0, pc.IsInMatch (position 0 2))
        Assert.AreEqual (Some 0, pc.IsInMatch (position 0 3))
        Assert.AreEqual (None  , pc.IsInMatch (position 0 4))
        Assert.AreEqual (None  , pc.IsInMatch (position 0 5))
        Assert.AreEqual (Some 0, pc.IsInMatch (position 0 6))
        Assert.AreEqual (Some 0, pc.IsInMatch (position 0 7))
        Assert.AreEqual (None  , pc.IsInMatch (position 0 8))
        Assert.AreEqual (None  , pc.IsInMatch (position 0 9))

    [<Test>]
    member _.IsInMatch_FromMiddle_False () =
        setSelections [
            selection (0, 0) (0, 0) true
        ]

        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors 0 0
        let matchRanges = [|
            Colors.mainGroupColor, TextRanges [
                match_ (0, 2) (0, 3)
                match_ (0, 6) (0, 7)
            ]
        |]

        Assert.AreEqual (2, (snd matchRanges[0]).Count)

        let pc = PositionClassifier (mainCursor, cursors, selectionRanges, matchRanges)

        Assert.AreEqual (None  , pc.IsInMatch (position 0 5))
        Assert.AreEqual (Some 0, pc.IsInMatch (position 0 6))

    [<Test>]
    member _.IsInMatch_FromMiddle_True () =
        setSelections [
            selection (0, 0) (0, 0) true
        ]

        let mainCursor, cursors, selectionRanges =
            mySelections.GetSelectedRangesAndCursors 0 0
        let matchRanges = [|
            Colors.mainGroupColor, TextRanges [
                match_ (0, 2) (0, 3)
                match_ (0, 6) (0, 7)
            ]
        |]

        Assert.AreEqual (2, (snd matchRanges[0]).Count)

        let pc = PositionClassifier (mainCursor, cursors, selectionRanges, matchRanges)

        Assert.AreEqual (Some 0, pc.IsInMatch (position 0 7))
        Assert.AreEqual (None  , pc.IsInMatch (position 0 8))
