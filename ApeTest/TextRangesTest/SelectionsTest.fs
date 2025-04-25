module SelectionsTest

open NUnit.Framework

open Position
open Selection
open Selections
open TextRange

[<TestFixture>]
type SelectionsTest () =

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

    let selectedRange (firstLine, firstChar) (lastLine, lastChar) =
        {
            first = position firstLine firstChar
            last  = position lastLine  lastChar
        }

    let setSelections selections =
        mySelections.Clear ()
        for selection in selections do
            mySelections.Add selection

    let clearSelections () =
        mySelections.Clear ()

    let assertSelectedRangesAndCursors startLine endLine expRanges expCursors =
        let _mainCursor, cursors, ranges =
            mySelections.GetSelectedRangesAndCursors startLine endLine

        Assert.AreEqual (expRanges, ranges)
        Assert.AreEqual (expCursors, cursors)

    // Add, Clear --------------------------------------------------------------

    [<Test>]
    member _.Add_And_Clear () =
        setSelections [
            selection (10, 0) (20, 0) true
            selection (30, 0) (40, 0) true
        ]

        Assert.AreEqual (2, mySelections.Count)

        clearSelections ()

        Assert.AreEqual (0, mySelections.Count)

    // GetSelectedRangesAndCursors, count check --------------------------------

    [<TestCase(0  , 9  , 0)>]
    [<TestCase(11 , 20 , 0)>]
    [<TestCase(0  , 10 , 2)>]
    [<TestCase(10 , 20 , 2)>]
    member _.GetSelectedRangesAndCursors_Count_Separated_OneLine startLine endLine expCount =
        setSelections [
            selection (10, 10) (10, 20) true
            selection (10, 30) (10, 40) true
        ]

        let _mainCursor, cursors, ranges =
            mySelections.GetSelectedRangesAndCursors startLine endLine

        Assert.AreEqual (expCount, ranges.Count)
        Assert.AreEqual (expCount, cursors.Count)

    [<TestCase(0  , 9  , 0)>]
    [<TestCase(21 , 29 , 0)>]
    [<TestCase(41 , 50 , 0)>]
    [<TestCase(0  , 10 , 1)>]
    [<TestCase(40 , 50 , 1)>]
    [<TestCase(20 , 30 , 2)>]
    member _.GetSelectedRangesAndCursors_Count_Separated_MultipleLines startLine endLine expCount =
        setSelections [
            selection (10, 0) (20, 0) true
            selection (30, 0) (40, 0) true
        ]

        let _mainCursor, cursors, ranges =
            mySelections.GetSelectedRangesAndCursors startLine endLine

        Assert.AreEqual (expCount, ranges.Count)
        Assert.AreEqual (expCount, cursors.Count)

    // GetSelectedRangesAndCursors, each selection on one line -----------------

    [<Test>]
    member _.GetSelectedRangesAndCursors_Separated_OneLine () =
        setSelections [
            selection (0, 10) (0, 20) true
            selection (0, 22) (0, 30) true
        ]

        assertSelectedRangesAndCursors 0 0 [
            selectedRange (0, 10) (0, 20)
            selectedRange (0, 22) (0, 30)
        ] [
            position 0 20
            position 0 30
        ]

    [<Test>]
    member _.GetSelectedRangesAndCursors_Touching_OneLine () =
        setSelections [
            selection (0, 10) (0, 20) true
            selection (0, 21) (0, 30) true
        ]

        assertSelectedRangesAndCursors 0 0 [
            selectedRange (0, 10) (0, 20)
            selectedRange (0, 21) (0, 30)
        ] [
            position 0 20
            position 0 30
        ]

    [<Test>]
    member _.GetSelectedRangesAndCursors_Overlapping_OneLine () =
        setSelections [
            selection (0, 10) (0, 20) true
            selection (0, 19) (0, 22) false
            selection (0, 21) (0, 30) true
        ]

        assertSelectedRangesAndCursors 0 0 [
            selectedRange (0, 10) (0, 30)
        ] [
            position 0 19
            position 0 20
            position 0 30
        ]

    // GetSelectedRangesAndCursors, each selection on multiple lines -----------

    [<Test>]
    member _.GetSelectedRangesAndCursors_Separated_MultipleLines () =
        setSelections [
            selection (10, 0 ) (20, 40) true
            selection (20, 42) (30, 0 ) true
        ]

        assertSelectedRangesAndCursors 0 99 [
            selectedRange (10, 0 ) (20, 40)
            selectedRange (20, 42) (30, 0 )
        ] [
            position 20 40
            position 30 0
        ]

    [<Test>]
    member _.GetSelectedRangesAndCursors_Touching_MultipleLines () =
        setSelections [
            selection (10, 0 ) (20, 40) true
            selection (20, 41) (30, 0 ) true
        ]

        assertSelectedRangesAndCursors 0 99 [
            selectedRange (10, 0 ) (20, 40)
            selectedRange (20, 41) (30, 0 )
        ] [
            position 20 40
            position 30 0
        ]

    [<Test>]
    member _.GetSelectedRangesAndCursors_Overlapping_MultipleLines () =
        setSelections [
            selection (10, 0 ) (20, 40) true
            selection (20, 39) (20, 42) false
            selection (20, 41) (30, 0 ) true
        ]

        assertSelectedRangesAndCursors 0 99 [
            selectedRange (10, 0) (30, 0)
        ] [
            position 20 39
            position 20 40
            position 30 0
        ]
