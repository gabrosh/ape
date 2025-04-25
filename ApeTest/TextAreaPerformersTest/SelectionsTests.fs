module TextAreaSelectionsTests

open NUnit.Framework
open System

open Commands.InCommands
open Registers
open Selection
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef 80 25

[<TestFixture>]
type SelectionsTest () =
    let myBuffer = new TextAreaBuffer (contextRef, UserMessages (), Registers (), "")

    // initialization

    let initWithDirection isForward cursorBeforeEol lines selections =
        myBuffer.LoadStrings lines

        TestUtils.set_cursorBeforeEol contextRef cursorBeforeEol

        myBuffer.Selections.Clear ()

        for firstLine, firstChar, lastLine, lastChar in selections do
            myBuffer.Selections.Add {
                Selection_Default with
                    first     = { line = firstLine; char = firstChar }
                    last      = { line = lastLine ; char = lastChar  }
                    firstWC   = { hard = firstChar; soft = firstChar }
                    lastWC    = { hard = lastChar ; soft = lastChar  }
                    isForward = isForward
            }

    let init         = initWithDirection true
    let initBackward = initWithDirection false

    let toTuple2 (a: 'T array) =
        a[0], a[1]

    let toTuple4 (a: 'T array) =
        a[0], a[1], a[2], a[3]

    // command and simple assertions

    let performCommand (command: obj) count =
        match command with
        | :? SelectionsCommand as x ->
            myBuffer.PerformCommand false false (SelectionsCommand x) count
        | _                         ->
            invalidOp ""

    let command command =
        performCommand command 1

    let assertFirst (selection: Selection) anchorLine anchorChar =
        Assert.AreEqual (
            ( anchorLine           , anchorChar           ),
            ( selection.first.line , selection.first.char )
        )

    let assertLast (selection: Selection) cursorLine cursorChar =
        Assert.AreEqual (
            ( cursorLine           , cursorChar           ),
            ( selection.last.line  , selection.last.char  )
        )

    let assertDirection (selection: Selection) isForward =
        Assert.AreEqual (selection.isForward, isForward)

    let assertMainIndex mainIndex =
        Assert.AreEqual (mainIndex, myBuffer.Selections.MainIndex)

    // assertions

    let assertSelectionsWithDirection isForward selections =
        for i, selection in selections |> Seq.indexed do
            let firstLine, firstChar, lastLine, lastChar = selection
            assertFirst     myBuffer.Selections[i] firstLine firstChar
            assertLast      myBuffer.Selections[i] lastLine  lastChar
            assertDirection myBuffer.Selections[i] isForward

    let assertSelections = assertSelectionsWithDirection true

    // commands followed by assertion(s)

    let commandAssertMainIndex command mainIndex =
        performCommand command 1
        assertMainIndex mainIndex

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // CopyFirstUp -------------------------------------------------------------

    [<TestCase( false, [|1; 1; 1; 1|] )>]
    [<TestCase( false, [|1; 2; 1; 2|] )>]
    [<TestCase( true , [|1; 1; 1; 1|] )>]
    [<TestCase( true , [|1; 2; 1; 1|] )>]
    member _.CopyFirstUp cbe chars =
        let anchorChar, cursorChar, newAnchorChar, newCursorChar =
            toTuple4 chars

        init cbe [
            "aa"; "bb"; "cc"
        ] [
            1, anchorChar   , 1, cursorChar
            2, 0, 2, 1
        ]
        command CopyFirstUp
        assertSelections [
            0, newAnchorChar, 0, newCursorChar
            1, anchorChar   , 1, cursorChar
            2, 0, 2, 1
        ]

    [<TestCase( false, [|1; 1; 1; 1|] )>]
    member _.CopyFirstUp_AtTop cbe chars =
        let anchorChar, cursorChar =
            toTuple2 chars

        init cbe [
            "aa"; "bb"
        ] [
            0, anchorChar, 0, cursorChar
            1, 0, 1, 1
        ]
        command CopyFirstUp
        assertSelections [
            0, anchorChar, 0, cursorChar
            1, 0, 1, 1
        ]

    // CopyLastDown ------------------------------------------------------------

    [<TestCase( false, [|1; 1; 1; 1|] )>]
    [<TestCase( false, [|1; 2; 1; 2|] )>]
    [<TestCase( true , [|1; 1; 1; 1|] )>]
    [<TestCase( true , [|1; 2; 1; 1|] )>]
    member _.CopyLastDown cbe chars =
        let anchorChar, cursorChar, newAnchorChar, newCursorChar =
            toTuple4 chars

        init cbe [
            "aa"; "bb"; "cc"
        ] [
            0, 0, 0, 1
            1, anchorChar   , 1, cursorChar
        ]
        command CopyLastDown
        assertSelections [
            0, 0, 0, 1
            1, anchorChar   , 1, cursorChar
            2, newAnchorChar, 2, newCursorChar
        ]

    [<TestCase( false, [|1; 1; 1; 1|] )>]
    member _.CopyLastDown_AtTop cbe chars =
        let anchorChar, cursorChar =
            toTuple2 chars

        init cbe [
            "aa"; "bb"
        ] [
            0, 0, 0, 1
            1, anchorChar, 1, cursorChar
        ]
        command CopyFirstUp
        assertSelections [
            0, 0, 0, 1
            1, anchorChar, 1, cursorChar
        ]

    // Multiply ----------------------------------------------------------------

    [<Test>]
    member _.Multiply () =
        init false [
            "aa"
        ] [
            0, 0, 0, 1
            0, 1, 0, 2
        ]
        command (Multiply 3)
        assertSelections [
            0, 0, 0, 1
            0, 0, 0, 1
            0, 0, 0, 1
            0, 1, 0, 2
            0, 1, 0, 2
            0, 1, 0, 2
        ]

    // SelectWholeBuffer -------------------------------------------------------

    [<Test>]
    member _.SelectWholeBuffer () =
        init false [
            "aa"; "bb"
        ] [
            0, 1, 0, 1
            1, 1, 1, 1
        ]
        command SelectWholeBuffer
        assertSelections [
            0, 0, 1, 2
        ]

    // InvertSelections --------------------------------------------------------

    [<Test>]
    member _.InvertSelections_InnerSelection () =
        init false [
            "aa"; "bb"
        ] [
            0, 2, 1, 0
        ]
        command InvertSelections
        assertSelections [
            0, 0, 0, 1
            1, 1, 1, 2
        ]

    [<Test>]
    member _.InvertSelections_OuterSelections () =
        init false [
            "aa"; "bb"
        ] [
            0, 0, 0, 1
            1, 1, 1, 2
        ]
        command InvertSelections
        assertSelections [
            0, 2, 1, 0
        ]

    [<Test>]
    member _.InvertSelections_Overlapping_1 () =
        init false [
            "aa"; "bb"
        ] [
            0, 1, 1, 0
            0, 2, 1, 1
        ]
        command InvertSelections
        assertSelections [
            0, 0, 0, 0
            1, 2, 1, 2
        ]

    [<Test>]
    member _.InvertSelections_Overlapping_2 () =
        init false [
            "aa"; "bb"
        ] [
            0, 1, 1, 1
            0, 2, 1, 0
        ]
        command InvertSelections
        assertSelections [
            0, 0, 0, 0
            1, 2, 1, 2
        ]

    [<Test>]
    member _.InvertSelections_WholeBufferSelected () =
        init false [
            "aa"; "bb"
        ] [
            0, 0, 1, 2
        ]
        command InvertSelections
        assertSelections [
            1, 2, 1, 2
        ]

    // SplitOnLineStarts ------------------------------------------------------

    [<TestCase( [|0; 0|] )>]
    [<TestCase( [|0; 2|] )>]
    [<TestCase( [|2; 2|] )>]
    member _.SplitOnLineStarts_OneLine chars =
        let anchorChar, cursorChar =
            toTuple2 chars

        init false [
            "aa"
        ] [
            0, anchorChar, 0, cursorChar
        ]
        command SplitOnLineStarts
        assertSelections [
            0, anchorChar, 0, cursorChar
        ]

    [<TestCase( [|0; 0; 2; 0|] )>]
    [<TestCase( [|0; 2; 2; 2|] )>]
    member _.SplitOnLineStarts_MultipleLines chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init false [
            "aa"; "bb"; "cc"
        ] [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]
        command SplitOnLineStarts
        assertSelections [
            anchorLine, anchorChar, 0, 2
            1, 0, 1, 2
            2, 0, cursorLine, cursorChar
        ]

    [<Test>]
    member _.SplitOnLineStarts_MultipleSelections () =
        init false [
            "aa"; "bb"
            "aa"; "bb"
        ] [
            0, 0, 1, 2
            2, 0, 3, 2
        ]
        command SplitOnLineStarts
        assertSelections [
            0, 0, 0, 2
            1, 0, 1, 2
            2, 0, 2, 2
            3, 0, 3, 2
        ]

    // MergeContiguous --------------------------------------------------------

    [<TestCase( [|0; 0; 2; 0|] )>]
    [<TestCase( [|0; 1; 2; 1|] )>]
    member _.MergeContiguous_NonContiguous chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init false [
            "aa"; "bb"; "cc"
        ] [
            anchorLine, anchorChar, 0, 1
            1, 0, 1, 2
            2, 1, cursorLine, cursorChar
        ]
        command MergeContiguous
        assertSelections [
            anchorLine, anchorChar, 0, 1
            1, 0, 1, 2
            2, 1, cursorLine, cursorChar
        ]

    [<TestCase( [|0; 0; 2; 0|] )>]
    [<TestCase( [|0; 2; 2; 2|] )>]
    member _.MergeContiguous_Contiguous_NonOverlapping chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init false [
            "aa"; "bb"; "cc"
        ] [
            anchorLine, anchorChar, 0, 2
            1, 0, 1, 2
            2, 0, cursorLine, cursorChar
        ]
        command MergeContiguous
        assertSelections [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]

    [<TestCase( [|0; 0; 2; 0|] )>]
    [<TestCase( [|0; 2; 2; 2|] )>]
    member _.MergeContiguous_Contiguous_Overlapping chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init false [
            "aa"; "bb"; "cc"
        ] [
            anchorLine, anchorChar, 1, 0
            1, 0, 1, 2
            1, 2, cursorLine, cursorChar
        ]
        command MergeContiguous
        assertSelections [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]

    // ExpandToFullLines ------------------------------------------------------

    [<TestCase( [|0; 0|] )>]
    [<TestCase( [|0; 2|] )>]
    [<TestCase( [|2; 2|] )>]
    member _.ExpandToFullLines_OneLine chars =
        let anchorChar, cursorChar =
            toTuple2 chars

        init false [
            "aa"
        ] [
            0, anchorChar, 0, cursorChar
        ]
        command ExpandToFullLines
        assertSelections [
            0, 0, 0, 2
        ]

    [<TestCase( [|0; 0; 2; 0|] )>]
    [<TestCase( [|0; 2; 2; 2|] )>]
    member _.ExpandToFullLines_MultipleLines chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init false [
            "aa"; "bb"; "cc"
        ] [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]
        command ExpandToFullLines
        assertSelections [
            0, 0, 2, 2
        ]

    [<Test>]
    member _.ExpandToFullLines_MultipleSelections () =
        init false [
            "aa"
            "aa"
        ] [
            0, 0, 0, 0
            1, 0, 1, 0
        ]
        command ExpandToFullLines
        assertSelections [
            0, 0, 0, 2
            1, 0, 1, 2
        ]

    // TrimToFullLines --------------------------------------------------------

    [<TestCase( [|0; 0|] )>]
    [<TestCase( [|0; 2|] )>]
    [<TestCase( [|2; 2|] )>]
    member _.TrimToFullLines_OneLine chars =
        let anchorChar, cursorChar =
            toTuple2 chars

        init false [
            "aa"
        ] [
            0, anchorChar, 0, cursorChar
        ]
        command TrimToFullLines
        assertSelections [
            0, anchorChar, 0, cursorChar
        ]

    [<TestCase( [|0; 0; 2; 0|] )>]
    member _.TrimToFullLines_MultipleLines_1 chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init false [
            "aa"; "bb"; "cc"
        ] [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]
        command TrimToFullLines
        assertSelections [
            0, 0, 1, 2
        ]

    [<TestCase( [|0; 2; 2; 2|] )>]
    member _.TrimToFullLines_MultipleLines_2 chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init false [
            "aa"; "bb"; "cc"
        ] [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]
        command TrimToFullLines
        assertSelections [
            1, 0, 2, 2
        ]

    [<Test>]
    member _.TrimToFullLines_MultipleSelections () =
        init false [
            "aa"; "bb"; "cc"
            "aa"; "bb"; "cc"
        ] [
            0, 1, 2, 1
            3, 1, 5, 1
        ]
        command TrimToFullLines
        assertSelections [
            1, 0, 1, 2
            4, 0, 4, 2
        ]

    // ReduceToCursor ---------------------------------------------------------

    [<TestCase( [|0; 0|] )>]
    [<TestCase( [|0; 2|] )>]
    [<TestCase( [|2; 2|] )>]
    member _.ReduceToCursor_Forward chars =
        let anchorChar, cursorChar =
            toTuple2 chars

        init false [
            "aa"
        ] [
            0, anchorChar, 0, cursorChar
        ]
        command ReduceToCursor
        assertSelections [
            0, cursorChar, 0, cursorChar
        ]

    [<TestCase( [|0; 0|] )>]
    [<TestCase( [|0; 2|] )>]
    [<TestCase( [|2; 2|] )>]
    member _.ReduceToCursor_Backward chars =
        let cursorChar, anchorChar =
            toTuple2 chars

        initBackward false [
            "aa"
        ] [
            0, cursorChar, 0, anchorChar
        ]
        command ReduceToCursor
        assertSelectionsWithDirection false [
            0, cursorChar, 0, cursorChar
        ]

    [<TestCase( [|0; 2|] )>]
    member _.ReduceToCursor_MultipleSelections chars =
        let anchorChar, cursorChar =
            toTuple2 chars

        init false [
            "aa"
            "aa"
        ] [
            0, anchorChar, 0, cursorChar
            1, anchorChar, 1, cursorChar
        ]
        command ReduceToCursor
        assertSelections [
            0, cursorChar, 0, cursorChar
            1, cursorChar, 1, cursorChar
        ]

    // ForwardDirection -------------------------------------------------------

    [<TestCase( true , true, [|0; 0|] )>]
    [<TestCase( true , true, [|0; 2|] )>]
    [<TestCase( true , true, [|2; 2|] )>]
    [<TestCase( false, true, [|0; 0|] )>]
    [<TestCase( false, true, [|0; 2|] )>]
    [<TestCase( false, true, [|2; 2|] )>]
    member _.ForwardDirection isForwardStart isForwardEnd chars =
        let firstChar, lastChar =
            toTuple2 chars

        initWithDirection isForwardStart false [
            "aa"
        ] [
            0, firstChar, 0, lastChar
        ]
        command ForwardDirection
        assertSelectionsWithDirection isForwardEnd [
            0, firstChar, 0, lastChar
        ]

    [<TestCase( false, true, [|0; 0|] )>]
    member _.ForwardDirection_MultipleSelections isForwardStart isForwardEnd chars =
        let firstChar, lastChar =
            toTuple2 chars

        initWithDirection isForwardStart false [
            "aa"
            "aa"
        ] [
            0, firstChar, 0, lastChar
            1, firstChar, 1, lastChar
        ]
        command ForwardDirection
        assertSelectionsWithDirection isForwardEnd [
            0, firstChar, 0, lastChar
            1, firstChar, 1, lastChar
        ]

    // FlipDirection ----------------------------------------------------------

    [<TestCase( true , false, [|0; 0|] )>]
    [<TestCase( true , false, [|0; 2|] )>]
    [<TestCase( true , false, [|2; 2|] )>]
    [<TestCase( false, true , [|0; 0|] )>]
    [<TestCase( false, true , [|0; 2|] )>]
    [<TestCase( false, true , [|2; 2|] )>]
    member _.FlipDirection isForwardStart isForwardEnd chars =
        let firstChar, lastChar =
            toTuple2 chars

        initWithDirection isForwardStart false [
            "aa"
        ] [
            0, firstChar, 0, lastChar
        ]
        command FlipDirection
        assertSelectionsWithDirection isForwardEnd [
            0, firstChar, 0, lastChar
        ]

    [<TestCase( false, true , [|0; 0|] )>]
    member _.FlipDirection_MultipleSelections isForwardStart isForwardEnd chars =
        let firstChar, lastChar =
            toTuple2 chars

        initWithDirection isForwardStart false [
            "aa"
            "aa"
        ] [
            0, firstChar, 0, lastChar
            1, firstChar, 1, lastChar
        ]
        command FlipDirection
        assertSelectionsWithDirection isForwardEnd [
            0, firstChar, 0, lastChar
            1, firstChar, 1, lastChar
        ]

    // KeepOnlyMain, RemoveMain ------------------------------------------------

    [<Test>]
    member _.KeepOnlyMain () =
        init false [
            "aa"; "bb"; "cc"
        ] [
            0, 0, 0, 0
            1, 0, 1, 0
            2, 0, 2, 0
        ]
        command KeepOnlyMain
        assertSelections [
            2, 0, 2, 0
        ]

    [<Test>]
    member _.RemoveMain () =
        init false [
            "aa"; "bb"; "cc"
        ] [
            0, 0, 0, 0
            1, 0, 1, 0
            2, 0, 2, 0
        ]
        command RemoveMain
        assertSelections [
            0, 0, 0, 0
            1, 0, 1, 0
        ]

    // RemoveLessIndented -----------------------------------------------------

    [<Test>]
    member _.RemoveLessIndented_Forward () =
        init false [
            "aaa"; "bbb"; "ccc"
        ] [
            0, 0, 0, 1
            2, 2, 2, 3
            1, 1, 1, 2
        ]
        command RemoveLessIndented
        assertSelections [
            1, 1, 1, 2
            2, 2, 2, 3
        ]

    [<Test>]
    member _.RemoveLessIndented_Backward () =
        initWithDirection false false [
            "aaa"; "bbb"; "ccc"
        ] [
            0, 0, 0, 1
            2, 2, 2, 3
            1, 1, 1, 2
        ]
        command RemoveLessIndented
        assertSelectionsWithDirection false [
            1, 1, 1, 2
            2, 2, 2, 3
        ]

    // RemoveMoreIndented -----------------------------------------------------

    [<Test>]
    member _.RemoveMoreIndented_Forward () =
        init false [
            "aaa"; "bbb"; "ccc"
        ] [
            0, 0, 0, 1
            2, 2, 2, 3
            1, 1, 1, 2
        ]
        command RemoveMoreIndented
        assertSelections [
            0, 0, 0, 1
            1, 1, 1, 2
        ]

    [<Test>]
    member _.RemoveMoreIndented_Backward () =
        initWithDirection false false [
            "aaa"; "bbb"; "ccc"
        ] [
            0, 0, 0, 1
            2, 2, 2, 3
            1, 1, 1, 2
        ]
        command RemoveMoreIndented
        assertSelectionsWithDirection false [
            0, 0, 0, 1
            1, 1, 1, 2
        ]

    // RotateUp, RotateDown ----------------------------------------------------

    [<Test>]
    member _.RotateUp () =
        init false [
            "aaa"; "bbb"; "ccc"
        ] [
            0, 0, 0, 1
            1, 1, 1, 2
            2, 2, 2, 3
        ]

        commandAssertMainIndex RotateUp 1
        commandAssertMainIndex RotateUp 0
        commandAssertMainIndex RotateUp 2

    [<Test>]
    member _.RotateDown () =
        init false [
            "aaa"; "bbb"; "ccc"
        ] [
            0, 0, 0, 1
            1, 1, 1, 2
            2, 2, 2, 3
        ]

        commandAssertMainIndex RotateDown 0
        commandAssertMainIndex RotateDown 1
        commandAssertMainIndex RotateDown 2

    // SelectMatching ----------------------------------------------------------

    [<Test>]
    member _.SelectMatching_MultipleSelections () =
        init false [
            "aa"; "bb"; "cc"
            "aa"; "bb"; "cc"
        ] [
            0, 0, 2, 2
            3, 0, 5, 2
        ]
        command (SelectMatching @"[ac]+\n")
        assertSelections [
            0, 0, 0, 2
            2, 0, 2, 2
            3, 0, 3, 2
            5, 0, 5, 2
        ]

    [<Test>]
    member _.SelectMatching_NoMatch () =
        init false [
            "aa"; "bb"; "cc"
            "aa"; "bb"; "cc"
        ] [
            0, 0, 2, 2
            3, 0, 5, 2
        ]
        command (SelectMatching "x")
        assertSelections [
            5, 2, 5, 2
        ]

    // KeepMatching ------------------------------------------------------------

    [<Test>]
    member _.KeepMatching_MultipleSelections () =
        init false [
            "aa"; "bb"; "cc"
        ] [
            0, 0, 0, 2
            1, 0, 1, 2
            2, 0, 2, 2
        ]
        command (KeepMatching "[ac]")
        assertSelections [
            0, 0, 0, 2
            2, 0, 2, 2
        ]

    [<Test>]
    member _.KeepMatching_NoMatch () =
        init false [
            "aa"; "bb"; "cc"
        ] [
            0, 0, 0, 2
            1, 0, 1, 2
            2, 0, 2, 2
        ]
        command (KeepMatching "x")
        assertSelections [
            2, 2, 2, 2
        ]

    // DiscardMatching ---------------------------------------------------------

    [<Test>]
    member _.DiscardMatching_MultipleSelections () =
        init false [
            "aa"; "bb"; "cc"
        ] [
            0, 0, 0, 2
            1, 0, 1, 2
            2, 0, 2, 2
        ]
        command (DiscardMatching "b")
        assertSelections [
            0, 0, 0, 2
            2, 0, 2, 2
        ]

    [<Test>]
    member _.DiscardMatching_NoMatch () =
        init false [
            "aa"; "bb"; "cc"
        ] [
            0, 0, 0, 2
            1, 0, 1, 2
            2, 0, 2, 2
        ]
        command (DiscardMatching "x")
        assertSelections [
            0, 0, 0, 2
            1, 0, 1, 2
            2, 0, 2, 2
        ]
