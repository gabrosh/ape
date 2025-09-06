module TextAreaModifyingOthersTest

open NUnit.Framework
open System

open Commands.InCommands
open DataTypes
open Registers
open Selection
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef 80 25

let defReg = DefaultRegister

[<TestFixture>]
type ModifyingOthersTest () =
    let myRegisters = Registers ()
    let myBuffer    = makeTextAreaBuffer (contextRef, UserMessages (), myRegisters, "")

    // initialization

    let init lines selections =
        myBuffer.LoadStrings lines

        myBuffer.Selections.Clear ()

        for anchorLine, anchorChar, cursorLine, cursorChar in selections do
            myBuffer.Selections.Add {
                Selection_Default with
                    first = { line = anchorLine; char = anchorChar }
                    last  = { line = cursorLine; char = cursorChar }
                    isForward = true
            }

    let toTuple4 (a: 'T array) =
        a[0], a[1], a[2], a[3]

    // command and simple assertions

    let performCommand (command: obj) count =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand false false (ModifyingCommand  x) count
        | _                        ->
            invalidOp ""

    let command command =
        performCommand command 1

    let getRegister () =
        myRegisters.GetSlot DefaultRegister 0
        |> Option.get
        |> Seq.map charsToString
        |> Seq.toList

    let setRegister lines =
        let lines = Lines (lines |> Seq.map stringToChars)
        myRegisters.ApplyToSlot DefaultRegister 0 lines

    let assertLines expLines =
        Assert.AreEqual (expLines, myBuffer.Lines)

    let assertCursor (selection: Selection) cursorLine cursorChar =
        Assert.AreEqual (
            ( cursorLine            , cursorChar            ),
            ( selection.Cursor.line , selection.Cursor.char )
        )

    let assertAnchor (selection: Selection) anchorLine anchorChar =
        Assert.AreEqual (
            ( anchorLine            , anchorChar            ),
            ( selection.Anchor.line , selection.Anchor.char )
        )

    let assertRegister expRegister =
        Assert.AreEqual (expRegister, getRegister ())

    // assertions

    let assertLinesSelections lines selections =
        assertLines lines

        for i, selection in selections |> Seq.indexed do
            let anchorLine, anchorChar, cursorLine, cursorChar = selection
            assertAnchor myBuffer.Selections[i] anchorLine anchorChar
            assertCursor myBuffer.Selections[i] cursorLine cursorChar

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // EnterInserting, EnterAppending ------------------------------------------

    [<TestCase( [|"ab"    |], [|"ab"    |], [|0; 1; 0; 0|] )>]
    [<TestCase( [|"ab"    |], [|"ab"    |], [|1; 2; 0; 1|] )>]
    [<TestCase( [|"ab"; ""|], [|"ab"; ""|], [|2; 2; 0; 2|] )>]
    [<TestCase( [|"ab"    |], [|"ab"    |], [|2; 2; 0; 2|] )>]
    member _.EnterInserting start end_ chars =
        let startAnchorChar, startCursorChar, endLine, endCommonChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command EnterInserting
        assertLinesSelections end_ [
            endLine, endCommonChar, endLine, endCommonChar
        ]

    [<TestCase( [|"ab"    |], [|"ab"    |], [|0; 1; 0; 2|] )>]
    [<TestCase( [|"ab"; ""|], [|"ab"; ""|], [|1; 2; 1; 0|] )>]
    [<TestCase( [|"ab"; ""|], [|"ab"; ""|], [|2; 2; 1; 0|] )>]
    [<TestCase( [|"ab"    |], [|"ab"; ""|], [|1; 2; 1; 0|] )>]
    [<TestCase( [|"ab"    |], [|"ab"; ""|], [|2; 2; 1; 0|] )>]
    member _.EnterAppending start end_ chars =
        let startAnchorChar, startCursorChar, endLine, endCommonChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command EnterAppending
        assertLinesSelections end_ [
            endLine, endCommonChar, endLine, endCommonChar
        ]

    // EnterInsertingAtSol, EnterAppendingAtEol --------------------------------

    [<TestCase( [|"ab"    |], [|"ab"    |], [|0; 1; 0; 0|] )>]
    [<TestCase( [|"ab"    |], [|"ab"    |], [|1; 2; 0; 0|] )>]
    [<TestCase( [|"ab"; ""|], [|"ab"; ""|], [|2; 2; 0; 0|] )>]
    [<TestCase( [|"ab"    |], [|"ab"    |], [|2; 2; 0; 0|] )>]
    member _.EnterInsertingAtSol start end_ chars =
        let startAnchorChar, startCursorChar, endLine, endCommonChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command EnterInsertingAtSol
        assertLinesSelections end_ [
            endLine, endCommonChar, endLine, endCommonChar
        ]

    [<TestCase( [|"ab"    |], [|"ab"    |], [|0; 1; 0; 2|] )>]
    [<TestCase( [|"ab"; ""|], [|"ab"; ""|], [|1; 2; 0; 2|] )>]
    [<TestCase( [|"ab"; ""|], [|"ab"; ""|], [|2; 2; 0; 2|] )>]
    [<TestCase( [|"ab"    |], [|"ab"    |], [|1; 2; 0; 2|] )>]
    [<TestCase( [|"ab"    |], [|"ab"    |], [|2; 2; 0; 2|] )>]
    member _.EnterAppendingAtEol start end_ chars =
        let startAnchorChar, startCursorChar, endLine, endCommonChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command EnterAppendingAtEol
        assertLinesSelections end_ [
            endLine, endCommonChar, endLine, endCommonChar
        ]

    // InsertChar <tab> --------------------------------------------------------

    [<TestCase( [|""  |], [|"\t"  |], [|0; 0; 0; 1|] )>]
    [<TestCase( [|"ab"|], [|"\tab"|], [|0; 0; 0; 1|] )>]
    [<TestCase( [|"ab"|], [|"a\tb"|], [|1; 1; 0; 2|] )>]
    [<TestCase( [|"ab"|], [|"ab\t"|], [|2; 2; 0; 3|] )>]
    member _.InsertCharTabAsSingleChar start end_ chars =
        let startAnchorChar, startCursorChar, endLine, endCommonChar =
            toTuple4 chars

        contextRef.Value <- {
            contextRef.Value with tabBySpaces = false
        }

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command (InsertChar '\t')

        contextRef.Value <- {
            contextRef.Value with tabBySpaces = true
        }

        assertLinesSelections end_ [
            endLine, endCommonChar, endLine, endCommonChar
        ]

    [<TestCase( [|""     |], [|"    "     |], [|0; 0; 0; 4|] )>]
    [<TestCase( [|"ab"   |], [|"    ab"   |], [|0; 0; 0; 4|] )>]
    [<TestCase( [|"ab"   |], [|"a   b"    |], [|1; 1; 0; 4|] )>]
    [<TestCase( [|"ab"   |], [|"ab  "     |], [|2; 2; 0; 4|] )>]

    [<TestCase( [|" "    |], [|"    "     |], [|1; 1; 0; 4|] )>]
    [<TestCase( [|" ab"  |], [|"    ab"   |], [|1; 1; 0; 4|] )>]
    [<TestCase( [|" ab"  |], [|" a  b"    |], [|2; 2; 0; 4|] )>]
    [<TestCase( [|" ab"  |], [|" ab "     |], [|3; 3; 0; 4|] )>]

    [<TestCase( [|"  "   |], [|"    "     |], [|2; 2; 0; 4|] )>]
    [<TestCase( [|"  ab" |], [|"    ab"   |], [|2; 2; 0; 4|] )>]
    [<TestCase( [|"  ab" |], [|"  a b"    |], [|3; 3; 0; 4|] )>]
    [<TestCase( [|"  ab" |], [|"  ab    " |], [|4; 4; 0; 8|] )>]

    [<TestCase( [|"   "  |], [|"    "     |], [|3; 3; 0; 4|] )>]
    [<TestCase( [|"   ab"|], [|"    ab"   |], [|3; 3; 0; 4|] )>]
    [<TestCase( [|"   ab"|], [|"   a    b"|], [|4; 4; 0; 8|] )>]
    [<TestCase( [|"   ab"|], [|"   ab   " |], [|5; 5; 0; 8|] )>]

    member _.InsertCharTabAsSpaces start end_ chars =
        let startAnchorChar, startCursorChar, endLine, endCommonChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command (InsertChar '\t')
        assertLinesSelections end_ [
            endLine, endCommonChar, endLine, endCommonChar
        ]

    // InsertChars -------------------------------------------------------------

    [<TestCase( [|""  |], [|"ccc"  |], [|0; 0; 0; 3|] )>]
    [<TestCase( [|"ab"|], [|"cccab"|], [|0; 0; 0; 3|] )>]
    [<TestCase( [|"ab"|], [|"acccb"|], [|1; 1; 0; 4|] )>]
    [<TestCase( [|"ab"|], [|"abccc"|], [|2; 2; 0; 5|] )>]
    member _.InsertChars start end_ chars =
        let startAnchorChar, startCursorChar, endLine, endCommonChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command (InsertChars (stringToChars "ccc"))
        assertLinesSelections end_ [
            endLine, endCommonChar, endLine, endCommonChar
        ]

    // YankAndDelete, Replace --------------------------------------------------

    [<TestCase( [|"acdb"|], [|"ab"|], [|1; 2; 1; 1|] )>]
    member _.YankAndDelete start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        setRegister ["efgh"]
        command (YankAndDelete defReg)
        assertLinesSelections end_ [
            0, endAnchorChar, 0, endCursorChar
        ]
        assertRegister ["cd"]

    [<TestCase( [|"acdb"|], [|"aefghb"|], [|1; 2; 1; 4|] )>]
    member _.Replace start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        setRegister ["efgh"]
        command (Replace defReg)
        assertLinesSelections end_ [
            0, endAnchorChar, 0, endCursorChar
        ]
        assertRegister ["efgh"]

    // AlignSelections ---------------------------------------------------------

    [<TestCase( [|"abc"; "de"|], [|"  abc"; "de"|], [|0; 1; 2; 3|] )>]
    [<TestCase( [|"abc"; "de"|], [|"a bc" ; "de"|], [|1; 2; 2; 3|] )>]
    [<TestCase( [|"abc"; "de"|], [|"abc"  ; "de"|], [|2; 3; 2; 3|] )>]
    [<TestCase( [|""   ; "cd"|], [|"  "   ; "cd"|], [|0; 0; 2; 2|] )>]
    [<TestCase( [|"a"  ; "cd"|], [|"a "   ; "cd"|], [|1; 1; 2; 2|] )>]
    [<TestCase( [|"ab" ; "cd"|], [|"ab"   ; "cd"|], [|2; 2; 2; 2|] )>]
    member _.Delete start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
            1, 2, 1, 2
        ]
        command AlignSelections
        assertLinesSelections end_ [
            0, endAnchorChar, 0, endCursorChar
            1, 2, 1, 2
        ]
