module TextAreaTextRangesTest

open NUnit.Framework
open System

open Commands.InCommands
open Registers
open Selection
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef 80 25

[<TestFixture>]
type TextRangesTest () =
    let myBuffer = makeTextAreaBuffer (contextRef, UserMessages (), Registers (), "")

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
        | :? TextRangesCommand as x ->
            myBuffer.PerformCommand false false (TextRangesCommand x) count
        | _                         ->
            invalidOp ""

    let command command =
        performCommand command 1

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

    // FillWithChar ------------------------------------------------------------

    [<TestCase( [|"ab"; "cd"; "ef"|], [|"ab"; "cX"; "ef"|], [|1; 1; 1; 1|] )>]
    [<TestCase( [|"ab"; "cd"; "ef"|], [|"ab"; "XX"; "ef"|], [|1; 0; 1; 2|] )>]
    [<TestCase( [|"ab"; "cd"; "ef"|], [|"ab"; "XX"; "ef"|], [|1; 0; 1; 3|] )>]
    [<TestCase( [|"ab"; "cd"; "ef"|], [|"aX"; "XX"; "Xf"|], [|0; 1; 2; 0|] )>]
    [<TestCase( [|"ab"; "cd"; "ef"|], [|"XX"; "XX"; "XX"|], [|0; 0; 2; 2|] )>]
    [<TestCase( [|"ab"; "cd"; "ef"|], [|"XX"; "XX"; "XX"|], [|0; 0; 2; 3|] )>]
    member _.FillWithChar start end_ chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init start [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]
        command (FillWithChar 'X')
        assertLinesSelections end_ [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]

    // ToUppercase, InvertCase -------------------------------------------------

    [<TestCase( [|"Ab="|], [|"AB="|], [|0; 0; 0; 2|] )>]
    member _.ToUppercase start end_ chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init start [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]
        command ToUppercase
        assertLinesSelections end_ [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]

    [<TestCase( [|"Ab="|], [|"aB="|], [|0; 0; 0; 2|] )>]
    member _.InvertCase start end_ chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init start [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]
        command InvertCase
        assertLinesSelections end_ [
            anchorLine, anchorChar, cursorLine, cursorChar
        ]

    // TabsToSpaces, SpacesToTabs ----------------------------------------------

    [<TestCase( [|"ab\tcd\tef"|], [|"ab\tcd  ef"|], [|4; 6; 4; 7|] )>]
    [<TestCase( [|"ab\tcd\t"  |], [|"ab\tcd  "  |], [|4; 5; 4; 6|] )>]
    [<TestCase( [|"ab\tcd"    |], [|"ab  cd"    |], [|2; 2; 2; 3|] )>]
    [<TestCase( [|"ab\t"      |], [|"ab  "      |], [|2; 2; 2; 3|] )>]
    member _.TabsToSpaces start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command TabsToSpaces
        assertLinesSelections end_ [
            0, endAnchorChar, 0, endCursorChar
        ]

    [<TestCase( [|"ab\tcd  ef"  |], [|"ab\tcd\tef"  |], [|1; 7; 1; 6|] )>]
    [<TestCase( [|"ab\tcd    ef"|], [|"ab\tcd\t  ef"|], [|1; 9; 1; 8|] )>]
    [<TestCase( [|"ab\tcd  "    |], [|"ab\tcd\t"    |], [|1; 6; 1; 5|] )>]
    [<TestCase( [|"ab\tcd    "  |], [|"ab\tcd\t  "  |], [|1; 8; 1; 7|] )>]
    [<TestCase( [|"ab  cd"      |], [|"ab\tcd"      |], [|2; 3; 2; 2|] )>]
    [<TestCase( [|"ab    cd"    |], [|"ab\t  cd"    |], [|2; 5; 2; 4|] )>]
    [<TestCase( [|"ab  "        |], [|"ab\t"        |], [|2; 3; 2; 2|] )>]
    [<TestCase( [|"ab    "      |], [|"ab\t  "      |], [|2; 5; 2; 4|] )>]
    member _.SpacesToTabs start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command SpacesToTabs
        assertLinesSelections end_ [
            0, endAnchorChar, 0, endCursorChar
        ]

    // RegexEscape -------------------------------------------------------------

    [<TestCase( [|"ab(cd(ef"|], [|"ab(cd\(ef"|], [|4; 6; 4; 7|] )>]
    [<TestCase( [|"ab(cd("  |], [|"ab(cd\("  |], [|4; 5; 4; 6|] )>]
    [<TestCase( [|"ab(cd"   |], [|"ab\(cd"   |], [|2; 2; 2; 3|] )>]
    [<TestCase( [|"ab("     |], [|"ab\("     |], [|2; 2; 2; 3|] )>]
    member _.RegexEscape start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command RegexEscape
        assertLinesSelections end_ [
            0, endAnchorChar, 0, endCursorChar
        ]

    // Indent, Unindent --------------------------------------------------------

    [<TestCase( [|""      |], [|"    "      |], [|0; 0; 4; 4|] )>]
    [<TestCase( [|"abcd"  |], [|"    abcd"  |], [|0; 1; 4; 5|] )>]
    [<TestCase( [|" abcd" |], [|"     abcd" |], [|0; 1; 4; 5|] )>]
    [<TestCase( [|"\tabcd"|], [|"    \tabcd"|], [|0; 1; 4; 5|] )>]
    [<TestCase( [|"abcd"  |], [|"    abcd"  |], [|4; 4; 8; 8|] )>]
    member _.Indent_BySpaces start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command Indent
        assertLinesSelections end_ [
            0, endAnchorChar, 0, endCursorChar
        ]

    [<TestCase( [|""      |], [|"\t"      |], [|0; 0; 1; 1|] )>]
    [<TestCase( [|"abcd"  |], [|"\tabcd"  |], [|0; 1; 1; 2|] )>]
    [<TestCase( [|" abcd" |], [|"\t abcd" |], [|0; 1; 1; 2|] )>]
    [<TestCase( [|"\tabcd"|], [|"\t\tabcd"|], [|0; 1; 1; 2|] )>]
    [<TestCase( [|"abcd"  |], [|"\tabcd"  |], [|4; 4; 5; 5|] )>]
    member _.Indent_ByTab start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        contextRef.Value <- {
            contextRef.Value with tabBySpaces = false
        }

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command Indent

        contextRef.Value <- {
            contextRef.Value with tabBySpaces = true
        }

        assertLinesSelections end_ [
            0, endAnchorChar, 0, endCursorChar
        ]

    [<TestCase( [|"    "      |], [|""      |], [|3; 4; 0; 0|] )>]  // from Indent_BySpaces
    [<TestCase( [|"    abcd"  |], [|"abcd"  |], [|3; 5; 0; 1|] )>]
    [<TestCase( [|"     abcd" |], [|" abcd" |], [|3; 5; 0; 1|] )>]
    [<TestCase( [|"    \tabcd"|], [|"\tabcd"|], [|3; 5; 0; 1|] )>]
    [<TestCase( [|"    abcd"  |], [|"abcd"  |], [|8; 8; 4; 4|] )>]
    [<TestCase( [|"\t"        |], [|""      |], [|0; 1; 0; 0|] )>]  // from Indent_ByTab
    [<TestCase( [|"\tabcd"    |], [|"abcd"  |], [|0; 2; 0; 1|] )>]
    [<TestCase( [|"\t abcd"   |], [|" abcd" |], [|0; 2; 0; 1|] )>]
    [<TestCase( [|"\t\tabcd"  |], [|"\tabcd"|], [|0; 2; 0; 1|] )>]
    [<TestCase( [|"\tabcd"    |], [|"abcd"  |], [|5; 5; 4; 4|] )>]
    [<TestCase( [|""          |], [|""      |], [|0; 0; 0; 0|] )>]  // not from Indent
    [<TestCase( [|"  "        |], [|""      |], [|0; 2; 0; 0|] )>]
    [<TestCase( [|"  abcd"    |], [|"abcd"  |], [|0; 6; 0; 4|] )>]
    member _.Unindent start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start [
            0, startAnchorChar, 0, startCursorChar
        ]
        command Unindent
        assertLinesSelections end_ [
            0, endAnchorChar, 0, endCursorChar
        ]
