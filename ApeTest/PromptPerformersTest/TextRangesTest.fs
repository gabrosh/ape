module PromptTextRangesTest

open NUnit.Framework
open System

open Commands.InCommands
open PromptBuffer
open Registers
open Selection
open UserMessages

let contextRef      = TestUtils.makeContextRef 80 25
let extraContextRef = TestUtils.makeExtraContextRef 1

[<TestFixture>]
type TextRangesTest () =
    let myBuffer = new PromptBuffer (contextRef, extraContextRef, UserMessages (), Registers ())

    // initialization

    let init line anchorLine anchorChar cursorLine cursorChar =
        myBuffer.LoadString line

        myBuffer.Selections.Clear ()
        myBuffer.Selections.Add {
            Selection_Default with
                first = { line = anchorLine; char = anchorChar }
                last  = { line = cursorLine; char = cursorChar }
                isForward = true
        }

    let toTuple4 (a: 'T array) =
        a[0], a[1], a[2], a[3]

    // command and simple assertions

    let performCommand (command: obj) =
        match command with
        | :? TextRangesCommand as x ->
            myBuffer.PerformCommand false false (TextRangesCommand x)
        | _                         ->
            invalidOp ""

    let command command =
        performCommand command

    let assertLine expLine =
        Assert.AreEqual ([expLine], myBuffer.Lines)

    let assertCursor cursorLine cursorChar =
        Assert.AreEqual (
            ( cursorLine                , cursorChar                ),
            ( myBuffer.Main.Cursor.line , myBuffer.Main.Cursor.char )
        )

    let assertAnchor anchorLine anchorChar =
        Assert.AreEqual (
            ( anchorLine                , anchorChar                ),
            ( myBuffer.Main.Anchor.line , myBuffer.Main.Anchor.char )
        )

    // commands followed by assertion(s)

    let assertLineSelection
        line anchorLine anchorChar cursorLine cursorChar =

        assertLine line
        assertAnchor anchorLine anchorChar
        assertCursor cursorLine cursorChar

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // ToUppercase, InvertCase -------------------------------------------------

    [<TestCase( "Ab=", "AB=", [|0; 0; 0; 2|] )>]
    member _.ToUppercase start end_ chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init start
            anchorLine anchorChar cursorLine cursorChar

        command ToUppercase

        assertLineSelection end_
            anchorLine anchorChar cursorLine cursorChar

    [<TestCase( "Ab=", "aB=", [|0; 0; 0; 2|] )>]
    member _.InvertCase start end_ chars =
        let anchorLine, anchorChar, cursorLine, cursorChar =
            toTuple4 chars

        init start anchorLine anchorChar cursorLine cursorChar

        command InvertCase

        assertLineSelection end_
            anchorLine anchorChar cursorLine cursorChar

    // RegexEscape -------------------------------------------------------------

    [<TestCase( "ab(cd(ef", "ab(cd\(ef", [|4; 6; 4; 7|] )>]
    [<TestCase( "ab(cd("  , "ab(cd\("  , [|4; 5; 4; 6|] )>]
    [<TestCase( "ab(cd"   , "ab\(cd"   , [|2; 2; 2; 3|] )>]
    [<TestCase( "ab("     , "ab\("     , [|2; 2; 2; 3|] )>]
    member _.RegexEscape start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start
            0 startAnchorChar 0 startCursorChar

        command RegexEscape

        assertLineSelection end_
            0 endAnchorChar 0 endCursorChar

    // Indent, Unindent --------------------------------------------------------

    [<TestCase( ""      , "    "      , [|0; 0; 4; 4|] )>]
    [<TestCase( "abcd"  , "    abcd"  , [|0; 1; 4; 5|] )>]
    [<TestCase( " abcd" , "     abcd" , [|0; 1; 4; 5|] )>]
    [<TestCase( "\tabcd", "    \tabcd", [|0; 1; 4; 5|] )>]
    [<TestCase( "abcd"  , "    abcd"  , [|4; 4; 8; 8|] )>]
    member _.Indent_BySpaces start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start
            0 startAnchorChar 0 startCursorChar

        command Indent

        assertLineSelection end_
            0 endAnchorChar 0 endCursorChar

    [<TestCase( ""      , "\t"      , [|0; 0; 1; 1|] )>]
    [<TestCase( "abcd"  , "\tabcd"  , [|0; 1; 1; 2|] )>]
    [<TestCase( " abcd" , "\t abcd" , [|0; 1; 1; 2|] )>]
    [<TestCase( "\tabcd", "\t\tabcd", [|0; 1; 1; 2|] )>]
    [<TestCase( "abcd"  , "\tabcd"  , [|4; 4; 5; 5|] )>]
    member _.Indent_ByTab start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        contextRef.Value <- {
            contextRef.Value with tabBySpaces = false
        }

        init start
            0 startAnchorChar 0 startCursorChar

        command Indent

        contextRef.Value <- {
            contextRef.Value with tabBySpaces = true
        }

        assertLineSelection end_
            0 endAnchorChar 0 endCursorChar

    [<TestCase( "    "      , ""      , [|3; 4; 0; 0|] )>]  // from Indent_BySpaces
    [<TestCase( "    abcd"  , "abcd"  , [|3; 5; 0; 1|] )>]
    [<TestCase( "     abcd" , " abcd" , [|3; 5; 0; 1|] )>]
    [<TestCase( "    \tabcd", "\tabcd", [|3; 5; 0; 1|] )>]
    [<TestCase( "    abcd"  , "abcd"  , [|8; 8; 4; 4|] )>]
    [<TestCase( "\t"        , ""      , [|0; 1; 0; 0|] )>]  // from Indent_ByTab
    [<TestCase( "\tabcd"    , "abcd"  , [|0; 2; 0; 1|] )>]
    [<TestCase( "\t abcd"   , " abcd" , [|0; 2; 0; 1|] )>]
    [<TestCase( "\t\tabcd"  , "\tabcd", [|0; 2; 0; 1|] )>]
    [<TestCase( "\tabcd"    , "abcd"  , [|5; 5; 4; 4|] )>]
    [<TestCase( ""          , ""      , [|0; 0; 0; 0|] )>]  // not from Indent
    [<TestCase( "  "        , ""      , [|0; 2; 0; 0|] )>]
    [<TestCase( "  abcd"    , "abcd"  , [|0; 6; 0; 4|] )>]
    member _.Unindent start end_ chars =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 chars

        init start
            0 startAnchorChar 0 startCursorChar

        command Unindent

        assertLineSelection end_
            0 endAnchorChar 0 endCursorChar
