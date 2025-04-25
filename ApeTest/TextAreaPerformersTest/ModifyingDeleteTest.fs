module TextAreaModifyingDeleteTest

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
type ModifyingDeleteTest () =
    let myBuffer = new TextAreaBuffer (contextRef, UserMessages (), Registers (), "")

    // initialization

    let init lines anchorLine anchorChar cursorLine cursorChar =
        myBuffer.LoadStrings lines

        myBuffer.Selections.Clear ()
        myBuffer.Selections.Add {
            Selection_Default with
                first = { line = anchorLine; char = anchorChar }
                last  = { line = cursorLine; char = cursorChar }
                isForward = true
        }

    let toTuple3 (a: 'T array) =
        a[0], a[1], a[2]

    // command and simple assertions

    let performCommand (command: obj) count =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand false false (ModifyingCommand x) count
        | _                        ->
            invalidOp ""

    let assertLines expLines =
        Assert.AreEqual (expLines, myBuffer.Lines)

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

    let commandAssertLinesSelection
        command lines anchorLine anchorChar cursorLine cursorChar =

        performCommand command 1
        assertLines lines
        assertAnchor anchorLine anchorChar
        assertCursor cursorLine cursorChar

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // Delete ------------------------------------------------------------------

    [<TestCase( [|"cd"  ; "xy"|], [|""  ; "xy"|], [|0; 1; 0|] )>]  // empty line
    [<TestCase( [|"cdab"; "xy"|], [|"ab"; "xy"|], [|0; 1; 0|] )>]  // first char of line
    [<TestCase( [|"acdb"; "xy"|], [|"ab"; "xy"|], [|1; 2; 1|] )>]  // last char of line
    [<TestCase( [|"abcd"; "xy"|], [|"ab"; "xy"|], [|2; 3; 2|] )>]  // EOL
    [<TestCase( [|"cd"        |], [|""        |], [|0; 1; 0|] )>]  // empty file
    [<TestCase( [|"abcd"      |], [|"ab"      |], [|2; 3; 2|] )>]  // EOF
    member _.Delete_OneLine start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLinesSelection Delete
            end_ 0 endChar 0 endChar

    [<TestCase( [|"cd"  ; "ef"  ; "xy"|], [|""  ; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|"cd"  ; "efab"; "xy"|], [|"ab"; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|"acd" ; "efb" ; "xy"|], [|"ab"; "xy"|], [|1; 1; 1|] )>]
    [<TestCase( [|"abcd"; "ef"  ; "xy"|], [|"ab"; "xy"|], [|2; 1; 2|] )>]
    [<TestCase( [|"cd"  ; "ef"        |], [|""        |], [|0; 1; 0|] )>]
    [<TestCase( [|"abcd"; "ef"        |], [|"ab"      |], [|2; 1; 2|] )>]
    member _.Delete_TwoLines start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 1 cursorChar

        commandAssertLinesSelection Delete
            end_ 0 endChar 0 endChar

    [<TestCase( [|"cd"  ; "ef"; "gh"  ; "xy"|], [|""  ; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|"cd"  ; "ef"; "ghab"; "xy"|], [|"ab"; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|"acd" ; "ef"; "ghb" ; "xy"|], [|"ab"; "xy"|], [|1; 1; 1|] )>]
    [<TestCase( [|"abcd"; "ef"; "gh"  ; "xy"|], [|"ab"; "xy"|], [|2; 1; 2|] )>]
    [<TestCase( [|"cd"  ; "ef"; "gh"        |], [|""        |], [|0; 1; 0|] )>]
    [<TestCase( [|"abcd"; "ef"; "gh"        |], [|"ab"      |], [|2; 1; 2|] )>]
    member _.Delete_ThreeLines start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 2 cursorChar

        commandAssertLinesSelection Delete
            end_ 0 endChar 0 endChar

    [<TestCase( [|""  ; ""  ; "xy"|], [|""  ; "xy"|], [|0; 0; 0|] )>]
    [<TestCase( [|""  ; "ab"; "xy"|], [|"ab"; "xy"|], [|0; 0; 0|] )>]
    [<TestCase( [|"a" ; "b" ; "xy"|], [|"ab"; "xy"|], [|1; 1; 1|] )>]
    [<TestCase( [|"ab"; ""  ; "xy"|], [|"ab"; "xy"|], [|2; 2; 2|] )>]
    [<TestCase( [|""              |], [|""        |], [|0; 0; 0|] )>]
    [<TestCase( [|"ab"            |], [|"ab"      |], [|2; 2; 2|] )>]
    member _.Delete_NewLine start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLinesSelection Delete
            end_ 0 endChar 0 endChar

    [<TestCase( [|""    ; "cd"  ; "xy"|], [|""  ; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|""    ; "cdab"; "xy"|], [|"ab"; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|"a"   ; "cdb" ; "xy"|], [|"ab"; "xy"|], [|1; 1; 1|] )>]
    [<TestCase( [|"ab"  ; "cd"  ; "xy"|], [|"ab"; "xy"|], [|2; 1; 2|] )>]
    [<TestCase( [|""    ; "cd"        |], [|""        |], [|0; 1; 0|] )>]
    [<TestCase( [|"ab"  ; "cd"        |], [|"ab"      |], [|2; 1; 2|] )>]
    member _.Delete_NewLineAndOneLine start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 1 cursorChar

        commandAssertLinesSelection Delete
            end_ 0 endChar 0 endChar

    [<TestCase( [|"cd"  ; ""  ; "xy"|], [|""  ; "xy"|], [|0; 2; 0|] )>]
    [<TestCase( [|"cd"  ; "ab"; "xy"|], [|"ab"; "xy"|], [|0; 2; 0|] )>]
    [<TestCase( [|"acd" ; "b" ; "xy"|], [|"ab"; "xy"|], [|1; 3; 1|] )>]
    [<TestCase( [|"abcd"; ""  ; "xy"|], [|"ab"; "xy"|], [|2; 4; 2|] )>]
    [<TestCase( [|"cd"              |], [|""        |], [|0; 2; 0|] )>]
    [<TestCase( [|"abcd"            |], [|"ab"      |], [|2; 4; 2|] )>]
    member _.Delete_OneLineAndNewLine start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLinesSelection Delete
            end_ 0 endChar 0 endChar

    [<TestCase( [|""  ; "cd"; "ef"  ; "xy"|], [|""  ; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|""  ; "cd"; "efab"; "xy"|], [|"ab"; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|"a" ; "cd"; "efb" ; "xy"|], [|"ab"; "xy"|], [|1; 1; 1|] )>]
    [<TestCase( [|"ab"; "cd"; "ef"  ; "xy"|], [|"ab"; "xy"|], [|2; 1; 2|] )>]
    [<TestCase( [|""  ; "cd"; "ef"        |], [|""        |], [|0; 1; 0|] )>]
    [<TestCase( [|"ab"; "cd"; "ef"        |], [|"ab"      |], [|2; 1; 2|] )>]
    member _.Delete_NewLineAndTwoLines start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 2 cursorChar

        commandAssertLinesSelection Delete
            end_ 0 endChar 0 endChar

    [<TestCase( [|"cd"  ; "ef"; ""  ; "xy"|], [|""  ; "xy"|], [|0; 2; 0|] )>]
    [<TestCase( [|"cd"  ; "ef"; "ab"; "xy"|], [|"ab"; "xy"|], [|0; 2; 0|] )>]
    [<TestCase( [|"acd" ; "ef"; "b" ; "xy"|], [|"ab"; "xy"|], [|1; 2; 1|] )>]
    [<TestCase( [|"abcd"; "ef"; ""  ; "xy"|], [|"ab"; "xy"|], [|2; 2; 2|] )>]
    [<TestCase( [|"cd"  ; "ef"            |], [|""        |], [|0; 2; 0|] )>]
    [<TestCase( [|"abcd"; "ef"            |], [|"ab"      |], [|2; 2; 2|] )>]
    member _.Delete_TwoLinesAndNewLine start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 1 cursorChar

        commandAssertLinesSelection Delete
            end_ 0 endChar 0 endChar
