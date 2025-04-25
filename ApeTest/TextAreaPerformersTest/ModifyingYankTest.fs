module TextAreaModifyingYankTest

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
type ModifyingYankTest () =
    let myRegisters = Registers ()
    let myBuffer    = new TextAreaBuffer (contextRef, UserMessages (), myRegisters, "")

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

    let toTuple2 (a: 'T array) =
        a[0], a[1]

    // command and simple assertions

    let performCommand (command: obj) count =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand false false (ModifyingCommand x) count
        | _                        ->
            invalidOp ""

    let getRegister () =
        myRegisters.GetSlot DefaultRegister 0
        |> Option.get
        |> Seq.map charsToString
        |> Seq.toList

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

    let assertRegister expRegister =
        Assert.AreEqual (expRegister, getRegister ())

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

    // Yank --------------------------------------------------------------------

    [<TestCase( [|"cd"  ; "xy"|], [|0; 1|] )>]  // empty line
    [<TestCase( [|"cdab"; "xy"|], [|0; 1|] )>]  // first char of line
    [<TestCase( [|"acdb"; "xy"|], [|1; 2|] )>]  // last char of line
    [<TestCase( [|"abcd"; "xy"|], [|2; 3|] )>]  // EOL
    [<TestCase( [|"cd"        |], [|0; 1|] )>]  // empty file
    [<TestCase( [|"abcd"      |], [|2; 3|] )>]  // EOF
    member _.Yank_OneLine start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLinesSelection (Yank defReg)
            start 0 anchorChar 0 cursorChar

        assertRegister ["cd"]

    [<TestCase( [|"cd"  ; "ef"  ; "xy"|], [|0; 1|] )>]
    [<TestCase( [|"cd"  ; "efab"; "xy"|], [|0; 1|] )>]
    [<TestCase( [|"acd" ; "efb" ; "xy"|], [|1; 1|] )>]
    [<TestCase( [|"abcd"; "ef"  ; "xy"|], [|2; 1|] )>]
    [<TestCase( [|"cd"  ; "ef"        |], [|0; 1|] )>]
    [<TestCase( [|"abcd"; "ef"        |], [|2; 1|] )>]
    member _.Yank_TwoLines start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 1 cursorChar

        commandAssertLinesSelection (Yank defReg)
            start 0 anchorChar 1 cursorChar

        assertRegister ["cd"; "ef"]

    [<TestCase( [|"cd"  ; "ef"; "gh"  ; "xy"|], [|0; 1|] )>]
    [<TestCase( [|"cd"  ; "ef"; "ghab"; "xy"|], [|0; 1|] )>]
    [<TestCase( [|"acd" ; "ef"; "ghb" ; "xy"|], [|1; 1|] )>]
    [<TestCase( [|"abcd"; "ef"; "gh"  ; "xy"|], [|2; 1|] )>]
    [<TestCase( [|"cd"  ; "ef"; "gh"        |], [|0; 1|] )>]
    [<TestCase( [|"abcd"; "ef"; "gh"        |], [|2; 1|] )>]
    member _.Yank_ThreeLines start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 2 cursorChar

        commandAssertLinesSelection (Yank defReg)
            start 0 anchorChar 2 cursorChar

        assertRegister ["cd"; "ef"; "gh"]

    [<TestCase( [|""  ; ""  ; "xy"|], [|0; 0|] )>]
    [<TestCase( [|""  ; "ab"; "xy"|], [|0; 0|] )>]
    [<TestCase( [|"a" ; "b" ; "xy"|], [|1; 1|] )>]
    [<TestCase( [|"ab"; ""  ; "xy"|], [|2; 2|] )>]
    [<TestCase( [|""              |], [|0; 0|] )>]
    [<TestCase( [|"ab"            |], [|2; 2|] )>]
    member _.Yank_NewLine start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLinesSelection (Yank defReg)
            start 0 anchorChar 0 cursorChar

        assertRegister [""; ""]

    [<TestCase( [|""    ; "cd"  ; "xy"|], [|0; 1|] )>]
    [<TestCase( [|""    ; "cdab"; "xy"|], [|0; 1|] )>]
    [<TestCase( [|"a"   ; "cdb" ; "xy"|], [|1; 1|] )>]
    [<TestCase( [|"ab"  ; "cd"  ; "xy"|], [|2; 1|] )>]
    [<TestCase( [|""    ; "cd"        |], [|0; 1|] )>]
    [<TestCase( [|"ab"  ; "cd"        |], [|2; 1|] )>]
    member _.Yank_NewLineAndOneLine start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 1 cursorChar

        commandAssertLinesSelection (Yank defReg)
            start 0 anchorChar 1 cursorChar

        assertRegister [""; "cd"]

    [<TestCase( [|"cd"  ; ""  ; "xy"|], [|0; 2|] )>]
    [<TestCase( [|"cd"  ; "ab"; "xy"|], [|0; 2|] )>]
    [<TestCase( [|"acd" ; "b" ; "xy"|], [|1; 3|] )>]
    [<TestCase( [|"abcd"; ""  ; "xy"|], [|2; 4|] )>]
    [<TestCase( [|"cd"              |], [|0; 2|] )>]
    [<TestCase( [|"abcd"            |], [|2; 4|] )>]
    member _.Yank_OneLineAndNewLine start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLinesSelection (Yank defReg)
            start 0 anchorChar 0 cursorChar

        assertRegister ["cd"; ""]

    [<TestCase( [|""  ; "cd"; "ef"  ; "xy"|], [|0; 1|] )>]
    [<TestCase( [|""  ; "cd"; "efab"; "xy"|], [|0; 1|] )>]
    [<TestCase( [|"a" ; "cd"; "efb" ; "xy"|], [|1; 1|] )>]
    [<TestCase( [|"ab"; "cd"; "ef"  ; "xy"|], [|2; 1|] )>]
    [<TestCase( [|""  ; "cd"; "ef"        |], [|0; 1|] )>]
    [<TestCase( [|"ab"; "cd"; "ef"        |], [|2; 1|] )>]
    member _.Yank_NewLineAndTwoLines start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 2 cursorChar

        commandAssertLinesSelection (Yank defReg)
            start 0 anchorChar 2 cursorChar

        assertRegister [""; "cd"; "ef"]

    [<TestCase( [|"cd"  ; "ef"; ""  ; "xy"|], [|0; 2|] )>]
    [<TestCase( [|"cd"  ; "ef"; "ab"; "xy"|], [|0; 2|] )>]
    [<TestCase( [|"acd" ; "ef"; "b" ; "xy"|], [|1; 2|] )>]
    [<TestCase( [|"abcd"; "ef"; ""  ; "xy"|], [|2; 2|] )>]
    [<TestCase( [|"cd"  ; "ef"            |], [|0; 2|] )>]
    [<TestCase( [|"abcd"; "ef"            |], [|2; 2|] )>]
    member _.Yank_TwoLinesAndNewLine start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 1 cursorChar

        commandAssertLinesSelection (Yank defReg)
            start 0 anchorChar 1 cursorChar

        assertRegister ["cd"; "ef"; ""]
