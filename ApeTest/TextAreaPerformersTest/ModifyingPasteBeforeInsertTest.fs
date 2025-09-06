module TextAreaModifyingPasteBeforeInsertTest

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
type ModifyingPasteBeforeInsertTest () =
    let myRegisters = Registers ()
    let myBuffer    = makeTextAreaBuffer (contextRef, UserMessages (), myRegisters, "")

    // initialization

    let init lines startLine startChar =
        myBuffer.LoadStrings lines

        myBuffer.Selections.Clear ()
        myBuffer.Selections.Add {
            Selection_Default with
                first = { line = startLine; char = startChar }
                last  = { line = startLine; char = startChar }
                isForward = true
        }

    let toTuple3 (a: 'T array) =
        a[0], a[1], a[2]

    let toTuple5 (a: 'T array) =
        a[0], a[1], a[2], a[3], a[4]

    // command and simple assertions

    let performCommand (command: obj) count =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand false false (ModifyingCommand x) count
        | _                        ->
            invalidOp ""

    let setRegister lines =
        let lines = Lines (lines |> Seq.map stringToChars)
        myRegisters.ApplyToSlot DefaultRegister 0 lines

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

    // PasteBefore -------------------------------------------------------------

    [<TestCase( [|""  ; "xy"|], [|"cd"  ; "xy"|], [|0; 0; 2|] )>]  // empty line
    [<TestCase( [|"ab"; "xy"|], [|"cdab"; "xy"|], [|0; 0; 2|] )>]  // first char of line
    [<TestCase( [|"ab"; "xy"|], [|"acdb"; "xy"|], [|1; 0; 3|] )>]  // last char of line
    [<TestCase( [|"ab"; "xy"|], [|"abcd"; "xy"|], [|2; 0; 4|] )>]  // EOL
    [<TestCase( [|""        |], [|"cd"        |], [|0; 0; 2|] )>]  // empty file
    [<TestCase( [|"ab"      |], [|"abcd"      |], [|2; 0; 4|] )>]  // EOF
    member _.PasteBefore_OneLine start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister ["cd"]

        commandAssertLinesSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( [|""  ; "xy"|], [|"cd"  ; "ef"  ; "xy"|], [|0; 1; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"cd"  ; "efab"; "xy"|], [|0; 1; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"acd" ; "efb" ; "xy"|], [|1; 1; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"abcd"; "ef"  ; "xy"|], [|2; 1; 2|] )>]
    [<TestCase( [|""        |], [|"cd"  ; "ef"        |], [|0; 1; 2|] )>]
    [<TestCase( [|"ab"      |], [|"abcd"; "ef"        |], [|2; 1; 2|] )>]
    member _.PasteBefore_TwoLines start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister ["cd"; "ef"]

        commandAssertLinesSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( [|""  ; "xy"|], [|"cd"  ; "ef"; "gh"  ; "xy"|], [|0; 2; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"cd"  ; "ef"; "ghab"; "xy"|], [|0; 2; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"acd" ; "ef"; "ghb" ; "xy"|], [|1; 2; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"abcd"; "ef"; "gh"  ; "xy"|], [|2; 2; 2|] )>]
    [<TestCase( [|""        |], [|"cd"  ; "ef"; "gh"        |], [|0; 2; 2|] )>]
    [<TestCase( [|"ab"      |], [|"abcd"; "ef"; "gh"        |], [|2; 2; 2|] )>]
    member _.PasteBefore_ThreeLines start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister ["cd"; "ef"; "gh"]

        commandAssertLinesSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( [|""  ; "xy"|], [|""  ; ""  ; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|"ab"; "xy"|], [|""  ; "ab"; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"a" ; "b" ; "xy"|], [|1; 1; 0|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"ab"; ""  ; "xy"|], [|2; 1; 0|] )>]
    [<TestCase( [|""        |], [|""  ; ""        |], [|0; 1; 0|] )>]
    [<TestCase( [|"ab"      |], [|"ab"; ""        |], [|2; 1; 0|] )>]
    member _.PasteBefore_NewLine start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister [""; ""]

        commandAssertLinesSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( [|""  ; "xy"|], [|""    ; "cd"  ; "xy"|], [|0; 1; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|""    ; "cdab"; "xy"|], [|0; 1; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"a"   ; "cdb" ; "xy"|], [|1; 1; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"ab"  ; "cd"  ; "xy"|], [|2; 1; 2|] )>]
    [<TestCase( [|""        |], [|""    ; "cd"        |], [|0; 1; 2|] )>]
    [<TestCase( [|"ab"      |], [|"ab"  ; "cd"        |], [|2; 1; 2|] )>]
    member _.PasteBefore_NewLineAndOneLine start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister [""; "cd"]

        commandAssertLinesSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( [|""  ; "xy"|], [|"cd"  ; ""  ; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"cd"  ; "ab"; "xy"|], [|0; 1; 0|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"acd" ; "b" ; "xy"|], [|1; 1; 0|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"abcd"; ""  ; "xy"|], [|2; 1; 0|] )>]
    [<TestCase( [|""        |], [|"cd"  ; ""        |], [|0; 1; 0|] )>]
    [<TestCase( [|"ab"      |], [|"abcd"; ""        |], [|2; 1; 0|] )>]
    member _.PasteBefore_OneLineAndNewLine start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister ["cd"; ""]

        commandAssertLinesSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( [|""  ; "xy"|], [|""  ; "cd"; "ef"  ; "xy"|], [|0; 2; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|""  ; "cd"; "efab"; "xy"|], [|0; 2; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"a" ; "cd"; "efb" ; "xy"|], [|1; 2; 2|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"ab"; "cd"; "ef"  ; "xy"|], [|2; 2; 2|] )>]
    [<TestCase( [|""        |], [|""  ; "cd"; "ef"        |], [|0; 2; 2|] )>]
    [<TestCase( [|"ab"      |], [|"ab"; "cd"; "ef"        |], [|2; 2; 2|] )>]
    member _.PasteBefore_NewLineAndTwoLines start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister [""; "cd"; "ef"]

        commandAssertLinesSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( [|""  ; "xy"|], [|"cd"  ; "ef"; ""  ; "xy"|], [|0; 2; 0|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"cd"  ; "ef"; "ab"; "xy"|], [|0; 2; 0|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"acd" ; "ef"; "b" ; "xy"|], [|1; 2; 0|] )>]
    [<TestCase( [|"ab"; "xy"|], [|"abcd"; "ef"; ""  ; "xy"|], [|2; 2; 0|] )>]
    [<TestCase( [|""        |], [|"cd"  ; "ef"; ""        |], [|0; 2; 0|] )>]
    [<TestCase( [|"ab"      |], [|"abcd"; "ef"; ""        |], [|2; 2; 0|] )>]
    member _.PasteBefore_TwoLinesAndNewLine start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister ["cd"; "ef"; ""]

        commandAssertLinesSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar
