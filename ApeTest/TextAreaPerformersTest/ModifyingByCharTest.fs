module TextAreaModifyingTest

open NUnit.Framework
open System

open Commands.InCommands
open Registers
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef 80 25

[<TestFixture>]
type ModifyingByCharTest () =
    let myBuffer = new TextAreaBuffer (contextRef, UserMessages (), Registers (), "")

    // initialization

    let init lines =
        myBuffer.LoadStrings lines

    let toTuple2 (a: 'T array) =
        a[0], a[1]

    // simple command, repeated command and simple assertions

    let performCommand (command: obj) count =
        match command with
        | :? CommonCommand       as x ->
            myBuffer.PerformCommand false false (CommonCommand x)       count
        | :? WrapLinesDepCommand as x ->
            myBuffer.PerformCommand false false (WrapLinesDepCommand x) count
        | :? ModifyingCommand    as x ->
            myBuffer.PerformCommand false false (ModifyingCommand x)    count
        | _                           ->
            invalidOp ""

    let command command =
        performCommand command 1

    let commandCount command count =
        performCommand command count

    let assertLines expLines =
        Assert.AreEqual (expLines, myBuffer.Lines)

    let assertCursor cursorLine cursorChar =
        Assert.AreEqual (
            ( cursorLine                , cursorChar                ),
            ( myBuffer.Main.Cursor.line , myBuffer.Main.Cursor.char )
        )

    // commands followed by assertion(s)

    let commandAssertLines command lines =
        performCommand command 1
        assertLines lines

    let commandAssertCursor command cursorLine cursorChar =
        performCommand command 1
        assertCursor cursorLine cursorChar

    let commandAssertLinesCursor command lines cursorLine cursorChar =
        performCommand command 1
        assertLines lines
        assertCursor cursorLine cursorChar

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // InsertChar, InsertNewLine, InsertNewLineIndent --------------------------

    [<TestCase(""  , 0, "c"  , 1)>]
    [<TestCase("ab", 0, "cab", 1)>]
    [<TestCase("ab", 1, "acb", 2)>]
    [<TestCase("ab", 2, "abc", 3)>]
    member _.InsertChar start startChar end_ endChar =
        init [start]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertLinesCursor (InsertChar 'c')  [end_] 0 endChar

    [<TestCase( [|""    ; "cd"|], [|""    ; ""    ; "cd"|], [|0; 0|] )>]
    [<TestCase( [|"ab"  ; "cd"|], [|""    ; "ab"  ; "cd"|], [|0; 0|] )>]
    [<TestCase( [|"ab"  ; "cd"|], [|"a"   ; "b"   ; "cd"|], [|1; 0|] )>]
    [<TestCase( [|"ab"  ; "cd"|], [|"ab"  ; ""    ; "cd"|], [|2; 0|] )>]

    [<TestCase( [|"  "  ; "cd"|], [|""    ; "  "  ; "cd"|], [|0; 0|] )>]
    [<TestCase( [|"  "  ; "cd"|], [|" "   ; " "   ; "cd"|], [|1; 0|] )>]
    [<TestCase( [|"  "  ; "cd"|], [|"  "  ; ""    ; "cd"|], [|2; 0|] )>]
    [<TestCase( [|"  ab"; "cd"|], [|""    ; "  ab"; "cd"|], [|0; 0|] )>]
    [<TestCase( [|"  ab"; "cd"|], [|" "   ; " ab" ; "cd"|], [|1; 0|] )>]
    [<TestCase( [|"  ab"; "cd"|], [|"  "  ; "ab"  ; "cd"|], [|2; 0|] )>]
    [<TestCase( [|"  ab"; "cd"|], [|"  a" ; "b"   ; "cd"|], [|3; 0|] )>]
    [<TestCase( [|"  ab"; "cd"|], [|"  ab"; ""    ; "cd"|], [|4; 0|] )>]
    member _.InsertNewLine start end_ chars =
        let startChar, endChar = toTuple2 chars

        init start
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertLinesCursor InsertNewLine end_ 1 endChar

    [<TestCase( [|""    ; "cd"|], [|""    ; ""    ; "cd"|], [|0; 0|] )>]
    [<TestCase( [|"ab"  ; "cd"|], [|""    ; "ab"  ; "cd"|], [|0; 0|] )>]
    [<TestCase( [|"ab"  ; "cd"|], [|"a"   ; "b"   ; "cd"|], [|1; 0|] )>]
    [<TestCase( [|"ab"  ; "cd"|], [|"ab"  ; ""    ; "cd"|], [|2; 0|] )>]

    [<TestCase( [|"  "  ; "cd"|], [|""    ; "  "  ; "cd"|], [|0; 0|] )>]
    [<TestCase( [|"  "  ; "cd"|], [|" "   ; "  "  ; "cd"|], [|1; 1|] )>]
    [<TestCase( [|"  "  ; "cd"|], [|"  "  ; "  "  ; "cd"|], [|2; 2|] )>]
    [<TestCase( [|"  ab"; "cd"|], [|""    ; "  ab"; "cd"|], [|0; 0|] )>]
    [<TestCase( [|"  ab"; "cd"|], [|" "   ; "  ab"; "cd"|], [|1; 1|] )>]
    [<TestCase( [|"  ab"; "cd"|], [|"  "  ; "  ab"; "cd"|], [|2; 2|] )>]
    [<TestCase( [|"  ab"; "cd"|], [|"  a" ; "  b" ; "cd"|], [|3; 2|] )>]
    [<TestCase( [|"  ab"; "cd"|], [|"  ab"; "  "  ; "cd"|], [|4; 2|] )>]
    member _.InsertNewLineIndent start end_ chars =
        let startChar, endChar = toTuple2 chars

        init start
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertLinesCursor InsertNewLineIndent end_ 1 endChar

    // DeleteChar, DeletePrevChar ----------------------------------------------

    [<TestCase("ab", 0, "b" , 0)>]
    [<TestCase("ab", 1, "a" , 1)>]
    [<TestCase("ab", 2, "ab", 2)>]
    member _.DeleteChar_OneLine start startChar end_ endChar =
        init [start]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertLinesCursor DeleteChar [end_] 0 endChar

    [<TestCase( [|"a"; "b"; "cd"|], [|"ab"; "cd"|], [|1; 1|] )>]
    member _.DeleteChar_MultipleLines start end_ chars =
        let startChar, endChar = toTuple2 chars

        init start
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertLinesCursor DeleteChar end_ 0 endChar

    [<TestCase("ab", 0, "ab", 0)>]
    [<TestCase("ab", 1, "b" , 0)>]
    [<TestCase("ab", 2, "a" , 1)>]
    member _.DeletePrevChar_OneLine start startChar end_ endChar =
        init [start]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertLinesCursor DeletePrevChar [end_] 0 endChar

    [<TestCase( [|"a"; "b"; "cd"|], [|"ab"; "cd"|], [|0; 1|] )>]
    member _.DeletePrevChar_MultipleLines start end_ chars =
        let startChar, endChar = toTuple2 chars

        init start
        command      CursorHardDown
        commandCount CursorRight startChar
        assertCursor 1 startChar

        commandAssertLinesCursor DeletePrevChar end_ 0 endChar
