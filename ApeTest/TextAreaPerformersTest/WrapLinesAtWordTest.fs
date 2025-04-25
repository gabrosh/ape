module TextAreaWrapLinesAtWordTest

open NUnit.Framework
open System

open Commands.InCommands
open Registers
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef_wrapLines 8 25 true

[<TestFixture>]
type WrapLinesAtWordTest () =
    let myBuffer = new TextAreaBuffer (contextRef, UserMessages (), Registers (), "")

    // initialization

    let init cursorBeforeEol lines =
        myBuffer.LoadStrings lines

        TestUtils.set_cursorBeforeEol contextRef cursorBeforeEol

    // simple command, repeated command and simple assertions

    let performCommand (command: obj) count =
        match command with
        | :? CommonCommand       as x ->
            myBuffer.PerformCommand false false (CommonCommand x)       count
        | :? WrapLinesDepCommand as x ->
            myBuffer.PerformCommand false false (WrapLinesDepCommand x) count
        | _                           ->
            invalidOp ""

    let command command =
        performCommand command 1

    let commandCount command count =
        performCommand command count

    let assertCursor cursorLine cursorChar =
        Assert.AreEqual (
            ( cursorLine                , cursorChar                ),
            ( myBuffer.Main.Cursor.line , myBuffer.Main.Cursor.char )
        )

    // commands followed by assertion(s)

    let commandAssertCursor command cursorLine cursorChar =
        performCommand command 1
        assertCursor cursorLine cursorChar

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // CursorSoftUp/Down -------------------------------------------------------

    [<TestCase(false, 18, 12, 4)>]
    [<TestCase(false, 19, 13, 5)>]
    [<TestCase(false, 20, 13, 6)>]
    [<TestCase(false, 21, 13, 7)>]
    [<TestCase(true , 18, 12, 4)>]
    [<TestCase(true , 19, 13, 5)>]
    [<TestCase(true , 20, 13, 6)>]
    [<TestCase(true , 21, 13, 7)>]
    member _.CursorSoftUp_WithoutTab cbe startChar middleChar endChar =
        init cbe [
            "0123456~"
          + "89012~"
          + "4567890"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftUp 0 middleChar
        commandAssertCursor CursorSoftUp 0 endChar
        commandAssertCursor CursorSoftUp 0 endChar

    [<TestCase(false, 8  , 0)>]
    [<TestCase(false, 9  , 1)>]
    [<TestCase(false, 10 , 2)>]
    [<TestCase(false, 11 , 4)>]
    [<TestCase(true , 8  , 0)>]
    [<TestCase(true , 9  , 1)>]
    [<TestCase(true , 10 , 2)>]
    [<TestCase(true , 11 , 4)>]
    member _.CursorSoftUp_FromTab cbe startChar endChar =
        init cbe [
            "0123456~"
          + "89\t1"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftUp 0 endChar
        commandAssertCursor CursorSoftUp 0 endChar

    [<TestCase(false, 7  , 0)>]
    [<TestCase(false, 8  , 1)>]
    [<TestCase(false, 9  , 2)>]
    [<TestCase(false, 10 , 2)>]
    [<TestCase(false, 11 , 3)>]
    [<TestCase(true , 7  , 0)>]
    [<TestCase(true , 8  , 1)>]
    [<TestCase(true , 9  , 2)>]
    [<TestCase(true , 10 , 2)>]
    [<TestCase(true , 11 , 3)>]
    member _.CursorSoftUp_ToTab cbe startChar endChar =
        init cbe [
            "01\t345~"
          + "78901"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftUp 0 endChar
        commandAssertCursor CursorSoftUp 0 endChar

    [<TestCase(false, 4, 12, 18)>]
    [<TestCase(false, 5, 13, 19)>]
    [<TestCase(false, 6, 13, 20)>]
    [<TestCase(false, 7, 13, 21)>]
    [<TestCase(true , 4, 12, 18)>]
    [<TestCase(true , 5, 13, 19)>]
    [<TestCase(true , 6, 13, 20)>]
    [<TestCase(true , 7, 13, 20)>]
    member _.CursorSoftDown_WithoutTab cbe startChar middleChar endChar =
        init cbe [
            "0123456~"
          + "89012~"
          + "4567890"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftDown 0 middleChar
        commandAssertCursor CursorSoftDown 0 endChar
        commandAssertCursor CursorSoftDown 0 endChar

    // CursorSoftLineStart/End -------------------------------------------------

    [<TestCase(false, 17 , 15)>]
    [<TestCase(false, 16 , 15)>]
    [<TestCase(false, 14 , 7 )>]
    [<TestCase(false, 6  , 0 )>]
    [<TestCase(true , 17 , 15)>]
    [<TestCase(true , 16 , 15)>]
    [<TestCase(true , 14 , 7 )>]
    [<TestCase(true , 6  , 0 )>]
    member _.CursorSoftLineStart_AtFirstLine cbe startChar endChar =
        init cbe [
            "012345~7890123~56"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftLineStart 0 endChar
        commandAssertCursor CursorSoftLineStart 0 endChar

    [<TestCase(false, 17 , 15)>]
    [<TestCase(false, 16 , 15)>]
    [<TestCase(false, 14 , 7 )>]
    [<TestCase(false, 6  , 0 )>]
    [<TestCase(true , 17 , 15)>]
    [<TestCase(true , 16 , 15)>]
    [<TestCase(true , 14 , 7 )>]
    [<TestCase(true , 6  , 0 )>]
    member _.CursorSoftLineStart_AtSecondLine cbe startChar endChar =
        init cbe [
            ""
            "012345~7890123~56"
            ""
        ]
        command      CursorHardDown
        commandCount CursorRight startChar
        assertCursor 1 startChar

        commandAssertCursor CursorSoftLineStart 1 endChar
        commandAssertCursor CursorSoftLineStart 1 endChar

    [<TestCase(false, 0  , 6 )>]
    [<TestCase(false, 7  , 14)>]
    [<TestCase(false, 15 , 16)>]
    [<TestCase(true , 0  , 6 )>]
    [<TestCase(true , 7  , 14)>]
    [<TestCase(true , 15 , 16)>]
    member _.CursorSoftLineEnd_AtLastLine cbe startChar endChar =
        init cbe [
            "012345~7890123~56"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftLineEnd 0 endChar
        commandAssertCursor CursorSoftLineEnd 0 endChar

    [<TestCase(false, 0  , 6 )>]
    [<TestCase(false, 7  , 14)>]
    [<TestCase(false, 15 , 16)>]
    [<TestCase(true , 0  , 6 )>]
    [<TestCase(true , 7  , 14)>]
    [<TestCase(true , 15 , 16)>]
    member _.CursorSoftLineEnd_AtSecondLine cbe startChar endChar =
        init cbe [
            ""
            "012345~7890123~56"
            ""
        ]
        command      CursorHardDown
        commandCount CursorRight startChar
        assertCursor 1 startChar

        commandAssertCursor CursorSoftLineEnd 1 endChar
        commandAssertCursor CursorSoftLineEnd 1 endChar

    // CursorSoftFileStart/End -------------------------------------------------

    // checking also that wanted column is applied after moving the cursor

    [<TestCase(false, 18, 4)>]
    [<TestCase(false, 19, 5)>]
    [<TestCase(false, 20, 6)>]
    [<TestCase(false, 21, 7)>]
    [<TestCase(true , 18, 4)>]
    [<TestCase(true , 19, 5)>]
    [<TestCase(true , 20, 6)>]
    [<TestCase(true , 21, 7)>]
    member _.CursorSoftFileStart_WithoutTab cbe startChar endChar =
        init cbe [
            "0123456~"
          + "89012~"
          + "4567890"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftFileStart 0 endChar
        commandAssertCursor CursorSoftFileStart 0 endChar

    [<TestCase(false, 4, 18)>]
    [<TestCase(false, 5, 19)>]
    [<TestCase(false, 6, 20)>]
    [<TestCase(false, 7, 21)>]
    [<TestCase(true , 4, 18)>]
    [<TestCase(true , 5, 19)>]
    [<TestCase(true , 6, 20)>]
    [<TestCase(true , 7, 20)>]
    member _.CursorSoftFileEnd_WithoutTab cbe startChar endChar =
        init cbe [
            "0123456~"
          + "89012~"
          + "4567890"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftFileEnd 0 endChar
        commandAssertCursor CursorSoftFileEnd 0 endChar
