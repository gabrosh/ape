module TextAreaNoWrapLinesTest

open NUnit.Framework
open System

open Commands.InCommands
open Registers
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef 8 25

[<TestFixture>]
type NoWrapLinesTest () =
    let myBuffer = makeTextAreaBuffer (contextRef, UserMessages (), Registers (), "")

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

    // CursorHardUp/Down -------------------------------------------------------

    [<TestCase(false, 8  , 8  )>]
    [<TestCase(false, 9  , 9  )>]
    [<TestCase(false, 10 , 10 )>]
    [<TestCase(false, 11 , 10 )>]
    [<TestCase(true , 8  , 8  )>]
    [<TestCase(true , 9  , 9  )>]
    [<TestCase(true , 10 , 9  )>]
    [<TestCase(true , 11 , 9  )>]
    member _.CursorHardUp_WithoutTab cbe startChar middleChar =
        init cbe [
            "0123456_8901"
            "0123456_89"
            "0123456_890"
        ]
        commandCount CursorHardDown 2
        commandCount CursorRight    startChar
        assertCursor 2 startChar

        commandAssertCursor CursorHardUp 1 middleChar
        commandAssertCursor CursorHardUp 0 startChar
        commandAssertCursor CursorHardUp 0 startChar

    [<TestCase(false, 8  , 8 )>]
    [<TestCase(false, 9  , 9 )>]
    [<TestCase(false, 10 , 10)>]
    [<TestCase(false, 11 , 12)>]
    [<TestCase(true , 8  , 8 )>]
    [<TestCase(true , 9  , 9 )>]
    [<TestCase(true , 10 , 10)>]
    [<TestCase(true , 11 , 12)>]
    member _.CursorHardUp_FromTab cbe startChar endChar =
        init cbe [
            "0123456_89012"
            "0123456_89\t1"
        ]
        commandCount CursorHardDown 1
        commandCount CursorRight    startChar
        assertCursor 1 startChar

        commandAssertCursor CursorHardUp 0 endChar
        commandAssertCursor CursorHardUp 0 endChar

    [<TestCase(false, 8  , 8 )>]
    [<TestCase(false, 9  , 9 )>]
    [<TestCase(false, 10 , 10)>]
    [<TestCase(false, 11 , 10)>]
    [<TestCase(false, 12 , 11)>]
    [<TestCase(true , 8  , 8 )>]
    [<TestCase(true , 9  , 9 )>]
    [<TestCase(true , 10 , 10)>]
    [<TestCase(true , 11 , 10)>]
    [<TestCase(true , 12 , 11)>]
    member _.CursorHardUp_ToTab cbe startChar endChar =
        init cbe [
            "0123456_89\t1"
            "0123456_89012"
        ]
        commandCount CursorHardDown 1
        commandCount CursorRight    startChar
        assertCursor 1 startChar

        commandAssertCursor CursorHardUp 0 endChar
        commandAssertCursor CursorHardUp 0 endChar

    [<TestCase(false, 8  , 8  , 8 )>]
    [<TestCase(false, 9  , 9  , 9 )>]
    [<TestCase(false, 10 , 10 , 10)>]
    [<TestCase(false, 11 , 10 , 11)>]
    [<TestCase(true , 8  , 8  , 8 )>]
    [<TestCase(true , 9  , 9  , 9 )>]
    [<TestCase(true , 10 , 9  , 10)>]
    [<TestCase(true , 11 , 9  , 10)>]
    member _.CursorHardDown_WithoutTab cbe startChar middleChar endChar =
        init cbe [
            "0123456_8901"
            "0123456_89"
            "0123456_890"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorHardDown 1 middleChar
        commandAssertCursor CursorHardDown 2 endChar
        commandAssertCursor CursorHardDown 2 endChar

    // CursorSoftUp/Down -------------------------------------------------------

    // These commands have the same implementation as CursorHardUp/Down.

    // CursorSoftLineStart/End -------------------------------------------------

    [<TestCase(true, 11, 0)>]
    [<TestCase(true, 12, 0)>]
    member _.CursorSoftLineStart_AtFirstLine cbe startChar endChar =
        init cbe [
            "0123456_8901"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftLineStart 0 endChar
        commandAssertCursor CursorSoftLineStart 0 endChar

    [<TestCase(true, 11, 0)>]
    [<TestCase(true, 12, 0)>]
    member _.CursorSoftLineStart_AtSecondLine cbe startChar endChar =
        init cbe [
            ""
            "0123456_8901"
            ""
        ]
        command      CursorHardDown
        commandCount CursorRight startChar
        assertCursor 1 startChar

        commandAssertCursor CursorSoftLineStart 1 endChar
        commandAssertCursor CursorSoftLineStart 1 endChar

    [<TestCase(true, 0, 11)>]
    member _.CursorSoftLineEnd_AtLastLine cbe startChar endChar =
        init cbe [
            "0123456_8901"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftLineEnd 0 endChar
        commandAssertCursor CursorSoftLineEnd 0 endChar

    [<TestCase(true, 0, 11)>]
    member _.CursorSoftLineEnd_AtSecondLine cbe startChar endChar =
        init cbe [
            ""
            "0123456_8901"
            ""
        ]
        command      CursorHardDown
        commandCount CursorRight startChar
        assertCursor 1 startChar

        commandAssertCursor CursorSoftLineEnd 1 endChar
        commandAssertCursor CursorSoftLineEnd 1 endChar

    // CursorSoftFileStart/End -------------------------------------------------

    // checking also that wanted column is applied after moving the cursor

    [<TestCase(false, 8 )>]
    [<TestCase(false, 9 )>]
    [<TestCase(false, 10)>]
    [<TestCase(false, 11)>]
    [<TestCase(true , 8 )>]
    [<TestCase(true , 9 )>]
    [<TestCase(true , 10)>]
    [<TestCase(true , 11)>]
    member _.CursorSoftFileStart_WithoutTab cbe startChar =
        init cbe [
            "0123456_8901"
            "0123456_89"
            "0123456_890"
        ]
        commandCount CursorHardDown 2
        commandCount CursorRight    startChar
        assertCursor 2 startChar

        commandAssertCursor CursorSoftFileStart 0 startChar
        commandAssertCursor CursorSoftFileStart 0 startChar

    [<TestCase(false, 8  , 8 )>]
    [<TestCase(false, 9  , 9 )>]
    [<TestCase(false, 10 , 10)>]
    [<TestCase(false, 11 , 11)>]
    [<TestCase(true , 8  , 8 )>]
    [<TestCase(true , 9  , 9 )>]
    [<TestCase(true , 10 , 10)>]
    [<TestCase(true , 11 , 10)>]
    member _.CursorSoftFileEnd_WithoutTab cbe startChar endChar =
        init cbe [
            "0123456_8901"
            "0123456_89"
            "0123456_890"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorSoftFileEnd 2 endChar
        commandAssertCursor CursorSoftFileEnd 2 endChar

    // CursorHardToLine --------------------------------------------------------

    // checking also that wanted column is applied after moving the cursor

    [<TestCase(true, 8  , 8  , 8 )>]
    [<TestCase(true, 9  , 9  , 9 )>]
    [<TestCase(true, 10 , 9  , 10)>]
    [<TestCase(true, 11 , 9  , 10)>]
    member _.CursorHardToLine_WithoutTab cbe startChar middleChar endChar =
        init cbe [
            "0123456_8901"
            "0123456_89"
            "0123456_890"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor (CursorHardToLine 1) 1 middleChar
        commandAssertCursor (CursorHardToLine 2) 2 endChar
