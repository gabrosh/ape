module PromptNoWrapLineTest

open NUnit.Framework
open System

open Commands.InCommands
open PromptBuffer
open Registers
open UserMessages

let contextRef      = TestUtils.makeContextRef 80 25
let extraContextRef = TestUtils.makeExtraContextRef 1

[<TestFixture>]
type NoWrapLineTest () =
    let myBuffer = new PromptBuffer (contextRef, extraContextRef, UserMessages (), Registers ())

    // initialization

    let init cursorBeforeEol lines =
        myBuffer.LoadString lines

        TestUtils.set_cursorBeforeEol contextRef cursorBeforeEol

    // simple command, repeated command and simple assertions

    let performCommand (command: obj) =
        match command with
        | :? CommonCommand       as x ->
            myBuffer.PerformCommand false false (CommonCommand x)
        | :? WrapLinesDepCommand as x ->
            myBuffer.PerformCommand false false (WrapLinesDepCommand x)
        | _                           ->
            invalidOp ""

    let assertCursor expCursorChar =
        Assert.AreEqual (expCursorChar, myBuffer.Main.Cursor.char)

    // commands followed by assertion(s)

    let commandCount command count =
        for _ = 1 to count do
            performCommand command

    let commandAssertCursor command expCursorChar =
        performCommand command
        assertCursor expCursorChar

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // CursorHardLineStart/End -------------------------------------------------

    [<TestCase(true , 11)>]
    [<TestCase(true , 12)>]
    member _.CursorSoftLineStart cbe startChar =
        init cbe "0123456_8901"
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor CursorSoftLineStart 0
        commandAssertCursor CursorSoftLineStart 0

    [<TestCase(true , 11)>]
    member _.CursorSoftLineEnd cbe endChar =
        init cbe "0123456_8901"
        assertCursor 0

        commandAssertCursor CursorSoftLineEnd endChar
        commandAssertCursor CursorSoftLineEnd endChar
