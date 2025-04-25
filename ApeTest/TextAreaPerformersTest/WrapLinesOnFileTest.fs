module TextAreaWrapLinesOnFileTest

open NUnit.Framework
open System

open Commands.InCommands
open Registers
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef_wrapLines 120 30 false

let filePath = @"Data\scrolling.txt"

[<TestFixture>]
type WrapLinesOnFileTest () =
    let myBuffer = new TextAreaBuffer (contextRef, UserMessages (), Registers (), filePath)

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

    // initialization

    let init cursorBeforeEol =
        command CursorSoftFileStart
        command CursorHardLineStart

        TestUtils.set_cursorBeforeEol contextRef cursorBeforeEol

    [<OneTimeSetUp>]
    member _.OneTimeSetup () =
        myBuffer.LoadFile FileUtils.defaultEncoding true false
            |> ignore

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // ScrollPageUp/Down -------------------------------------------------------

    [<TestCase(false, 0, 0)>]
    [<TestCase(false, 1, 1)>]
    [<TestCase(false, 2, 2)>]
    [<TestCase(true , 0, 0)>]
    [<TestCase(true , 1, 1)>]
    [<TestCase(true , 2, 1)>]
    member _.ScrollPage cbe startChar endChar =
        init cbe
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor ScrollPageDown 28  endChar
        commandAssertCursor ScrollPageDown 53  endChar
        commandAssertCursor ScrollPageDown 78  endChar
        commandAssertCursor ScrollPageDown 101 endChar
        commandAssertCursor ScrollPageUp   74  endChar
        commandAssertCursor ScrollPageUp   49  endChar
        commandAssertCursor ScrollPageUp   24  endChar
        commandAssertCursor ScrollPageUp   0   endChar
