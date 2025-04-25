module PromptModifyinByCharTest

open NUnit.Framework
open System

open Commands.InCommands
open PromptBuffer
open Registers
open UserMessages

let contextRef      = TestUtils.makeContextRef 80 25
let extraContextRef = TestUtils.makeExtraContextRef 1

[<TestFixture>]
type ModifyinByCharTest () =
    let myBuffer = new PromptBuffer (contextRef, extraContextRef, UserMessages (), Registers ())

    // initialization

    let init line =
        myBuffer.LoadString line

    // simple command, repeated command and simple assertions

    let performCommand (command: obj) =
        match command with
        | :? CommonCommand       as x ->
            myBuffer.PerformCommand false false (CommonCommand x)
        | :? WrapLinesDepCommand as x ->
            myBuffer.PerformCommand false false (WrapLinesDepCommand x)
        | :? ModifyingCommand    as x ->
            myBuffer.PerformCommand false false (ModifyingCommand x)
        | _                           ->
            invalidOp ""

    let commandCount command count =
        for _ = 1 to count do
            performCommand command

    let assertCursor expCursorChar =
        Assert.AreEqual (expCursorChar, myBuffer.Main.Cursor.char)

    let assertLine expLine =
        Assert.AreEqual ([expLine], myBuffer.Lines)

    // commands followed by assertion(s)

    let commandAssertCursorLine command expCursorChar expLine =
        performCommand command
        assertCursor expCursorChar
        assertLine expLine

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // InsertChar --------------------------------------------------------------

    [<TestCase(""  , 0, "c"  , 1)>]
    [<TestCase("ab", 0, "cab", 1)>]
    [<TestCase("ab", 1, "acb", 2)>]
    [<TestCase("ab", 2, "abc", 3)>]
    member _.InsertChar start startChar end_ endChar =
        init start
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursorLine (InsertChar 'c') endChar end_

    // DeleteChar, DeletePrevChar ----------------------------------------------

    [<TestCase("ab" , 0, "b" , 0, "" , 0)>]
    [<TestCase("abc", 1, "ac", 1, "a", 1)>]
    member _.DeleteChar start startChar middle middleChar end_ endChar =
        init start
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursorLine DeleteChar middleChar middle
        commandAssertCursorLine DeleteChar endChar    end_
        commandAssertCursorLine DeleteChar endChar    end_

    [<TestCase("ab" , 2, "a" , 1, "" , 0)>]
    [<TestCase("abc", 2, "ac", 1, "c", 0)>]
    member _.DeletePrevChar start startChar middle middleChar end_ endChar =
        init start
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursorLine DeletePrevChar middleChar middle
        commandAssertCursorLine DeletePrevChar endChar    end_
        commandAssertCursorLine DeletePrevChar endChar    end_
