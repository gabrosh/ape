module PromptModifyingDeleteTest

open NUnit.Framework
open System

open Commands.InCommands
open DataTypes
open PromptBuffer
open Registers
open Selection
open UserMessages

let contextRef      = TestUtils.makeContextRef 80 25
let extraContextRef = TestUtils.makeExtraContextRef 1

let defReg = DefaultRegister

[<TestFixture>]
type ModifyingDeleteTest () =
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

    let toTuple3 (a: 'T array) =
        a[0], a[1], a[2]

    // command and simple assertions

    let performCommand (command: obj) =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand false false (ModifyingCommand x)
        | _                        ->
            invalidOp ""

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

    let commandAssertLineSelection
        command line anchorLine anchorChar cursorLine cursorChar =

        performCommand command
        assertLine line
        assertAnchor anchorLine anchorChar
        assertCursor cursorLine cursorChar

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // Delete ------------------------------------------------------------------

    [<TestCase( "cd"  , ""  , [|0; 1; 0|] )>]  // empty line
    [<TestCase( "cdab", "ab", [|0; 1; 0|] )>]  // first char of line
    [<TestCase( "acdb", "ab", [|1; 2; 1|] )>]  // last char of line
    [<TestCase( "abcd", "ab", [|2; 3; 2|] )>]  // EOL
    member _.Delete_OneLine start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLineSelection Delete
            end_ 0 endChar 0 endChar

    [<TestCase( ""  , ""  , [|0; 0; 0|] )>]
    //[<TestCase( ""  , ""  , [|0; 0; 0|] )>]  // duplicate
    [<TestCase( "a" , "a" , [|1; 1; 1|] )>]
    [<TestCase( "ab", "ab", [|2; 2; 2|] )>]
    member _.Delete_NewLine start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLineSelection Delete
            end_ 0 endChar 0 endChar

    [<TestCase( "cd"  , ""  , [|0; 2; 0|] )>]
    //[<TestCase( "cd"  , ""  , [|0; 2; 0|] )>]  // duplicate
    [<TestCase( "acd" , "a" , [|1; 3; 1|] )>]
    [<TestCase( "abcd", "ab", [|2; 4; 2|] )>]
    member _.Delete_OneLineAndNewLine start end_ numbers =
        let anchorChar, cursorChar, endChar = toTuple3 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLineSelection Delete
            end_ 0 endChar 0 endChar
