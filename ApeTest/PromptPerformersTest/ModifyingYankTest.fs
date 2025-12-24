module PromptModifyingYankTest

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
type ModifyingYankTest () =
    let myRegisters = Registers ()
    let myBuffer    = new PromptBuffer (contextRef, extraContextRef, UserMessages (), myRegisters)

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

    let toTuple2 (a: 'T array) =
        a[0], a[1]

    // command and simple assertions

    let performCommand (command: obj) =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand false false (ModifyingCommand x)
        | _                        ->
            invalidOp ""

    let getRegister () =
        myRegisters.GetSlot DefaultRegister 0
        |> Option.get
        |> Seq.map charsToString
        |> Seq.toList

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

    let assertRegister expRegister =
        Assert.AreEqual (expRegister, getRegister ())

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

    // Yank --------------------------------------------------------------------

    [<TestCase( "cd"  , [|0; 1|] )>]  // empty line
    [<TestCase( "cdab", [|0; 1|] )>]  // first char of line
    [<TestCase( "acdb", [|1; 2|] )>]  // last char of line
    [<TestCase( "abcd", [|2; 3|] )>]  // EOL
    member _.Yank_OneLine start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLineSelection (Yank defReg)
            start 0 anchorChar 0 cursorChar

        assertRegister ["cd"]

    [<TestCase( ""  , [|0; 0|] )>]
    //[<TestCase( ""  , [|0; 0|] )>]  // duplicate
    [<TestCase( "a" , [|1; 1|] )>]
    [<TestCase( "ab", [|2; 2|] )>]
    member _.Yank_NewLine start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLineSelection (Yank defReg)
            start 0 anchorChar 0 cursorChar

        assertRegister [""; ""]

    [<TestCase( "cd"  , [|0; 2|] )>]
    //[<TestCase( "cd"  , [|0; 2|] )>]  // duplicate
    [<TestCase( "acd" , [|1; 3|] )>]
    [<TestCase( "abcd", [|2; 4|] )>]
    member _.Yank_OneLineAndNewLine start numbers =
        let anchorChar, cursorChar = toTuple2 numbers

        init start 0 anchorChar 0 cursorChar

        commandAssertLineSelection (Yank defReg)
            start 0 anchorChar 0 cursorChar

        assertRegister ["cd"; ""]
