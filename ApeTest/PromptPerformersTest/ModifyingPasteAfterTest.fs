module PromptModifyingPasteAfterTest

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
type ModifyingPasteAfterTest () =
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

    let toTuple4 (a: 'T array) =
        a[0], a[1], a[2], a[3]

    // command and simple assertions

    let performCommand (command: obj) =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand false false (ModifyingCommand x)
        | _                        ->
            invalidOp ""

    let setRegister lines =
        let lines = Lines (lines |> Seq.map stringToChars)
        myRegisters.ApplyToSlot DefaultRegister 0 lines

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

    // PasteAfter --------------------------------------------------------------

    [<TestCase( ""  , "cd"  , [|0; 0; 0; 1|] )>]  // empty line
    [<TestCase( "ab", "acdb", [|0; 0; 1; 2|] )>]  // first char of line
    [<TestCase( "ab", "abcd", [|1; 1; 2; 3|] )>]  // last char of line
    [<TestCase( "ab", "abcd", [|2; 2; 2; 3|] )>]  // EOL
    [<TestCase( "ab", "abcd", [|1; 2; 2; 3|] )>]  // last char of line and EOL
    member _.PasteAfter_OneLine start end_ numbers =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 numbers

        init start 0 startAnchorChar 0 startCursorChar

        setRegister ["cd"]

        commandAssertLineSelection (PasteAfter defReg)
            end_ 0 endAnchorChar 0 endCursorChar

    [<TestCase( ""  , ""  , [|0; 0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|0; 0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|1; 1; 1; 1|] )>]
    [<TestCase( "ab", "ab", [|2; 2; 2; 2|] )>]
    [<TestCase( "ab", "ab", [|1; 2; 1; 2|] )>]
    member _.PasteAfter_TwoLines start end_ numbers =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 numbers

        init start 0 startAnchorChar 0 startCursorChar

        setRegister ["cd"; "ef"]

        commandAssertLineSelection (PasteAfter defReg)
            end_ 0 endAnchorChar 0 endCursorChar

    [<TestCase( ""  , ""  , [|0; 0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|0; 0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|1; 1; 1; 1|] )>]
    [<TestCase( "ab", "ab", [|2; 2; 2; 2|] )>]
    [<TestCase( "ab", "ab", [|1; 2; 1; 2|] )>]
    member _.PasteAfter_NewLine start end_ numbers =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 numbers

        init start 0 startAnchorChar 0 startCursorChar

        setRegister [""; ""]

        commandAssertLineSelection (PasteAfter defReg)
            end_ 0 endAnchorChar 0 endCursorChar

    [<TestCase( ""  , ""  , [|0; 0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|0; 0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|1; 1; 1; 1|] )>]
    [<TestCase( "ab", "ab", [|2; 2; 2; 2|] )>]
    [<TestCase( "ab", "ab", [|1; 2; 1; 2|] )>]
    member _.PasteAfter_NewLineOneLine start end_ numbers =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 numbers

        init start 0 startAnchorChar 0 startCursorChar

        setRegister [""; "cd"]

        commandAssertLineSelection (PasteAfter defReg)
            end_ 0 endAnchorChar 0 endCursorChar

    [<TestCase( ""  , "cd"  , [|0; 0; 0; 1|] )>]
    [<TestCase( "ab", "acdb", [|0; 0; 1; 2|] )>]
    [<TestCase( "ab", "abcd", [|1; 1; 2; 3|] )>]
    [<TestCase( "ab", "abcd", [|2; 2; 2; 3|] )>]
    [<TestCase( "ab", "abcd", [|1; 2; 2; 3|] )>]
    member _.PasteAfter_OneLineAndNewLine start end_ numbers =
        let startAnchorChar, startCursorChar, endAnchorChar, endCursorChar =
            toTuple4 numbers

        init start 0 startAnchorChar 0 startCursorChar

        setRegister ["cd"; ""]

        commandAssertLineSelection (PasteAfter defReg)
            end_ 0 endAnchorChar 0 endCursorChar
