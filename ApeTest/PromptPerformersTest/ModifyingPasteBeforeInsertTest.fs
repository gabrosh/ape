module PromptModifyingPasteBeforeInsertTest

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
type ModifyingPasteBeforeInsertTest () =
    let myRegisters = Registers ()
    let myBuffer    = new PromptBuffer (contextRef, extraContextRef, UserMessages (), myRegisters)

    // initialization

    let init line startLine startChar =
        myBuffer.LoadString line

        myBuffer.Selections.Clear ()
        myBuffer.Selections.Add {
            Selection_Default with
                first = { line = startLine; char = startChar }
                last  = { line = startLine; char = startChar }
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

    // PasteBefore -------------------------------------------------------------

    [<TestCase( ""  , "cd"  , [|0; 0; 2|] )>]  // empty line
    [<TestCase( "ab", "cdab", [|0; 0; 2|] )>]  // first char of line
    [<TestCase( "ab", "acdb", [|1; 0; 3|] )>]  // last char of line
    [<TestCase( "ab", "abcd", [|2; 0; 4|] )>]  // EOL
    member _.PasteBefore_OneLine start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister ["cd"]

        commandAssertLineSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( ""  , ""  , [|0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|1; 0; 1|] )>]
    [<TestCase( "ab", "ab", [|2; 0; 2|] )>]
    member _.PasteBefore_TwoLines start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister ["cd"; "ef"]

        commandAssertLineSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( ""  , ""  , [|0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|1; 0; 1|] )>]
    [<TestCase( "ab", "ab", [|2; 0; 2|] )>]
    member _.PasteBefore_NewLine start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister [""; ""]

        commandAssertLineSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( ""  , ""  , [|0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|0; 0; 0|] )>]
    [<TestCase( "ab", "ab", [|1; 0; 1|] )>]
    [<TestCase( "ab", "ab", [|2; 0; 2|] )>]
    member _.PasteBefore_NewLineAndOneLine start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister [""; "cd"]

        commandAssertLineSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar

    [<TestCase( ""  , "cd"  , [|0; 0; 2|] )>]
    [<TestCase( "ab", "cdab", [|0; 0; 2|] )>]
    [<TestCase( "ab", "acdb", [|1; 0; 3|] )>]
    [<TestCase( "ab", "abcd", [|2; 0; 4|] )>]
    member _.PasteBefore_OneLineAndNewLine start end_ numbers =
        let startChar, endLine, endChar = toTuple3 numbers

        init start 0 startChar

        setRegister ["cd"; ""]

        commandAssertLineSelection (PasteBefore defReg)
            end_ endLine endChar endLine endChar
