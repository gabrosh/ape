module MultiSelectModifyingAtEofTest

open NUnit.Framework
open System

open Commands.InCommands
open DataTypes
open Registers
open Selection
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef 80 25

let defReg = DefaultRegister

[<TestFixture>]
type ModifyingAtEofTest () =
    let myRegisters = Registers ()
    let myBuffer    = new TextAreaBuffer (contextRef, UserMessages (), myRegisters, "")

    // initialization

    let init line (char1, char2) =
        myBuffer.LoadStrings [line]

        myBuffer.Selections.Clear ()
        myBuffer.Selections.Add {
            Selection_Default with
                first = { line = 0; char = char1 }
                last  = { line = 0; char = char1 }
                isForward = true
        }
        myBuffer.Selections.Add {
            Selection_Default with
                first = { line = 0; char = char2 }
                last  = { line = 0; char = char2 }
                isForward = true
        }

    let toCharsAndRanges (a: 'T array) =
        (
            (a[0], a[1]),
            (
                ((a[2], a[3]), (a[4], a[5])),
                ((a[6], a[7]), (a[8], a[9]))
            )
        )

    // command and simple assertions

    let performCommand (command: obj) isNormalMode count =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand isNormalMode false (ModifyingCommand x) count
        | _                        ->
            invalidOp ""

    let setRegister (slots: string list list) =
        myRegisters.CreateOrClear DefaultRegister

        slots |> Seq.indexed |> Seq.iter (
            fun (i, slot) ->
                let lines = Lines ( slot |> Seq.map stringToChars )
                myRegisters.ApplyToSlot DefaultRegister i lines
        )

    let assertLine expLine =
        Assert.AreEqual ([expLine], myBuffer.Lines)

    let assertLines expLines =
        Assert.AreEqual (expLines, myBuffer.Lines)

    let assertSelection
        index (anchorLine, anchorChar) (cursorLine, cursorChar) =

        let selection = myBuffer.Selections[index]

        Assert.AreEqual (
            ( anchorLine            , anchorChar            ),
            ( selection.Anchor.line , selection.Anchor.char )
        )
        Assert.AreEqual (
            ( cursorLine            , cursorChar            ),
            ( selection.Cursor.line , selection.Cursor.char )
        )

    // commands followed by assertion(s)

    let commandAssertLinesSelections command isNormalMode lines
        ((anchor1, cursor1), (anchor2, cursor2)) =

        performCommand command isNormalMode 1
        assertLines lines
        assertSelection 0 anchor1 cursor1
        assertSelection 1 anchor2 cursor2

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // PasteBefore - Normal ----------------------------------------------------

    [<TestCase( "a", [|"ax"; "y"    |], [|1; 1; 0; 1; 0; 2; 1; 0; 1; 1|] )>]
    member _.PasteBefore_Normal_TwoSlots start end_ numbers =
        let startChars, endRanges = toCharsAndRanges numbers

        init start startChars

        setRegister [["x"; ""]; ["y"; ""]]

        commandAssertLinesSelections (PasteBefore defReg)
            true end_ endRanges

    [<TestCase( "a", [|"ax"; ""     |], [|1; 1; 0; 1; 0; 2; 1; 0; 1; 0|] )>]
    member _.PasteBefore_Normal_OneSlot start end_ numbers =
        let startChars, endRanges = toCharsAndRanges numbers

        init start startChars

        setRegister [["x"; ""]]

        commandAssertLinesSelections (PasteBefore defReg)
            true end_ endRanges

    // PasteBefore - Insert ----------------------------------------------------

    [<TestCase( "a", [|"ax"; "y"; ""|], [|1; 1; 1; 0; 1; 0; 2; 0; 2; 0|] )>]
    member _.PasteBefore_Insert_TwoSlots start end_ numbers =
        let startChars, endRanges = toCharsAndRanges numbers

        init start startChars

        setRegister [["x"; ""]; ["y"; ""]]

        commandAssertLinesSelections (PasteBefore defReg)
            false end_ endRanges

    [<TestCase( "a", [|"ax"; ""     |], [|1; 1; 1; 0; 1; 0; 1; 0; 1; 0|] )>]
    member _.PasteBefore_Insert_OneSlot start end_ numbers =
        let startChars, endRanges = toCharsAndRanges numbers

        init start startChars

        setRegister [["x"; ""]]

        commandAssertLinesSelections (PasteBefore defReg)
            false end_ endRanges

    // PasteAfter - Normal -----------------------------------------------------

    [<TestCase( "a", [|"a"; "x"; "y"|], [|1; 1; 1; 0; 1; 1; 2; 0; 2; 1|] )>]
    member _.PasteAfter_Normal_TwoSlots start end_ numbers =
        let startChars, endRanges = toCharsAndRanges numbers

        init start startChars

        setRegister [["x"; ""]; ["y"; ""]]

        commandAssertLinesSelections (PasteAfter defReg)
            true end_ endRanges

    [<TestCase( "a", [|"a"; "x"     |], [|1; 1; 1; 0; 1; 1; 1; 1; 1; 1|] )>]
    member _.PasteAfter_Normal_OneSlot start end_ numbers =
        let startChars, endRanges = toCharsAndRanges numbers

        init start startChars

        setRegister [["x"; ""]]

        commandAssertLinesSelections (PasteAfter defReg)
            true end_ endRanges
