module MultiSelectModifyingTest

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
type ModifyingTest () =
    let myRegisters = Registers ()
    let myBuffer    = makeTextAreaBuffer (contextRef, UserMessages (), myRegisters, "")

    // initialization

    let init line (range1, range2) =
        myBuffer.LoadStrings [line]

        myBuffer.Selections.Clear ()
        myBuffer.Selections.Add {
            Selection_Default with
                first = { line = 0; char = fst range1 }
                last  = { line = 0; char = snd range1 }
                isForward = true
        }
        myBuffer.Selections.Add {
            Selection_Default with
                first = { line = 0; char = fst range2 }
                last  = { line = 0; char = snd range2 }
                isForward = true
        }

    let toRangesAndChars (a: 'T array) =
        (
            ((a[0], a[1]), (a[2], a[3])),
            (a[4], a[5])
        )

    let toRangesAndPositions (a: 'T array) =
        (
            ((a[0], a[1]), (a[2], a[3])),
            ((a[4], a[5]), (a[6], a[7]))
        )

    // command and simple assertions

    let performCommand (command: obj) isNormalMode count =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand isNormalMode false (ModifyingCommand x) count
        | _                        ->
            invalidOp ""

    let setRegister (slots: string list) =
        myRegisters.CreateOrClear DefaultRegister

        slots |> Seq.indexed |> Seq.iter (
            fun (i, slot) ->
                let lines = Lines [ slot |> stringToChars ]
                myRegisters.ApplyToSlot DefaultRegister i lines
        )

    let assertLine expLine =
        Assert.AreEqual ([expLine], myBuffer.Lines)

    let assertLines expLines =
        Assert.AreEqual (expLines, myBuffer.Lines)

    let assertSelection index line char =
        let selection = myBuffer.Selections[index]

        Assert.AreEqual (
            ( line                  , char                  ),
            ( selection.Anchor.line , selection.Anchor.char )
        )
        Assert.AreEqual (
            ( line                  , char                  ),
            ( selection.Cursor.line , selection.Cursor.char )
        )

    // commands followed by assertion(s)

    let commandAssertLineSelections command isNormalMode line
        (char1, char2) =

        performCommand command isNormalMode 1
        assertLine line
        assertSelection 0 0 char1
        assertSelection 1 0 char2

    let commandAssertLinesSelections command isNormalMode lines
        ((line1, char1), (line2, char2)) =

        performCommand command isNormalMode 1
        assertLines lines
        assertSelection 0 line1 char1
        assertSelection 1 line2 char2

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // InsertChar --------------------------------------------------------------

    [<TestCase( "abc", "xabxc", [|0; 0; 2; 2; 1; 4|] )>]  // separated
    [<TestCase( "abc", "xaxbc", [|0; 0; 1; 1; 1; 3|] )>]  // touching
    [<TestCase( "abc", "xxabc", [|0; 0; 0; 0; 1; 2|] )>]  // equal
    member _.InsertChar_BeforeEof start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        commandAssertLineSelections (InsertChar 'x')
            false end_ endChars

    [<TestCase( "ab" , "xabx" , [|0; 0; 2; 2; 1; 4|] )>]  // separated
    [<TestCase( "a"  , "xax"  , [|0; 0; 1; 1; 1; 3|] )>]  // touching
    [<TestCase( ""   , "xx"   , [|0; 0; 0; 0; 1; 2|] )>]  // equal
    member _.InsertChar_AtEof start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        commandAssertLineSelections (InsertChar 'x')
            false end_ endChars

    // InsertNewLine -----------------------------------------------------------

    [<TestCase( "abc", [|""; "ab"; "c"  |], [|0; 0; 2; 2; 1; 0; 2; 0|] )>]  // separated
    [<TestCase( "abc", [|""; "a" ; "bc" |], [|0; 0; 1; 1; 1; 0; 2; 0|] )>]  // touching
    [<TestCase( "abc", [|""; ""  ; "abc"|], [|0; 0; 0; 0; 1; 0; 2; 0|] )>]  // equal
    member _.InsertNewLine_BeforeEof start end_ numbers =
        let startRanges, endPositions = toRangesAndPositions numbers

        init start startRanges

        commandAssertLinesSelections InsertNewLine
            true end_ endPositions

    [<TestCase( "ab" , [|""; "ab"; ""   |], [|0; 0; 2; 2; 1; 0; 2; 0|] )>]  // separated
    [<TestCase( "a"  , [|""; "a" ; ""   |], [|0; 0; 1; 1; 1; 0; 2; 0|] )>]  // touching
    [<TestCase( ""   , [|""; ""  ; ""   |], [|0; 0; 0; 0; 1; 0; 2; 0|] )>]  // equal
    member _.InsertNewLine_AtEof start end_ numbers =
        let startRanges, endPositions = toRangesAndPositions numbers

        init start startRanges

        commandAssertLinesSelections InsertNewLine
            true end_ endPositions

    // InsertNewLineIndent -----------------------------------------------------

    [<TestCase( "  abc", [|"  "; "  ab"; "  c"  |], [|0; 2; 2; 4; 1; 2; 2; 2|] )>]  // separated
    [<TestCase( "  abc", [|"  "; "  a" ; "  bc" |], [|0; 2; 1; 3; 1; 2; 2; 2|] )>]  // touching
    [<TestCase( "  abc", [|"  "; "  "  ; "  abc"|], [|0; 2; 0; 2; 1; 2; 2; 2|] )>]  // equal
    member _.InsertNewLineIndent_BeforeEof start end_ numbers =
        let startRanges, endPositions = toRangesAndPositions numbers

        init start startRanges

        commandAssertLinesSelections InsertNewLineIndent
            true end_ endPositions

    [<TestCase( "  ab" , [|"  "; "  ab"; "  "   |], [|0; 2; 2; 4; 1; 2; 2; 2|] )>]  // separated
    [<TestCase( "  a"  , [|"  "; "  a" ; "  "   |], [|0; 2; 1; 3; 1; 2; 2; 2|] )>]  // touching
    [<TestCase( "  "   , [|"  "; "  "  ; "  "   |], [|0; 2; 0; 2; 1; 2; 2; 2|] )>]  // equal
    member _.InsertNewLineIndent_AtEof start end_ numbers =
        let startRanges, endPositions = toRangesAndPositions numbers

        init start startRanges

        commandAssertLinesSelections InsertNewLineIndent
            true end_ endPositions

    // DeleteChar --------------------------------------------------------------

    [<TestCase( "abc", "b", [|0; 0; 2; 2; 0; 1|] )>]  // separated
    [<TestCase( "abc", "c", [|0; 0; 1; 1; 0; 0|] )>]  // touching
    [<TestCase( "abc", "c", [|0; 0; 0; 0; 0; 0|] )>]  // equal
    member _.DeleteChar_BeforeEof start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        commandAssertLineSelections DeleteChar
            true end_ endChars

    [<TestCase( "ab" , "b", [|0; 0; 2; 2; 0; 1|] )>]  // separated
    [<TestCase( "a"  , "" , [|0; 0; 1; 1; 0; 0|] )>]  // touching
    [<TestCase( ""   , "" , [|0; 0; 0; 0; 0; 0|] )>]  // equal
    member _.DeleteChar_AtEof start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        commandAssertLineSelections DeleteChar
            true end_ endChars

    // DeletePrevChar ----------------------------------------------------------

    [<TestCase( "abcde", "ace", [|2; 2; 4; 4; 1; 2|] )>]  // separated
    [<TestCase( "abcde", "ade", [|2; 2; 3; 3; 1; 1|] )>]  // touching
    [<TestCase( "abcde", "cde", [|2; 2; 2; 2; 0; 0|] )>]  // equal
    member _.DeletePrevChar_AfterSof start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        commandAssertLineSelections DeletePrevChar
            true end_ endChars

    [<TestCase( "ab"   , "a"  , [|0; 0; 2; 2; 0; 1|] )>]  // separated
    [<TestCase( "a"    , ""   , [|0; 0; 1; 1; 0; 0|] )>]  // touching
    [<TestCase( ""     , ""   , [|0; 0; 0; 0; 0; 0|] )>]  // equal
    member _.DeletePrevChar_AtSof start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        commandAssertLineSelections DeletePrevChar
            true end_ endChars

    // Delete ------------------------------------------------------------------

    [<TestCase( "abcde", "c", [|0; 1; 3; 4; 0; 1|] )>]  // separated
    [<TestCase( "abcde", "e", [|0; 1; 2; 3; 0; 0|] )>]  // touching
    [<TestCase( "abcde", "e", [|0; 3; 0; 3; 0; 0|] )>]  // equal
    [<TestCase( "abcde", "e", [|0; 3; 0; 1; 0; 0|] )>]  // outer/inner left
    [<TestCase( "abcde", "e", [|0; 3; 1; 2; 0; 0|] )>]  // outer/inner center
    [<TestCase( "abcde", "e", [|0; 3; 2; 3; 0; 0|] )>]  // outer/inner right
    member _.Delete_BeforeEof start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        commandAssertLineSelections Delete
            true end_ endChars

    [<TestCase( "abcd" , "c", [|0; 1; 3; 4; 0; 1|] )>]  // separated
    [<TestCase( "abc"  , "" , [|0; 1; 2; 3; 0; 0|] )>]  // touching
    [<TestCase( "abc"  , "" , [|0; 3; 0; 3; 0; 0|] )>]  // equal
    [<TestCase( "abc"  , "" , [|0; 3; 0; 1; 0; 0|] )>]  // outer/inner left
    [<TestCase( "abc"  , "" , [|0; 3; 1; 2; 0; 0|] )>]  // outer/inner center
    [<TestCase( "abc"  , "" , [|0; 3; 2; 3; 0; 0|] )>]  // outer/inner right
    member _.Delete_AtEof start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        commandAssertLineSelections Delete
            true end_ endChars

    // PasteBefore - Normal ----------------------------------------------------

    [<TestCase( "abcde", "xabcyde", [|0; 1; 3; 4; 0; 4|] )>]  // separated
    [<TestCase( "abcde", "xabycde", [|0; 1; 2; 3; 0; 3|] )>]  // touching
    [<TestCase( "abcde", "xyabcde", [|0; 3; 0; 3; 0; 1|] )>]  // equal
    [<TestCase( "abcde", "xyabcde", [|0; 3; 0; 1; 0; 1|] )>]  // outer/inner left
    [<TestCase( "abcde", "xaybcde", [|0; 3; 1; 2; 0; 2|] )>]  // outer/inner center
    [<TestCase( "abcde", "xabycde", [|0; 3; 2; 3; 0; 3|] )>]  // outer/inner right
    member _.PasteBefore_Normal_BeforeEof_TwoSlots start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        setRegister ["x"; "y"]

        commandAssertLineSelections (PasteBefore defReg)
            true end_ endChars

    [<TestCase( "abcd" , "xabcyd" , [|0; 1; 3; 4; 0; 4|] )>]  // separated
    [<TestCase( "abc"  , "xabyc"  , [|0; 1; 2; 3; 0; 3|] )>]  // touching
    [<TestCase( "abc"  , "xyabc"  , [|0; 3; 0; 3; 0; 1|] )>]  // equal
    [<TestCase( "abc"  , "xyabc"  , [|0; 3; 0; 1; 0; 1|] )>]  // outer/inner left
    [<TestCase( "abc"  , "xaybc"  , [|0; 3; 1; 2; 0; 2|] )>]  // outer/inner center
    [<TestCase( "abc"  , "xabyc"  , [|0; 3; 2; 3; 0; 3|] )>]  // outer/inner right
    member _.PasteBefore_Normal_AtEof_TwoSlots start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        setRegister ["x"; "y"]

        commandAssertLineSelections (PasteBefore defReg)
            true end_ endChars

    [<TestCase( "abcde", "xabcde" , [|0; 1; 3; 4; 0; 4|] )>]  // separated
    [<TestCase( "abcde", "xabcde" , [|0; 1; 2; 3; 0; 3|] )>]  // touching
    [<TestCase( "abcde", "xabcde" , [|0; 3; 0; 3; 0; 1|] )>]  // equal
    [<TestCase( "abcde", "xabcde" , [|0; 3; 0; 1; 0; 1|] )>]  // outer/inner left
    [<TestCase( "abcde", "xabcde" , [|0; 3; 1; 2; 0; 2|] )>]  // outer/inner center
    [<TestCase( "abcde", "xabcde" , [|0; 3; 2; 3; 0; 3|] )>]  // outer/inner right
    member _.PasteBefore_Normal_BeforeEof_OneSlot start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        setRegister ["x"]

        commandAssertLineSelections (PasteBefore defReg)
            true end_ endChars

    [<TestCase( "abcd" , "xabcd"  , [|0; 1; 3; 4; 0; 4|] )>]  // separated
    [<TestCase( "abc"  , "xabc"   , [|0; 1; 2; 3; 0; 3|] )>]  // touching
    [<TestCase( "abc"  , "xabc"   , [|0; 3; 0; 3; 0; 1|] )>]  // equal
    [<TestCase( "abc"  , "xabc"   , [|0; 3; 0; 1; 0; 1|] )>]  // outer/inner left
    [<TestCase( "abc"  , "xabc"   , [|0; 3; 1; 2; 0; 2|] )>]  // outer/inner center
    [<TestCase( "abc"  , "xabc"   , [|0; 3; 2; 3; 0; 3|] )>]  // outer/inner right
    member _.PasteBefore_Normal_AtEof_OneSlot start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        setRegister ["x"]

        commandAssertLineSelections (PasteBefore defReg)
            true end_ endChars

    // PasteBefore - Insert ----------------------------------------------------

    [<TestCase( "abc", "xabyc", [|0; 0; 2; 2; 1; 4|] )>]  // separated
    [<TestCase( "abc", "xaybc", [|0; 0; 1; 1; 1; 3|] )>]  // touching
    [<TestCase( "abc", "xyabc", [|0; 0; 0; 0; 1; 2|] )>]  // equal
    member _.PasteBefore_Insert_BeforeEof_TwoSlots start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        setRegister ["x"; "y"]

        commandAssertLineSelections (PasteBefore defReg)
            false end_ endChars

    [<TestCase( "ab" , "xaby" , [|0; 0; 2; 2; 1; 4|] )>]  // separated
    [<TestCase( "a"  , "xay"  , [|0; 0; 1; 1; 1; 3|] )>]  // touching
    [<TestCase( ""   , "xy"   , [|0; 0; 0; 0; 1; 2|] )>]  // equal
    member _.PasteBefore_Insert_AtEof_TwoSlots start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        setRegister ["x"; "y"]

        commandAssertLineSelections (PasteBefore defReg)
            false end_ endChars

    [<TestCase( "abc", "xabc" , [|0; 0; 2; 2; 1; 3|] )>]  // separated
    [<TestCase( "abc", "xabc" , [|0; 0; 1; 1; 1; 2|] )>]  // touching
    [<TestCase( "abc", "xabc" , [|0; 0; 0; 0; 1; 1|] )>]  // equal
    member _.PasteBefore_Insert_BeforeEof_OneSlot start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        setRegister ["x"]

        commandAssertLineSelections (PasteBefore defReg)
            false end_ endChars

    [<TestCase( "ab" , "xab" , [|0; 0; 2; 2; 1; 3|] )>]  // separated
    [<TestCase( "a"  , "xa"  , [|0; 0; 1; 1; 1; 2|] )>]  // touching
    [<TestCase( ""   , "x"   , [|0; 0; 0; 0; 1; 1|] )>]  // equal
    member _.PasteBefore_Insert_AtEof_OneSlot start end_ chars =
        let startRanges, endChars = toRangesAndChars chars

        init start startRanges

        setRegister ["x"]

        commandAssertLineSelections (PasteBefore defReg)
            false end_ endChars

    // PasteAfter - Normal -----------------------------------------------------

    [<TestCase( "abcde", [|"abxcdey"    |], [|0; 1; 3; 4; 0; 2; 0; 6|] )>]  // separated
    [<TestCase( "abcde", [|"abxcdye"    |], [|0; 1; 2; 3; 0; 2; 0; 5|] )>]  // touching
    [<TestCase( "abcde", [|"abcdxye"    |], [|0; 3; 0; 3; 0; 4; 0; 5|] )>]  // equal
    [<TestCase( "abcde", [|"abxcdye"    |], [|0; 3; 0; 1; 0; 2; 0; 5|] )>]  // outer/inner left
    [<TestCase( "abcde", [|"abcxdye"    |], [|0; 3; 1; 2; 0; 3; 0; 5|] )>]  // outer/inner center
    [<TestCase( "abcde", [|"abcdxye"    |], [|0; 3; 2; 3; 0; 4; 0; 5|] )>]  // outer/inner right
    member _.PasteAfter_Normal_BeforeEof_TwoSlots start end_ numbers =
        let startRanges, endPositions = toRangesAndPositions numbers

        init start startRanges

        setRegister ["x"; "y"]

        commandAssertLinesSelections (PasteAfter defReg)
            true end_ endPositions

    [<TestCase( "abcd" , [|"abxcd"; "y" |], [|0; 1; 3; 4; 0; 2; 1; 0|] )>]  // separated
    [<TestCase( "abc"  , [|"abxc" ; "y" |], [|0; 1; 2; 3; 0; 2; 1; 0|] )>]  // touching
    [<TestCase( "abc"  , [|"abc"  ; "xy"|], [|0; 3; 0; 3; 1; 0; 1; 1|] )>]  // equal
    [<TestCase( "abc"  , [|"abxc" ; "y" |], [|0; 3; 0; 1; 0; 2; 1; 0|] )>]  // outer/inner left
    [<TestCase( "abc"  , [|"abcx" ; "y" |], [|0; 3; 1; 2; 0; 3; 1; 0|] )>]  // outer/inner center
    [<TestCase( "abc"  , [|"abc"  ; "xy"|], [|0; 3; 2; 3; 1; 0; 1; 1|] )>]  // outer/inner right
    member _.PasteAfter_Normal_AtEof_TwoSlots start end_ numbers =
        let startRanges, endPositions = toRangesAndPositions numbers

        init start startRanges

        setRegister ["x"; "y"]

        commandAssertLinesSelections (PasteAfter defReg)
            true end_ endPositions

    [<TestCase( "abcde", [|"abxcde"     |], [|0; 1; 3; 4; 0; 2; 0; 5|] )>]  // separated
    [<TestCase( "abcde", [|"abxcde"     |], [|0; 1; 2; 3; 0; 2; 0; 4|] )>]  // touching
    [<TestCase( "abcde", [|"abcdxe"     |], [|0; 3; 0; 3; 0; 4; 0; 4|] )>]  // equal
    [<TestCase( "abcde", [|"abxcde"     |], [|0; 3; 0; 1; 0; 2; 0; 4|] )>]  // outer/inner left
    [<TestCase( "abcde", [|"abcxde"     |], [|0; 3; 1; 2; 0; 3; 0; 4|] )>]  // outer/inner center
    [<TestCase( "abcde", [|"abcdxe"     |], [|0; 3; 2; 3; 0; 4; 0; 4|] )>]  // outer/inner right
    member _.PasteAfter_Normal_BeforeEof_OneSlot start end_ numbers =
        let startRanges, endPositions = toRangesAndPositions numbers

        init start startRanges

        setRegister ["x"]

        commandAssertLinesSelections (PasteAfter defReg)
            true end_ endPositions

    [<TestCase( "abcd" , [|"abxcd"      |], [|0; 1; 3; 4; 0; 2; 0; 5|] )>]  // separated
    [<TestCase( "abc"  , [|"abxc"       |], [|0; 1; 2; 3; 0; 2; 0; 4|] )>]  // touching
    [<TestCase( "abc"  , [|"abc"  ; "x" |], [|0; 3; 0; 3; 1; 0; 1; 0|] )>]  // equal
    [<TestCase( "abc"  , [|"abxc"       |], [|0; 3; 0; 1; 0; 2; 0; 4|] )>]  // outer/inner left
    [<TestCase( "abc"  , [|"abcx"       |], [|0; 3; 1; 2; 0; 3; 0; 4|] )>]  // outer/inner center
    [<TestCase( "abc"  , [|"abc"  ; "x" |], [|0; 3; 2; 3; 1; 0; 1; 0|] )>]  // outer/inner right
    member _.PasteAfter_Normal_AtEof_OneSlot start end_ numbers =
        let startRanges, endPositions = toRangesAndPositions numbers

        init start startRanges

        setRegister ["x"]

        commandAssertLinesSelections (PasteAfter defReg)
            true end_ endPositions
