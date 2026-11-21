module PromptCommonTest

open NUnit.Framework
open System

open Commands.InCommands
open PromptBuffer
open Registers
open UserMessages

let contextRef      = TestUtils.makeContextRef 80 25
let extraContextRef = TestUtils.makeExtraContextRef 1

[<TestFixture>]
type CommonTest () =
    let myBuffer = new PromptBuffer (contextRef, extraContextRef, UserMessages (), Registers ())

    // initialization

    let init cursorBeforeEol line =
        myBuffer.LoadString line

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

    let commandCount command count =
        for _ = 1 to count do
            performCommand command

    let assertCursor expCursorChar =
        Assert.AreEqual (expCursorChar, myBuffer.Main.Cursor.char)

    // commands followed by assertion(s)

    let commandAssertCursor command expCursorChar =
        performCommand command
        assertCursor expCursorChar

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // CursorLeft/Right, CursorAfterSelection ----------------------------------

    [<TestCase(false, 3)>]
    member _.CursorLeft cbe startChar =
        init cbe "abc"
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor CursorLeft 2
        commandAssertCursor CursorLeft 1
        commandAssertCursor CursorLeft 0
        commandAssertCursor CursorLeft 0

    [<TestCase(false)>]
    member _.CursorRight cbe =
        init cbe "abc"
        assertCursor 0

        commandAssertCursor CursorRight 1
        commandAssertCursor CursorRight 2
        commandAssertCursor CursorRight 3
        commandAssertCursor CursorRight 3

    [<TestCase(false)>]
    member _.CursorAfterSelection cbe =
        init cbe "abc"
        assertCursor 0

        commandAssertCursor (CursorAfterSelection true) 1
        commandAssertCursor (CursorAfterSelection true) 2
        commandAssertCursor (CursorAfterSelection true) 3

    // CursorLeft/Right... -----------------------------------------------------

    [<TestCase(false, 12)>]
    member _.CursorLeftAtWordStart cbe startChar =
        //        0123456789012
        init cbe " abc d_f:-/ "
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor CursorLeftAtWordStart  8
        commandAssertCursor CursorLeftAtWordStart  5
        commandAssertCursor CursorLeftAtWordStart  1
        commandAssertCursor CursorLeftAtWordStart  0
        commandAssertCursor CursorLeftAtWordStart  0

    [<TestCase(false, 14)>]
    member _.CursorLeftAtWordEnd_OneLine cbe startChar =
        //        012345678901234
        init cbe "  abc d_f:-/  "
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor CursorLeftAtWordEnd 11
        commandAssertCursor CursorLeftAtWordEnd 8
        commandAssertCursor CursorLeftAtWordEnd 4
        commandAssertCursor CursorLeftAtWordEnd 0
        commandAssertCursor CursorLeftAtWordEnd 0

    [<TestCase(false, 14)>]
    member _.CursorLeftAfterWordEnd_OneLine cbe startChar =
        //        012345678901234
        init cbe "  abc d_f:-/  "
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor CursorLeftAfterWordEnd 12
        commandAssertCursor CursorLeftAfterWordEnd 9
        commandAssertCursor CursorLeftAfterWordEnd 5
        commandAssertCursor CursorLeftAfterWordEnd 0
        commandAssertCursor CursorLeftAfterWordEnd 0

    [<TestCase(false)>]
    member _.CursorRightAtWordStart cbe =
        //        0123456789012
        init cbe " abc d_f:-/ "
        assertCursor 0

        commandAssertCursor CursorRightAtWordStart 1
        commandAssertCursor CursorRightAtWordStart 5
        commandAssertCursor CursorRightAtWordStart 8
        commandAssertCursor CursorRightAtWordStart 12
        commandAssertCursor CursorRightAtWordStart 12

    [<TestCase(false)>]
    member _.CursorRightAtWordEnd_OneLine cbe =
        //        012345678901234
        init cbe "  abc d_f:-/  "
        assertCursor 0

        commandAssertCursor CursorRightAtWordEnd 4
        commandAssertCursor CursorRightAtWordEnd 8
        commandAssertCursor CursorRightAtWordEnd 11
        commandAssertCursor CursorRightAtWordEnd 14
        commandAssertCursor CursorRightAtWordEnd 14

    [<TestCase(false)>]
    member _.CursorRightBeforeWordStart_OneLine cbe =
        //        012345678901234
        init cbe "  abc d_f:-/  "
        assertCursor 0

        commandAssertCursor CursorRightBeforeWordStart 1
        commandAssertCursor CursorRightBeforeWordStart 5
        commandAssertCursor CursorRightBeforeWordStart 8
        commandAssertCursor CursorRightBeforeWordStart 14
        commandAssertCursor CursorRightBeforeWordStart 14

    // CursorLeft/Right...Char -------------------------------------------------

    [<TestCase(false, 11)>]
    member _.CursorLeftToChar cbe startChar =
        //        012345678901
        init cbe "  a a  aa  "
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor (CursorLeftToChar 'a') 8
        commandAssertCursor (CursorLeftToChar 'a') 7
        commandAssertCursor (CursorLeftToChar 'a') 4
        commandAssertCursor (CursorLeftToChar 'a') 2
        commandAssertCursor (CursorLeftToChar 'a') 8

    [<TestCase(false, 11)>]
    member _.CursorLeftUntilChar cbe startChar =
        //        012345678901
        init cbe "  a a  aa  "
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor (CursorLeftUntilChar 'a') 9
        commandAssertCursor (CursorLeftUntilChar 'a') 8
        commandAssertCursor (CursorLeftUntilChar 'a') 5
        commandAssertCursor (CursorLeftUntilChar 'a') 3
        commandAssertCursor (CursorLeftUntilChar 'a') 9

    [<TestCase(false)>]
    member _.CursorRightToChar cbe =
        //        012345678901
        init cbe "  a a  aa  "
        assertCursor 0

        commandAssertCursor (CursorRightToChar 'a') 2
        commandAssertCursor (CursorRightToChar 'a') 4
        commandAssertCursor (CursorRightToChar 'a') 7
        commandAssertCursor (CursorRightToChar 'a') 8
        commandAssertCursor (CursorRightToChar 'a') 2

    [<TestCase(false)>]
    member _.CursorRightUntilChar cbe =
        //        012345678901
        init cbe "  a a  aa  "
        assertCursor 0

        commandAssertCursor (CursorRightUntilChar 'a') 1
        commandAssertCursor (CursorRightUntilChar 'a') 3
        commandAssertCursor (CursorRightUntilChar 'a') 6
        commandAssertCursor (CursorRightUntilChar 'a') 7
        commandAssertCursor (CursorRightUntilChar 'a') 1

    // CursorLeft/Right...Char - special cases ---------------------------------

    [<TestCase(false, 0)>]
    member _.CursorLeftRightChar_EmptyLine cbe startChar =
        init cbe ""
        commandCount CursorRight startChar
        assertCursor 0

        commandAssertCursor (CursorLeftToChar     'a') startChar
        commandAssertCursor (CursorLeftToChar     'a') startChar
        commandAssertCursor (CursorLeftUntilChar  'a') startChar
        commandAssertCursor (CursorLeftUntilChar  'a') startChar
        commandAssertCursor (CursorRightToChar    'a') startChar
        commandAssertCursor (CursorRightToChar    'a') startChar
        commandAssertCursor (CursorRightUntilChar 'a') startChar
        commandAssertCursor (CursorRightUntilChar 'a') startChar

    [<TestCase(false, 1)>]
    member _.CursorLeftRightChar_OneCharLine cbe startChar =
        init cbe "a"
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor (CursorLeftToChar     'a') 0
        commandAssertCursor (CursorLeftToChar     'a') 0
        commandAssertCursor (CursorLeftUntilChar  'a') 1
        commandAssertCursor (CursorLeftUntilChar  'a') 1
        commandAssertCursor (CursorRightToChar    'a') 0
        commandAssertCursor (CursorRightToChar    'a') 0
        commandAssertCursor (CursorRightUntilChar 'a') 1
        commandAssertCursor (CursorRightUntilChar 'a') 1

    [<TestCase(false, 0)>]
    [<TestCase(false, 1)>]
    [<TestCase(false, 2)>]
    [<TestCase(false, 3)>]
    member _.CursorLeftRightChar_NotFound cbe startChar =
        init cbe " x "
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor (CursorLeftToChar     'a') startChar
        commandAssertCursor (CursorLeftToChar     'a') startChar
        commandAssertCursor (CursorLeftUntilChar  'a') startChar
        commandAssertCursor (CursorLeftUntilChar  'a') startChar
        commandAssertCursor (CursorRightToChar    'a') startChar
        commandAssertCursor (CursorRightToChar    'a') startChar
        commandAssertCursor (CursorRightUntilChar 'a') startChar
        commandAssertCursor (CursorRightUntilChar 'a') startChar

    // CursorToPairChar --------------------------------------------------------

    [<TestCase(false, 1 , 1 )>]
    [<TestCase(false, 2 , 2 )>]
    [<TestCase(false, 3 , 4 )>]
    [<TestCase(false, 4 , 3 )>]
    [<TestCase(false, 6 , 11)>]
    [<TestCase(false, 11, 6 )>]
    [<TestCase(false, 13, 13)>]
    member _.CursorToPairChar_OneLine cbe startChar endChar =
        //        0123456789012345
        init cbe " ) () ({()]) ( "
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor CursorToPairChar endChar

    [<TestCase(false, 0, 0)>]
    member _.CursorToPairChar_EmptyLine cbe startChar endChar =
        //        0
        init cbe ""
        commandCount CursorRight    startChar
        assertCursor startChar

        commandAssertCursor CursorToPairChar endChar

    [<TestCase(false, 1, 1)>]
    member _.CursorToPairChar_LineEnd cbe startChar endChar =
        //        01
        init cbe " "
        commandCount CursorRight    startChar
        assertCursor startChar

        commandAssertCursor CursorToPairChar endChar

    // CursorHardLineStart/End, CursorAtEol, CursorBeforeEol -------------------

    [<TestCase(false, 3)>]
    [<TestCase(true , 3)>]
    member _.CursorHardLineStart cbe startChar =
        init cbe "abc"
        commandCount CursorRight startChar
        assertCursor startChar

        commandAssertCursor CursorHardLineStart 0
        commandAssertCursor CursorHardLineStart 0

    [<TestCase(false, 0)>]
    [<TestCase(true , 0)>]
    member _.CursorHardLineEnd_EmptyLine cbe endChar =
        init cbe ""
        assertCursor 0

        commandAssertCursor CursorHardLineEnd endChar
        commandAssertCursor CursorHardLineEnd endChar

    [<TestCase(false, 3)>]
    [<TestCase(true , 3)>]
    member _.CursorHardLineEnd_NonEmptyLine cbe endChar =
        init cbe "abc"
        assertCursor 0

        commandAssertCursor CursorHardLineEnd endChar
        commandAssertCursor CursorHardLineEnd endChar

    [<TestCase(false, 0)>]
    [<TestCase(true , 0)>]
    member _.CursorBeforeEol_EmptyLine cbe endChar =
        init cbe ""
        assertCursor 0

        commandAssertCursor CursorBeforeEol endChar
        commandAssertCursor CursorBeforeEol endChar

    [<TestCase(false, 2)>]
    [<TestCase(true , 2)>]
    member _.CursorBeforeEol_NonEmptyLine cbe endChar =
        init cbe "abc"
        assertCursor 0

        commandAssertCursor CursorBeforeEol endChar
        commandAssertCursor CursorBeforeEol endChar

    // CursorAt ----------------------------------------------------------------

    [<TestCase(false)>]
    [<TestCase(true )>]
    member _.CursorAt_NonEmptyLine cbe =
        init cbe "abc"
        assertCursor 0

        commandAssertCursor (CursorAt 1) 1
        commandAssertCursor (CursorAt 3) 3
