module TextAreaCommonTest

open NUnit.Framework
open System

open Commands.InCommands
open Registers
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef 80 25

[<TestFixture>]
type CommonModeTest () =
    let myBuffer = new TextAreaBuffer (contextRef, UserMessages (), Registers (), "")

    // initialization

    let init cursorBeforeEol lines =
        myBuffer.LoadStrings lines

        TestUtils.set_cursorBeforeEol contextRef cursorBeforeEol

    let toTuple4 (a: 'T array) =
        a[0], a[1], a[2], a[3]

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

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // CursorLeft/Right, CursorAfterSelection ----------------------------------

    [<TestCase(false, 1)>]
    [<TestCase(true , 1)>]
    member _.CursorLeft cbe startChar =
        init cbe [
            "abc"
            "def"
            "ghi"
        ]
        command      CursorHardDown
        commandCount CursorRight startChar
        assertCursor 1 startChar

        commandAssertCursor CursorLeft 1 0
        commandAssertCursor CursorLeft 0 3
        commandAssertCursor CursorLeft 0 2
        commandAssertCursor CursorLeft 0 1
        commandAssertCursor CursorLeft 0 0
        commandAssertCursor CursorLeft 0 0

    [<TestCase(false, 2)>]
    [<TestCase(true , 2)>]
    member _.CursorRight cbe startChar =
        init cbe [
            "abc"
            "def"
            "ghi"
        ]
        command      CursorHardDown
        commandCount CursorRight startChar
        assertCursor 1 startChar

        commandAssertCursor CursorRight 1 3
        commandAssertCursor CursorRight 2 0
        commandAssertCursor CursorRight 2 1
        commandAssertCursor CursorRight 2 2
        commandAssertCursor CursorRight 2 3
        commandAssertCursor CursorRight 2 3

    [<TestCase(true, 2)>]
    member _.CursorAfterSelection cbe startChar =
        init cbe [
            "abc"
            "def"
            "ghi"
        ]
        command      CursorHardDown
        commandCount CursorRight startChar
        assertCursor 1 startChar

        commandAssertCursor (CursorAfterSelection true ) 1 3
        commandAssertCursor (CursorAfterSelection true ) 2 0
        commandAssertCursor (CursorAfterSelection true ) 2 1
        commandAssertCursor (CursorAfterSelection true ) 2 2
        commandAssertCursor (CursorAfterSelection true ) 2 3
        commandAssertCursor (CursorAfterSelection true ) 3 0

    // CursorLeft/Right... on one line -----------------------------------------

    [<TestCase(false, 14)>]
    [<TestCase(true , 14)>]
    member _.CursorLeftAtWordStart_OneLine cbe startChar =
        //         012345678901234
        init cbe ["  abc d_f:-/  "]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorLeftAtWordStart 0 9
        commandAssertCursor CursorLeftAtWordStart 0 6
        commandAssertCursor CursorLeftAtWordStart 0 2
        commandAssertCursor CursorLeftAtWordStart 0 0
        commandAssertCursor CursorLeftAtWordStart 0 0

    [<TestCase(true, 14)>]
    member _.CursorLeftAtWordEnd_OneLine cbe startChar =
        //         012345678901234
        init cbe ["  abc d_f:-/  "]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorLeftAtWordEnd 0 11
        commandAssertCursor CursorLeftAtWordEnd 0 8
        commandAssertCursor CursorLeftAtWordEnd 0 4
        commandAssertCursor CursorLeftAtWordEnd 0 0
        commandAssertCursor CursorLeftAtWordEnd 0 0

    [<TestCase(true, 14)>]
    member _.CursorLeftAfterWordEnd_OneLine cbe startChar =
        //         012345678901234
        init cbe ["  abc d_f:-/  "]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorLeftAfterWordEnd 0 12
        commandAssertCursor CursorLeftAfterWordEnd 0 9
        commandAssertCursor CursorLeftAfterWordEnd 0 5
        commandAssertCursor CursorLeftAfterWordEnd 0 0
        commandAssertCursor CursorLeftAfterWordEnd 0 0

    [<TestCase(false, 14)>]
    [<TestCase(true , 14)>]
    member _.CursorRightAtWordStart_OneLine cbe endChar =
        //         012345678901234
        init cbe ["  abc d_f:-/  "]
        assertCursor 0 0

        commandAssertCursor CursorRightAtWordStart 0 2
        commandAssertCursor CursorRightAtWordStart 0 6
        commandAssertCursor CursorRightAtWordStart 0 9
        commandAssertCursor CursorRightAtWordStart 0 endChar
        commandAssertCursor CursorRightAtWordStart 0 endChar

    [<TestCase(true, 14)>]
    member _.CursorRightAtWordEnd_OneLine cbe endChar =
        //         012345678901234
        init cbe ["  abc d_f:-/  "]
        assertCursor 0 0

        commandAssertCursor CursorRightAtWordEnd 0 4
        commandAssertCursor CursorRightAtWordEnd 0 8
        commandAssertCursor CursorRightAtWordEnd 0 11
        commandAssertCursor CursorRightAtWordEnd 0 endChar
        commandAssertCursor CursorRightAtWordEnd 0 endChar

    [<TestCase(true, 14)>]
    member _.CursorRightBeforeWordStart_OneLine cbe endChar =
        //         012345678901234
        init cbe ["  abc d_f:-/  "]
        assertCursor 0 0

        commandAssertCursor CursorRightBeforeWordStart 0 1
        commandAssertCursor CursorRightBeforeWordStart 0 5
        commandAssertCursor CursorRightBeforeWordStart 0 8
        commandAssertCursor CursorRightBeforeWordStart 0 endChar
        commandAssertCursor CursorRightBeforeWordStart 0 endChar

    // CursorLeft/Right... through multiple lines ------------------------------

    [<TestCase(false, 7)>]
    [<TestCase(true , 7)>]
    member _.CursorLeftAtWordStart_MultipleLines cbe startChar =
        init cbe [
            "  abc"
            ""
            "abc  "
            "   "
            "  abc  "
        ]
        commandCount CursorHardDown 4
        commandCount CursorRight    startChar
        assertCursor 4 startChar

        commandAssertCursor CursorLeftAtWordStart 4 2
        commandAssertCursor CursorLeftAtWordStart 2 0
        commandAssertCursor CursorLeftAtWordStart 0 2
        commandAssertCursor CursorLeftAtWordStart 0 0
        commandAssertCursor CursorLeftAtWordStart 0 0

    [<TestCase(true, 7)>]
    member _.CursorLeftAtWordEnd_MultipleLines cbe startChar =
        init cbe [
            "  abc"
            ""
            "  abc"
            "   "
            "  abc  "
        ]
        commandCount CursorHardDown 4
        commandCount CursorRight    startChar
        assertCursor 4 startChar

        commandAssertCursor CursorLeftAtWordEnd 4 4
        commandAssertCursor CursorLeftAtWordEnd 2 4
        commandAssertCursor CursorLeftAtWordEnd 0 4
        commandAssertCursor CursorLeftAtWordEnd 0 0
        commandAssertCursor CursorLeftAtWordEnd 0 0

    [<TestCase(true, 7)>]
    member _.CursorLeftAfterWordEnd_MultipleLines cbe startChar =
        init cbe [
            "  abc"
            ""
            "  abc"
            "   "
            "  abc  "
        ]
        commandCount CursorHardDown 4
        commandCount CursorRight    startChar
        assertCursor 4 startChar

        commandAssertCursor CursorLeftAfterWordEnd 4 5
        commandAssertCursor CursorLeftAfterWordEnd 2 5
        commandAssertCursor CursorLeftAfterWordEnd 0 5
        commandAssertCursor CursorLeftAfterWordEnd 0 0
        commandAssertCursor CursorLeftAfterWordEnd 0 0

    [<TestCase(false)>]
    [<TestCase(true )>]
    member _.CursorRightAtWordStart_MultipleLines cbe =
        init cbe [
            "  abc  "
            "   "
            "abc  "
            ""
            "abc  "
        ]
        assertCursor 0 0

        commandAssertCursor CursorRightAtWordStart 0 2
        commandAssertCursor CursorRightAtWordStart 2 0
        commandAssertCursor CursorRightAtWordStart 4 0
        commandAssertCursor CursorRightAtWordStart 4 5
        commandAssertCursor CursorRightAtWordStart 4 5

    [<TestCase(true)>]
    member _.CursorRightAtWordEnd_MultipleLines cbe =
        init cbe [
            "  abc  "
            "   "
            "  abc"
            ""
            "abc  "
        ]
        assertCursor 0 0

        commandAssertCursor CursorRightAtWordEnd 0 4
        commandAssertCursor CursorRightAtWordEnd 2 4
        commandAssertCursor CursorRightAtWordEnd 4 2
        commandAssertCursor CursorRightAtWordEnd 4 5
        commandAssertCursor CursorRightAtWordEnd 4 5

    [<TestCase(true)>]
    member _.CursorRightBeforeWordStart_MultipleLines cbe =
        init cbe [
            "  abc  "
            "   "
            "abc  "
            ""
            "abc  "
        ]
        assertCursor 0 0

        commandAssertCursor CursorRightBeforeWordStart 0 1
        commandAssertCursor CursorRightBeforeWordStart 1 3
        commandAssertCursor CursorRightBeforeWordStart 3 0
        commandAssertCursor CursorRightBeforeWordStart 4 5
        commandAssertCursor CursorRightBeforeWordStart 4 5

    // CursorLeft/Right...Char on one line -------------------------------------

    [<TestCase(true, 11)>]
    member _.CursorLeftToChar_OneLine cbe startChar =
        //         012345678901
        init cbe ["  a a  aa  "]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor (CursorLeftToChar 'a') 0 8
        commandAssertCursor (CursorLeftToChar 'a') 0 7
        commandAssertCursor (CursorLeftToChar 'a') 0 4
        commandAssertCursor (CursorLeftToChar 'a') 0 2
        commandAssertCursor (CursorLeftToChar 'a') 0 8

    [<TestCase(true, 11)>]
    member _.CursorLeftUntilChar_OneLine cbe startChar =
        //         012345678901
        init cbe ["  a a  aa  "]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor (CursorLeftUntilChar 'a') 0 9
        commandAssertCursor (CursorLeftUntilChar 'a') 0 8
        commandAssertCursor (CursorLeftUntilChar 'a') 0 5
        commandAssertCursor (CursorLeftUntilChar 'a') 0 3
        commandAssertCursor (CursorLeftUntilChar 'a') 0 9

    [<TestCase(true)>]
    member _.CursorRightToChar_OneLine cbe =
        //         012345678901
        init cbe ["  a a  aa  "]
        assertCursor 0 0

        commandAssertCursor (CursorRightToChar 'a') 0 2
        commandAssertCursor (CursorRightToChar 'a') 0 4
        commandAssertCursor (CursorRightToChar 'a') 0 7
        commandAssertCursor (CursorRightToChar 'a') 0 8
        commandAssertCursor (CursorRightToChar 'a') 0 2

    [<TestCase(true)>]
    member _.CursorRightUntilChar_OneLine cbe =
        //         012345678901
        init cbe ["  a a  aa  "]
        assertCursor 0 0

        commandAssertCursor (CursorRightUntilChar 'a') 0 1
        commandAssertCursor (CursorRightUntilChar 'a') 0 3
        commandAssertCursor (CursorRightUntilChar 'a') 0 6
        commandAssertCursor (CursorRightUntilChar 'a') 0 7
        commandAssertCursor (CursorRightUntilChar 'a') 0 1

    // CursorLeft/Right...Char on multiple lines -------------------------------

    [<TestCase(true, 5)>]
    member _.CursorLeftToChar_MultipleLines cbe startChar =
        init cbe [
            "  a"
            ""
            "a  "
            "   "
            "  a  "
        ]
        commandCount CursorHardDown 4
        commandCount CursorRight    startChar
        assertCursor 4 startChar

        commandAssertCursor (CursorLeftToChar 'a') 4 2
        commandAssertCursor (CursorLeftToChar 'a') 2 0
        commandAssertCursor (CursorLeftToChar 'a') 0 2
        commandAssertCursor (CursorLeftToChar 'a') 4 2

    [<TestCase(true, 5)>]
    member _.CursorLeftUntilChar_MultipleLines cbe startChar =
        init cbe [
            "  a"
            ""
            "a  "
            "   "
            "  a  "
        ]
        commandCount CursorHardDown 4
        commandCount CursorRight    startChar
        assertCursor 4 startChar

        commandAssertCursor (CursorLeftUntilChar 'a') 4 3
        commandAssertCursor (CursorLeftUntilChar 'a') 2 1
        commandAssertCursor (CursorLeftUntilChar 'a') 0 3
        commandAssertCursor (CursorLeftUntilChar 'a') 4 3

    [<TestCase(true)>]
    member _.CursorRightToChar_MultipleLines cbe =
        init cbe [
            "  a  "
            ""
            "a  "
            "   "
            "  a"
        ]
        assertCursor 0 0

        commandAssertCursor (CursorRightToChar 'a') 0 2
        commandAssertCursor (CursorRightToChar 'a') 2 0
        commandAssertCursor (CursorRightToChar 'a') 4 2
        commandAssertCursor (CursorRightToChar 'a') 0 2

    [<TestCase(true)>]
    member _.CursorRightUntilChar_MultipleLines cbe =
        init cbe [
            "  a  "
            ""
            "a  "
            "   "
            "  a"
        ]
        assertCursor 0 0

        commandAssertCursor (CursorRightUntilChar 'a') 0 1
        commandAssertCursor (CursorRightUntilChar 'a') 1 0
        commandAssertCursor (CursorRightUntilChar 'a') 4 1
        commandAssertCursor (CursorRightUntilChar 'a') 0 1

    // CursorLeft/Right...Char - special cases ---------------------------------

    [<TestCase(true, 0)>]
    member _.CursorLeftRightChar_EmptyLine cbe startChar =
        init cbe [""]
        commandCount CursorRight startChar
        assertCursor 0 0

        commandAssertCursor (CursorLeftToChar     'a') 0 startChar
        commandAssertCursor (CursorLeftToChar     'a') 0 startChar
        commandAssertCursor (CursorLeftUntilChar  'a') 0 startChar
        commandAssertCursor (CursorLeftUntilChar  'a') 0 startChar
        commandAssertCursor (CursorRightToChar    'a') 0 startChar
        commandAssertCursor (CursorRightToChar    'a') 0 startChar
        commandAssertCursor (CursorRightUntilChar 'a') 0 startChar
        commandAssertCursor (CursorRightUntilChar 'a') 0 startChar

    [<TestCase(true, 1)>]
    member _.CursorLeftRightChar_OneCharLine cbe startChar =
        init cbe ["a"]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor (CursorLeftToChar     'a') 0 0
        commandAssertCursor (CursorLeftToChar     'a') 0 0
        commandAssertCursor (CursorLeftUntilChar  'a') 0 1
        commandAssertCursor (CursorLeftUntilChar  'a') 0 1
        commandAssertCursor (CursorRightToChar    'a') 0 0
        commandAssertCursor (CursorRightToChar    'a') 0 0
        commandAssertCursor (CursorRightUntilChar 'a') 0 1
        commandAssertCursor (CursorRightUntilChar 'a') 0 1

    [<TestCase(true, 0)>]
    [<TestCase(true, 1)>]
    [<TestCase(true, 2)>]
    [<TestCase(true, 3)>]
    member _.CursorLeftRightChar_NotFound cbe startChar =
        init cbe [" x "]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor (CursorLeftToChar     'a') 0 startChar
        commandAssertCursor (CursorLeftToChar     'a') 0 startChar
        commandAssertCursor (CursorLeftUntilChar  'a') 0 startChar
        commandAssertCursor (CursorLeftUntilChar  'a') 0 startChar
        commandAssertCursor (CursorRightToChar    'a') 0 startChar
        commandAssertCursor (CursorRightToChar    'a') 0 startChar
        commandAssertCursor (CursorRightUntilChar 'a') 0 startChar
        commandAssertCursor (CursorRightUntilChar 'a') 0 startChar

    // CursorToPairChar --------------------------------------------------------

    [<TestCase(true, 1 , 1 )>]
    [<TestCase(true, 2 , 2 )>]
    [<TestCase(true, 3 , 4 )>]
    [<TestCase(true, 4 , 3 )>]
    [<TestCase(true, 6 , 11)>]
    [<TestCase(true, 11, 6 )>]
    [<TestCase(true, 13, 13)>]
    member _.CursorToPairChar_OneLine cbe startChar endChar =
        //         0123456789012345
        init cbe [" ) () ({()]) ( "]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorToPairChar 0 endChar

    [<TestCase(true, 0, 1, 0, 1)>]
    [<TestCase(true, 0, 2, 0, 2)>]
    [<TestCase(true, 1, 0, 2, 0)>]
    [<TestCase(true, 2, 0, 1, 0)>]
    [<TestCase(true, 3, 0, 4, 2)>]
    [<TestCase(true, 4, 2, 3, 0)>]
    [<TestCase(true, 5, 1, 5, 1)>]
    member _.CursorToPairChar_MultipleLines cbe startLine startChar endLine endChar =
        init cbe [
            " ) "
            "("
            ")"
            "([("
            ")})"
            " ( "
        ]
        commandCount CursorHardDown startLine
        commandCount CursorRight    startChar
        assertCursor startLine startChar

        commandAssertCursor CursorToPairChar endLine endChar

    [<TestCase(true, 0, 0)>]
    member _.CursorToPairChar_EmptyLine cbe startChar endChar =
        //         0
        init cbe [""]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorToPairChar 0 endChar

    [<TestCase(true, 1, 1)>]
    member _.CursorToPairChar_LineEnd cbe startChar endChar =
        //         01
        init cbe [" "]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorToPairChar 0 endChar

    // CursorHardLineStart/End, CursorToNewLine --------------------------------

    [<TestCase(false, 3)>]
    [<TestCase(true , 3)>]
    member _.CursorHardLineStart_AtFirstLine cbe startChar =
        init cbe [
            "abc"
        ]
        commandCount CursorRight startChar
        assertCursor 0 startChar

        commandAssertCursor CursorHardLineStart 0 0
        commandAssertCursor CursorHardLineStart 0 0

    [<TestCase(false, 3)>]
    [<TestCase(true , 3)>]
    member _.CursorHardLineStart_AtSecondLine cbe startChar =
        init cbe [
            ""
            "abc"
            ""
        ]
        command      CursorHardDown
        commandCount CursorRight startChar
        assertCursor 1 startChar

        commandAssertCursor CursorHardLineStart 1 0
        commandAssertCursor CursorHardLineStart 1 0

    [<TestCase(false, 0)>]
    [<TestCase(true , 0)>]
    member _.CursorHardLineEnd_AtEmptyLine cbe endChar =
        init cbe [
            ""
        ]
        assertCursor 0 0

        commandAssertCursor CursorHardLineEnd 0 endChar
        commandAssertCursor CursorHardLineEnd 0 endChar

    [<TestCase(false, 3)>]
    [<TestCase(true , 2)>]
    member _.CursorHardLineEnd_AtLastLine cbe endChar =
        init cbe [
            "abc"
        ]
        assertCursor 0 0

        commandAssertCursor CursorHardLineEnd 0 endChar
        commandAssertCursor CursorHardLineEnd 0 endChar

    [<TestCase(false, 3)>]
    [<TestCase(true , 2)>]
    member _.CursorHardLineEnd_AtSecondLine cbe endChar =
        init cbe [
            ""
            "abc"
            ""
        ]
        command      CursorHardDown
        assertCursor 1 0

        commandAssertCursor CursorHardLineEnd 1 endChar
        commandAssertCursor CursorHardLineEnd 1 endChar

    [<TestCase(true, 0)>]
    member _.CursorAtEol_AtEmptyLine cbe endChar =
        init cbe [
            ""
        ]
        assertCursor 0 0

        commandAssertCursor CursorAtEol 0 endChar
        commandAssertCursor CursorAtEol 0 endChar

    [<TestCase(true, 3)>]
    member _.CursorAtEol_AtLastLine cbe endChar =
        init cbe [
            "abc"
        ]
        assertCursor 0 0

        commandAssertCursor CursorAtEol 0 endChar
        commandAssertCursor CursorAtEol 0 endChar

    [<TestCase(true, 3)>]
    member _.CursorAtEol_AtSecondLine cbe endChar =
        init cbe [
            ""
            "abc"
            ""
        ]
        command      CursorHardDown
        assertCursor 1 0

        commandAssertCursor CursorAtEol 1 endChar
        commandAssertCursor CursorAtEol 1 endChar

    [<TestCase(true, 0)>]
    member _.CursorBeforeEol_AtEmptyLine cbe endChar =
        init cbe [
            ""
        ]
        assertCursor 0 0

        commandAssertCursor CursorBeforeEol 0 endChar
        commandAssertCursor CursorBeforeEol 0 endChar

    [<TestCase(true, 2)>]
    member _.CursorBeforeEol_AtLastLine cbe endChar =
        init cbe [
            "abc"
        ]
        assertCursor 0 0

        commandAssertCursor CursorBeforeEol 0 endChar
        commandAssertCursor CursorBeforeEol 0 endChar

    [<TestCase(true, 2)>]
    member _.CursorBeforeEol_AtSecondLine cbe endChar =
        init cbe [
            ""
            "abc"
            ""
        ]
        command      CursorHardDown
        assertCursor 1 0

        commandAssertCursor CursorBeforeEol 1 endChar
        commandAssertCursor CursorBeforeEol 1 endChar
