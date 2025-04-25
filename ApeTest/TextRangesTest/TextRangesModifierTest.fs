module TextRangesModifierTest

open NUnit.Framework

open Position
open TextRange
open TextRanges
open TextRangesModifier

[<TestFixture>]
type TextRangesModifierTest () =

    let position line char =
        {
            line = line
            char = char
        }

    let textRange (line, char) =
        {
            first = position line char
            last  = position line char
        }

    let insertSpecChar (line, char) =
        {
            target     = position line char
            startChars = 1
            newLines   = 0
            endChars   = 0
            preferMove = true
        }

    let insertSpecNewLine (line, char) =
        {
            target     = position line char
            startChars = 0
            newLines   = 1
            endChars   = 0
            preferMove = true
        }

    let deleteSpecChar (line, char) =
        {
            leftKept  = position line (char - 1)
            first     = position line  char
            rightKept = position line (char + 1)
        }

    let deleteSpecNewLine (line, char) =
        {
            leftKept  = position (line - 1) char
            first     = position  line      char
            rightKept = position (line + 1) char
        }

    let oneLineInsertSpecChar (line, char) =
        {
            line       = line
            target     = char
            chars      = 1
            preferMove = true
        }

    let oneLineDeleteSpecChar (line, char) =
        {
            line      = line
            first     = char
            rightKept = char + 1
        }

    let assertTextRanges textRanges expRanges =
        Assert.AreEqual (expRanges, textRanges)

    // ApplyInsert -------------------------------------------------------------

    [<Test>]
    member _.ApplyInsert_SingleChar () =
        let textRanges = TextRanges [|
            textRange (0, 10)
            textRange (0, 11)
            textRange (0, 11)

            textRange (0, 12)
            textRange (0, 13)
            textRange (0, 13)

            textRange (0, 14)
            textRange (0, 15)
            textRange (0, 15)
        |]

        let modifier = TextRangesModifier textRanges

        modifier.StartApplyingModifications ()

        modifier.ApplyInsert (insertSpecChar (0, 11))
        modifier.ApplyInsert (insertSpecChar (0, 11))
        modifier.ApplyInsert (insertSpecChar (0, 10))

        modifier.ApplyInsert (insertSpecChar (0, 16))
        modifier.ApplyInsert (insertSpecChar (0, 16))
        modifier.ApplyInsert (insertSpecChar (0, 15))

        modifier.ApplyInsert (insertSpecChar (0, 21))
        modifier.ApplyInsert (insertSpecChar (0, 21))
        modifier.ApplyInsert (insertSpecChar (0, 20))

        assertTextRanges textRanges [|
            textRange (0, 11)
            textRange (0, 14)
            textRange (0, 14)

            textRange (0, 16)
            textRange (0, 19)
            textRange (0, 19)

            textRange (0, 21)
            textRange (0, 24)
            textRange (0, 24)
        |]

    [<Test>]
    member _.ApplyInsert_SingleNewLine () =
        let textRanges = TextRanges [|
            textRange (10, 0)
            textRange (11, 0)
            textRange (11, 0)

            textRange (12, 0)
            textRange (13, 0)
            textRange (13, 0)

            textRange (14, 0)
            textRange (15, 0)
            textRange (15, 0)
        |]

        let modifier = TextRangesModifier textRanges

        modifier.StartApplyingModifications ()

        modifier.ApplyInsert (insertSpecNewLine (11, 0))
        modifier.ApplyInsert (insertSpecNewLine (11, 0))
        modifier.ApplyInsert (insertSpecNewLine (10, 0))

        modifier.ApplyInsert (insertSpecNewLine (16, 0))
        modifier.ApplyInsert (insertSpecNewLine (16, 0))
        modifier.ApplyInsert (insertSpecNewLine (15, 0))

        modifier.ApplyInsert (insertSpecNewLine (21, 0))
        modifier.ApplyInsert (insertSpecNewLine (21, 0))
        modifier.ApplyInsert (insertSpecNewLine (20, 0))

        assertTextRanges textRanges [|
            textRange (11, 0)
            textRange (14, 0)
            textRange (14, 0)

            textRange (16, 0)
            textRange (19, 0)
            textRange (19, 0)

            textRange (21, 0)
            textRange (24, 0)
            textRange (24, 0)
        |]

    // ApplyDelete -------------------------------------------------------------

    [<Test>]
    member _.ApplyDelete_SingleChar () =
        let textRanges = TextRanges [|
            textRange (0, 11)
            textRange (0, 14)
            textRange (0, 14)

            textRange (0, 16)
            textRange (0, 19)
            textRange (0, 19)

            textRange (0, 21)
            textRange (0, 24)
            textRange (0, 24)
        |]

        let modifier = TextRangesModifier textRanges

        modifier.StartApplyingModifications ()

        modifier.ApplyDelete (deleteSpecChar (0, 12))
        modifier.ApplyDelete (deleteSpecChar (0, 12))
        modifier.ApplyDelete (deleteSpecChar (0, 10))

        modifier.ApplyDelete (deleteSpecChar (0, 14))
        modifier.ApplyDelete (deleteSpecChar (0, 14))
        modifier.ApplyDelete (deleteSpecChar (0, 12))

        modifier.ApplyDelete (deleteSpecChar (0, 16))
        modifier.ApplyDelete (deleteSpecChar (0, 16))
        modifier.ApplyDelete (deleteSpecChar (0, 14))

        assertTextRanges textRanges [|
            textRange (0, 10)
            textRange (0, 11)
            textRange (0, 11)

            textRange (0, 12)
            textRange (0, 13)
            textRange (0, 13)

            textRange (0, 14)
            textRange (0, 15)
            textRange (0, 15)
        |]

    [<Test>]
    member _.ApplyDelete_SingleNewLine () =
        let textRanges = TextRanges [|
            textRange (11, 0)
            textRange (14, 0)
            textRange (14, 0)

            textRange (16, 0)
            textRange (19, 0)
            textRange (19, 0)

            textRange (21, 0)
            textRange (24, 0)
            textRange (24, 0)
        |]

        let modifier = TextRangesModifier textRanges

        modifier.StartApplyingModifications ()

        modifier.ApplyDelete (deleteSpecNewLine (12, 0))
        modifier.ApplyDelete (deleteSpecNewLine (12, 0))
        modifier.ApplyDelete (deleteSpecNewLine (10, 0))

        modifier.ApplyDelete (deleteSpecNewLine (14, 0))
        modifier.ApplyDelete (deleteSpecNewLine (14, 0))
        modifier.ApplyDelete (deleteSpecNewLine (12, 0))

        modifier.ApplyDelete (deleteSpecNewLine (16, 0))
        modifier.ApplyDelete (deleteSpecNewLine (16, 0))
        modifier.ApplyDelete (deleteSpecNewLine (14, 0))

        assertTextRanges textRanges [|
            textRange (10, 0)
            textRange (11, 0)
            textRange (11, 0)

            textRange (12, 0)
            textRange (13, 0)
            textRange (13, 0)

            textRange (14, 0)
            textRange (15, 0)
            textRange (15, 0)
        |]

    // ApplyOneLineInsert ------------------------------------------------------

    [<Test>]
    member _.ApplyOneLineInsert_SingleChar () =
        let textRanges = TextRanges [|
            textRange (0, 10)
            textRange (0, 11)
            textRange (0, 11)

            textRange (0, 12)
            textRange (0, 13)
            textRange (0, 13)

            textRange (0, 14)
            textRange (0, 15)
            textRange (0, 15)
        |]

        let modifier = TextRangesModifier textRanges

        modifier.StartApplyingModifications ()

        modifier.ApplyOneLineInsert (oneLineInsertSpecChar (0, 11))
        modifier.ApplyOneLineInsert (oneLineInsertSpecChar (0, 11))
        modifier.ApplyOneLineInsert (oneLineInsertSpecChar (0, 10))

        modifier.ApplyOneLineInsert (oneLineInsertSpecChar (0, 16))
        modifier.ApplyOneLineInsert (oneLineInsertSpecChar (0, 16))
        modifier.ApplyOneLineInsert (oneLineInsertSpecChar (0, 15))

        modifier.ApplyOneLineInsert (oneLineInsertSpecChar (0, 21))
        modifier.ApplyOneLineInsert (oneLineInsertSpecChar (0, 21))
        modifier.ApplyOneLineInsert (oneLineInsertSpecChar (0, 20))

        assertTextRanges textRanges [|
            textRange (0, 11)
            textRange (0, 14)
            textRange (0, 14)

            textRange (0, 16)
            textRange (0, 19)
            textRange (0, 19)

            textRange (0, 21)
            textRange (0, 24)
            textRange (0, 24)
        |]

    // ApplyOneLineDelete ------------------------------------------------------

    [<Test>]
    member _.ApplyOneLineDelete_SingleChar () =
        let textRanges = TextRanges [|
            textRange (0, 11)
            textRange (0, 14)
            textRange (0, 14)

            textRange (0, 16)
            textRange (0, 19)
            textRange (0, 19)

            textRange (0, 21)
            textRange (0, 24)
            textRange (0, 24)
        |]

        let modifier = TextRangesModifier textRanges

        modifier.StartApplyingModifications ()

        modifier.ApplyOneLineDelete (oneLineDeleteSpecChar (0, 12))
        modifier.ApplyOneLineDelete (oneLineDeleteSpecChar (0, 12))
        modifier.ApplyOneLineDelete (oneLineDeleteSpecChar (0, 10))

        modifier.ApplyOneLineDelete (oneLineDeleteSpecChar (0, 14))
        modifier.ApplyOneLineDelete (oneLineDeleteSpecChar (0, 14))
        modifier.ApplyOneLineDelete (oneLineDeleteSpecChar (0, 12))

        modifier.ApplyOneLineDelete (oneLineDeleteSpecChar (0, 16))
        modifier.ApplyOneLineDelete (oneLineDeleteSpecChar (0, 16))
        modifier.ApplyOneLineDelete (oneLineDeleteSpecChar (0, 14))

        assertTextRanges textRanges [|
            textRange (0, 10)
            textRange (0, 11)
            textRange (0, 11)

            textRange (0, 12)
            textRange (0, 13)
            textRange (0, 13)

            textRange (0, 14)
            textRange (0, 15)
            textRange (0, 15)
        |]
