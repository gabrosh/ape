module TextRangesTest

open NUnit.Framework

open Position
open TextRange
open TextRanges

[<TestFixture>]
type TextRangesTest () =

    let position line char =
        {
            line = line
            char = char
        }

    let textRange (firstLine, firstChar) (lastLine, lastChar) =
        {
            first = position firstLine firstChar
            last  = position lastLine  lastChar
        }

    let assertTextRanges textRanges startLine endLine expRanges =
        let ranges = getFromInterval textRanges startLine endLine

        Assert.AreEqual (expRanges, ranges)

    // getFromInterval, count check --------------------------------------------

    [<TestCase(0  , 9  , 0)>]
    [<TestCase(11 , 20 , 0)>]
    [<TestCase(0  , 10 , 2)>]
    [<TestCase(10 , 20 , 2)>]
    member _.getFromInterval_Count_Separated_OneLine startLine endLine expCount =
        let textRanges = TextRanges [
            textRange (10, 10) (10, 20)
            textRange (10, 30) (10, 40)
        ]

        let ranges = getFromInterval textRanges startLine endLine

        Assert.AreEqual (expCount, ranges.Count)

    [<TestCase(0  , 9  , 0)>]
    [<TestCase(21 , 29 , 0)>]
    [<TestCase(41 , 50 , 0)>]
    [<TestCase(0  , 10 , 1)>]
    [<TestCase(40 , 50 , 1)>]
    [<TestCase(20 , 30 , 2)>]
    member _.getFromInterval_Count_Separated_MultipleLines startLine endLine expCount =
        let textRanges = TextRanges [
            textRange (10, 0) (20, 0)
            textRange (30, 0) (40, 0)
        ]

        let ranges = getFromInterval textRanges startLine endLine

        Assert.AreEqual (expCount, ranges.Count)

    // getFromInterval, each selection on one line -----------------------------

    [<Test>]
    member _.getFromInterval_Separated_OneLine () =
        let textRanges = TextRanges [
            textRange (0, 10) (0, 20)
            textRange (0, 22) (0, 30)
        ]

        assertTextRanges textRanges 0 0 [
            textRange (0, 10) (0, 20)
            textRange (0, 22) (0, 30)
        ]

    [<Test>]
    member _.getFromInterval_Touching_OneLine () =
        let textRanges = TextRanges [
            textRange (0, 10) (0, 20)
            textRange (0, 21) (0, 30)
        ]

        assertTextRanges textRanges 0 0 [
            textRange (0, 10) (0, 20)
            textRange (0, 21) (0, 30)
        ]

    // getFromInterval, each selection on multiple lines -----------------------

    [<Test>]
    member _.getFromInterval_Separated_MultipleLines () =
        let textRanges = TextRanges [
            textRange (10, 0 ) (20, 40)
            textRange (20, 42) (30, 0 )
        ]

        assertTextRanges textRanges 0 99 [
            textRange (10, 0 ) (20, 40)
            textRange (20, 42) (30, 0 )
        ]

    [<Test>]
    member _.getFromInterval_Touching_MultipleLines () =
        let textRanges = TextRanges [
            textRange (10, 0 ) (20, 40)
            textRange (20, 41) (30, 0 )
        ]

        assertTextRanges textRanges 0 99 [
            textRange (10, 0 ) (20, 40)
            textRange (20, 41) (30, 0 )
        ]
