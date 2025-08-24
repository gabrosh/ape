module LineMatchClassifierTest

open NUnit.Framework

open LineMatchClassifier
open Position
open TextRange
open TextRanges

[<TestFixture>]
type LineMatchClassifierTest () =

    let position line char =
        {
            line = line
            char = char
        }

    let match_ (firstLine, firstChar) (lastLine, lastChar) =
        {
            first = position firstLine firstChar
            last  = position lastLine  lastChar
        }

    // IsInMatch -----------------------------------------------------------

    [<Test>]
    member _.IsInMatch_FromStart () =
        let matchRanges = [|
            ColorSchemes.mainGroupColor, TextRanges [
                match_ (2, 0) (3, 0)
                match_ (6, 0) (7, 0)
            ]
        |]

        Assert.AreEqual (2, (snd matchRanges[0]).Count)

        let pc = LineMatchClassifier matchRanges

        Assert.AreEqual (None  , pc.IsInMatch 0)
        Assert.AreEqual (None  , pc.IsInMatch 1)
        Assert.AreEqual (Some 0, pc.IsInMatch 2)
        Assert.AreEqual (Some 0, pc.IsInMatch 3)
        Assert.AreEqual (None  , pc.IsInMatch 4)
        Assert.AreEqual (None  , pc.IsInMatch 5)
        Assert.AreEqual (Some 0, pc.IsInMatch 6)
        Assert.AreEqual (Some 0, pc.IsInMatch 7)
        Assert.AreEqual (None  , pc.IsInMatch 8)
        Assert.AreEqual (None  , pc.IsInMatch 9)

    [<Test>]
    member _.IsInMatch_FromMiddle_False () =
        let matchRanges = [|
            ColorSchemes.mainGroupColor, TextRanges [
                match_ (2, 0) (3, 0)
                match_ (6, 0) (7, 0)
            ]
        |]

        Assert.AreEqual (2, (snd matchRanges[0]).Count)

        let pc = LineMatchClassifier matchRanges

        Assert.AreEqual (None  , pc.IsInMatch 5)
        Assert.AreEqual (Some 0, pc.IsInMatch 6)

    [<Test>]
    member _.IsInMatch_FromMiddle_True () =
        let matchRanges = [|
            ColorSchemes.mainGroupColor, TextRanges [
                match_ (2, 0) (3, 0)
                match_ (6, 0) (7, 0)
            ]
        |]

        Assert.AreEqual (2, (snd matchRanges[0]).Count)

        let pc = LineMatchClassifier matchRanges

        Assert.AreEqual (Some 0, pc.IsInMatch 7)
        Assert.AreEqual (None  , pc.IsInMatch 8)
