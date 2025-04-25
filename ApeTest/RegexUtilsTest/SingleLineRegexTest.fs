module SingleLinesRegexTest

open NUnit.Framework
open System.Text.RegularExpressions

open DataTypes
open Selection
open TextRange
open TextRanges
open UserMessages

[<TestFixture>]
type SingleLinesRegexTest () =

    // AddMatchesAsTextRanges

    [<Test>]
    member _.AddMatchesAsTextRanges_NoMatch () =
        // arrange

        let textRanges = makeTextRangesGroups ()

        let slr = SingleLineRegex.AddMatchesAsTextRanges (
            Regex @"x\n?"
        )
        slr.Init textRanges

        // act

        let _ = slr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = slr.ProcessLine 11 0 3 false (stringToChars "def345")

        slr.FinishProcessing ()

        // assert

        let expTextRanges = []

        Assert.AreEqual (expTextRanges, textRanges[mainGroupName])

    [<Test>]
    member _.AddMatchesAsTextRanges_TwoMatches () =
        // arrange

        let textRanges = makeTextRangesGroups ()

        let slr = SingleLineRegex.AddMatchesAsTextRanges (
            Regex @".+\n?"
        )
        slr.Init textRanges

        // act

        let _ = slr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = slr.ProcessLine 11 0 3 false (stringToChars "def345")

        slr.FinishProcessing ()

        // assert

        let expTextRanges = [
            {
                first = { line = 10; char = 3 }
                last  = { line = 10; char = 6 }
            }
            {
                first = { line = 11; char = 0 }
                last  = { line = 11; char = 2 }
            }
        ]

        Assert.AreEqual (expTextRanges, textRanges[mainGroupName])

    [<Test>]
    member _.AddMatchesAsTextRanges_WithoutEndNewLine () =
        // arrange

        let textRanges = makeTextRangesGroups ()

        let slr = SingleLineRegex.AddMatchesAsTextRanges (
            Regex @"def\n"
        )
        slr.Init textRanges

        // act

        let _ = slr.ProcessLine 10 0 3 true  (stringToChars "abc")
        let _ = slr.ProcessLine 11 0 3 false (stringToChars "def")

        slr.FinishProcessing ()

        // assert

        let expTextRanges = []

        Assert.AreEqual (expTextRanges, textRanges[mainGroupName])

    [<Test>]
    member _.AddMatchesAsTextRanges_WithEndNewLine () =
        // arrange

        let textRanges = makeTextRangesGroups ()

        let slr = SingleLineRegex.AddMatchesAsTextRanges (
            Regex @"def\n"
        )
        slr.Init textRanges

        // act

        let _ = slr.ProcessLine 10 0 3 true (stringToChars "abc")
        let _ = slr.ProcessLine 11 0 3 true (stringToChars "def")

        slr.FinishProcessing ()

        // assert

        let expTextRanges = [
            {
                first = { line = 11; char = 0 }
                last  = { line = 11; char = 3 }
            }
        ]

        Assert.AreEqual (expTextRanges, textRanges[mainGroupName])

    [<Test>]
    member _.AddMatchesAsTextRanges_WithGroups () =
        // arrange

        let textRanges = makeTextRangesGroups ()

        let slr = SingleLineRegex.AddMatchesAsTextRanges (
            Regex @".*(?'x'a(?'y'b)c).*"
        )
        slr.Init textRanges

        // act

        let _ = slr.ProcessLine 10 0 6 true  (stringToChars "012abc")
        let _ = slr.ProcessLine 11 0 6 false (stringToChars "abc345")

        slr.FinishProcessing ()

        // assert

        let expTextRanges = [
            {
                first = { line = 10; char = 0 }
                last  = { line = 10; char = 5 }
            }
            {
                first = { line = 11; char = 0 }
                last  = { line = 11; char = 5 }
            }
        ]

        let expTextRanges_x = [
            {
                first = { line = 10; char = 3 }
                last  = { line = 10; char = 5 }
            }
            {
                first = { line = 11; char = 0 }
                last  = { line = 11; char = 2 }
            }
        ]

        let expTextRanges_y = [
            {
                first = { line = 10; char = 4 }
                last  = { line = 10; char = 4 }
            }
            {
                first = { line = 11; char = 1 }
                last  = { line = 11; char = 1 }
            }
        ]

        Assert.AreEqual (expTextRanges  , textRanges[mainGroupName])
        Assert.AreEqual (expTextRanges_x, textRanges["x"])
        Assert.AreEqual (expTextRanges_y, textRanges["y"])

    // AddMatchesAsSelections

    [<Test>]
    member _.AddMatchesAsSelections_NoMatch () =
        // arrange

        let selections = ResizeArray<Selection> ()

        let slr = SingleLineRegex.AddMatchesAsSelections (
            Regex @"x\n?"
        )
        slr.Init true selections

        // act

        let _ = slr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = slr.ProcessLine 11 0 3 false (stringToChars "def345")

        slr.FinishProcessing ()

        // assert

        let expSelections = []

        Assert.AreEqual (expSelections, selections)

    [<Test>]
    member _.AddMatchesAsSelections_TwoMatches () =
        // arrange

        let selections = ResizeArray<Selection> ()

        let slr = SingleLineRegex.AddMatchesAsSelections (
            Regex @".+\n?"
        )
        slr.Init true selections

        // act

        let _ = slr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = slr.ProcessLine 11 0 3 false (stringToChars "def345")

        slr.FinishProcessing ()

        // assert

        let expSelections = [
            {
                first     = { line = 10; char = 3 }
                last      = { line = 10; char = 6 }
                firstWC   = WantedColumns_Default
                lastWC    = WantedColumns_Default
                isForward = true
            }
            {
                first     = { line = 11; char = 0  }
                last      = { line = 11; char = 2 }
                firstWC   = WantedColumns_Default
                lastWC    = WantedColumns_Default
                isForward = true
            }
        ]

        Assert.AreEqual (expSelections, selections)

    [<Test>]
    member _.AddMatchesAsSelections_WithoutEndNewLine () =
        // arrange

        let selections = ResizeArray<Selection> ()

        let slr = SingleLineRegex.AddMatchesAsSelections (
            Regex @"def\n"
        )
        slr.Init true selections

        // act

        let _ = slr.ProcessLine 10 0 3 true  (stringToChars "abc")
        let _ = slr.ProcessLine 11 0 3 false (stringToChars "def")

        slr.FinishProcessing ()

        // assert

        let expSelections = []

        Assert.AreEqual (expSelections, selections)

    [<Test>]
    member _.AddMatchesAsSelections_WithEndNewLine () =
        // arrange

        let selections = ResizeArray<Selection> ()

        let slr = SingleLineRegex.AddMatchesAsSelections (
            Regex @"def\n"
        )
        slr.Init true selections

        // act

        let _ = slr.ProcessLine 10 0 3 true (stringToChars "abc")
        let _ = slr.ProcessLine 11 0 3 true (stringToChars "def")

        slr.FinishProcessing ()

        // assert

        let expSelections = [
            {
                first     = { line = 11; char = 0 }
                last      = { line = 11; char = 3 }
                firstWC   = WantedColumns_Default
                lastWC    = WantedColumns_Default
                isForward = true
            }
        ]

        Assert.AreEqual (expSelections, selections)

    // IsMatch

    [<Test>]
    member _.IsMatch_NoMatch () =
        // arrange

        let slr = SingleLineRegex.IsMatch (
            Regex @"x\n?"
        )
        slr.Init ()

        // act

        let _ = slr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = slr.ProcessLine 11 0 3 false (stringToChars "def345")

        slr.FinishProcessing ()

        // assert

        Assert.AreEqual (false, slr.IsMatch)

    [<Test>]
    member _.IsMatch_TwoMatches () =
        // arrange

        let slr = SingleLineRegex.IsMatch (
            Regex @".+\n?"
        )
        slr.Init ()

        // act

        let _ = slr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = slr.ProcessLine 11 0 3 false (stringToChars "def456")

        slr.FinishProcessing ()

        // assert

        Assert.AreEqual (true, slr.IsMatch)

    [<Test>]
    member _.IsMatch_WithoutEndNewLine () =
        // arrange

        let slr = SingleLineRegex.IsMatch (
            Regex @"def\n"
        )
        slr.Init ()

        // act

        let _ = slr.ProcessLine 10 0 3 true  (stringToChars "abc")
        let _ = slr.ProcessLine 11 0 3 false (stringToChars "def")

        slr.FinishProcessing ()

        // assert

        Assert.AreEqual (false, slr.IsMatch)

    [<Test>]
    member _.IsMatch_WithEndNewLine () =
        // arrange

        let slr = SingleLineRegex.IsMatch (
            Regex @"def\n"
        )
        slr.Init ()

        // act

        let _ = slr.ProcessLine 10 0 3 true (stringToChars "abc")
        let _ = slr.ProcessLine 11 0 3 true (stringToChars "def")

        slr.FinishProcessing ()

        // assert

        Assert.AreEqual (true, slr.IsMatch)
