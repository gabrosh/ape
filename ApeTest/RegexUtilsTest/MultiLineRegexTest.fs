module MultiLinesRegexTest

open NUnit.Framework
open System.Text.RegularExpressions

open DataTypes
open Selection
open TextRange
open TextRanges
open UserMessages

[<TestFixture>]
type MultiLinesRegexTest () =

    // AddMatchesAsTextRanges

    [<Test>]
    member _.AddMatchesAsTextRanges_NoMatch () =
        // arrange

        let textRanges = makeTextRangesGroups ()

        let mlr = MultiLineRegex.AddMatchesAsTextRanges (
            UserMessages (), Regex @"(?m)x\nx", textRanges
        )

        // act

        let _ = mlr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = mlr.ProcessLine 11 0 3 true  (stringToChars "def")
        let _ = mlr.ProcessLine 12 0 3 true  (stringToChars "ghi")
        let _ = mlr.ProcessLine 13 0 3 false (stringToChars "jkl345")

        mlr.FinishProcessing ()

        // assert

        let expTextRanges = []

        Assert.AreEqual (expTextRanges, textRanges[mainGroupName])

    [<Test>]
    member _.AddMatchesAsTextRanges_TwoMatches () =
        // arrange

        let textRanges = makeTextRangesGroups ()

        let mlr = MultiLineRegex.AddMatchesAsTextRanges (
            UserMessages (), Regex @"(?m).+\n.+", textRanges
        )

        // act

        let _ = mlr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = mlr.ProcessLine 11 0 3 true  (stringToChars "def")
        let _ = mlr.ProcessLine 12 0 3 true  (stringToChars "ghi")
        let _ = mlr.ProcessLine 13 0 3 false (stringToChars "jkl345")

        mlr.FinishProcessing ()

        // assert

        let expTextRanges = [
            {
                first = { line = 10; char = 3 }
                last  = { line = 11; char = 2 }
            }
            {
                first = { line = 12; char = 0 }
                last  = { line = 13; char = 2 }
            }
        ]

        Assert.AreEqual (expTextRanges, textRanges[mainGroupName])

    [<Test>]
    member _.AddMatchesAsTextRanges_WithoutEndNewLine () =
        // arrange

        let textRanges = makeTextRangesGroups ()

        let mlr = MultiLineRegex.AddMatchesAsTextRanges (
            UserMessages (), Regex @"(?m)def\n", textRanges
        )

        // act

        let _ = mlr.ProcessLine 10 0 3 true  (stringToChars "abc")
        let _ = mlr.ProcessLine 11 0 3 false (stringToChars "def")

        mlr.FinishProcessing ()

        // assert

        let expTextRanges = []

        Assert.AreEqual (expTextRanges, textRanges[mainGroupName])

    [<Test>]
    member _.AddMatchesAsTextRanges_WithEndNewLine () =
        // arrange

        let textRanges = makeTextRangesGroups ()

        let mlr = MultiLineRegex.AddMatchesAsTextRanges (
            UserMessages (), Regex @"(?m)def\n", textRanges
        )

        // act

        let _ = mlr.ProcessLine 10 0 3 true (stringToChars "abc")
        let _ = mlr.ProcessLine 11 0 3 true (stringToChars "def")

        mlr.FinishProcessing ()

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

        let mlr = MultiLineRegex.AddMatchesAsTextRanges (
            UserMessages (), Regex @"(?m).*(?'x'a(?'y'b)c).*", textRanges
        )

        // act

        let _ = mlr.ProcessLine 10 0 6 true  (stringToChars "012abc")
        let _ = mlr.ProcessLine 11 0 3 true  (stringToChars "def")
        let _ = mlr.ProcessLine 12 0 3 true  (stringToChars "ghi")
        let _ = mlr.ProcessLine 13 0 6 false (stringToChars "abc345")

        mlr.FinishProcessing ()

        // assert

        let expTextRanges = [
            {
                first = { line = 10; char = 0 }
                last  = { line = 10; char = 5 }
            }
            {
                first = { line = 13; char = 0 }
                last  = { line = 13; char = 5 }
            }
        ]

        let expTextRanges_x = [
            {
                first = { line = 10; char = 3 }
                last  = { line = 10; char = 5 }
            }
            {
                first = { line = 13; char = 0 }
                last  = { line = 13; char = 2 }
            }
        ]

        let expTextRanges_y = [
            {
                first = { line = 10; char = 4 }
                last  = { line = 10; char = 4 }
            }
            {
                first = { line = 13; char = 1 }
                last  = { line = 13; char = 1 }
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

        let mlr = MultiLineRegex.AddMatchesAsSelections (
            UserMessages (), Regex @"(?m)x\nx", selections, true
        )

        // act

        let _ = mlr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = mlr.ProcessLine 11 0 3 true  (stringToChars "def")
        let _ = mlr.ProcessLine 12 0 3 true  (stringToChars "ghi")
        let _ = mlr.ProcessLine 13 0 3 false (stringToChars "jkl345")

        mlr.FinishProcessing ()

        // assert

        let expSelections = []

        Assert.AreEqual (expSelections, selections)

    [<Test>]
    member _.AddMatchesAsSelections_TwoMatches () =
        // arrange

        let selections = ResizeArray<Selection> ()

        let mlr = MultiLineRegex.AddMatchesAsSelections (
            UserMessages (), Regex @"(?m).+\n.+", selections, true
        )

        // act

        let _ = mlr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = mlr.ProcessLine 11 0 3 true  (stringToChars "def")
        let _ = mlr.ProcessLine 12 0 3 true  (stringToChars "ghi")
        let _ = mlr.ProcessLine 13 0 3 false (stringToChars "jkl345")

        mlr.FinishProcessing ()

        // assert

        let expSelections = [
            {
                first     = { line = 10; char = 3 }
                last      = { line = 11; char = 2 }
                firstWC   = WantedColumns_Default
                lastWC    = WantedColumns_Default
                isForward = true
            }
            {
                first     = { line = 12; char = 0 }
                last      = { line = 13; char = 2 }
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

        let mlr = MultiLineRegex.AddMatchesAsSelections (
            UserMessages (), Regex @"(?m)def\n", selections, true
        )

        // act

        let _ = mlr.ProcessLine 10 0 3 true  (stringToChars "abc")
        let _ = mlr.ProcessLine 11 0 3 false (stringToChars "def")

        mlr.FinishProcessing ()

        // assert

        let expSelections = []

        Assert.AreEqual (expSelections, selections)

    [<Test>]
    member _.AddMatchesAsSelections_WithEndNewLine () =
        // arrange

        let selections = ResizeArray<Selection> ()

        let mlr = MultiLineRegex.AddMatchesAsSelections (
            UserMessages (), Regex @"(?m)def\n", selections, true
        )

        // act

        let _ = mlr.ProcessLine 10 0 3 true (stringToChars "abc")
        let _ = mlr.ProcessLine 11 0 3 true (stringToChars "def")

        mlr.FinishProcessing ()

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

        let mlr = MultiLineRegex.IsMatch (UserMessages (), Regex @"(?m)x\nx")

        // act

        let _ = mlr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = mlr.ProcessLine 11 0 3 true  (stringToChars "def")
        let _ = mlr.ProcessLine 12 0 3 true  (stringToChars "ghi")
        let _ = mlr.ProcessLine 13 0 3 false (stringToChars "jkl345")

        mlr.FinishProcessing ()

        // assert

        Assert.AreEqual (false, mlr.IsMatch)

    [<Test>]
    member _.IsMatch_TwoMatches () =
        // arrange

        let mlr = MultiLineRegex.IsMatch (UserMessages (), Regex @"(?m).+\n.+")

        // act

        let _ = mlr.ProcessLine 10 3 6 true  (stringToChars "012abc")
        let _ = mlr.ProcessLine 11 0 3 true  (stringToChars "def")
        let _ = mlr.ProcessLine 12 0 3 true  (stringToChars "ghi")
        let _ = mlr.ProcessLine 13 0 3 false (stringToChars "jkl345")

        mlr.FinishProcessing ()

        // assert

        Assert.AreEqual (true, mlr.IsMatch)

    [<Test>]
    member _.IsMatch_WithoutEndNewLine () =
        // arrange

        let mlr = MultiLineRegex.IsMatch (UserMessages (), Regex @"(?m)def\n")

        // act

        let _ = mlr.ProcessLine 10 0 3 true  (stringToChars "abc")
        let _ = mlr.ProcessLine 11 0 3 false (stringToChars "def")

        mlr.FinishProcessing ()

        // assert

        Assert.AreEqual (false, mlr.IsMatch)

    [<Test>]
    member _.IsMatch_WithEndNewLine () =
        // arrange

        let mlr = MultiLineRegex.IsMatch (UserMessages (), Regex @"(?m)def\n")

        // act

        let _ = mlr.ProcessLine 10 0 3 true (stringToChars "abc")
        let _ = mlr.ProcessLine 11 0 3 true (stringToChars "def")

        mlr.FinishProcessing ()

        // assert

        Assert.AreEqual (true, mlr.IsMatch)
