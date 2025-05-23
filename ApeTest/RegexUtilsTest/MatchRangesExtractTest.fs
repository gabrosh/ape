module MatchRangesExtractTest

open NUnit.Framework

open DataTypes
open MatchRanges
open MatchRangesExtract
open Position
open TextRange
open UserMessages

let contextRef = TestUtils.makeContextRef 80 25

[<TestFixture>]
type MatchRangesExtractTest () =
    let myUserMessages = UserMessages ()

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

    // assertions

    let stringsToLines (ss: string seq) =
        ss |> Seq.map stringToChars |> Lines

    let assertLines (actual: Lines) (expected: string seq) =
        let expected' = stringsToLines expected
        Assert.AreEqual (expected', actual)

    let assertTextRanges (matchRanges: MatchRanges) (expected: TextRange seq) =
        let actual = matchRanges.GetAllFromMainGroup ()
        Assert.AreEqual (expected, actual)

    // CreateExtract, Search, ClearSearch, ReSearch ----------------------------

    [<Test>]
    member _.CreateExtract_Search_ClearSearch_ReSearch () =
        let lines =
            [|"abc"; "def"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract =
            [| |]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)

        let matchRangesExtract = matchRanges.CreateExtract MatchRangesExtract linesExtract
        matchRangesExtract.Init ()

        matchRangesExtract.Search "abc"

        assertLines linesExtract [|
            "abc"; "def"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]

        matchRangesExtract.ClearSearch ()

        assertLines linesExtract [|
            "abc"; "def"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
        |]

        matchRangesExtract.ReSearch ()

        assertLines linesExtract [|
            "abc"; "def"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]

    // Search, CreateExtract, Extract ------------------------------------------

    [<Test>]
    member _.Search_CreateExtract () =
        let lines =
            [|"abc"; "def"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract =
            [| |]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)

        matchRanges.Search "abc"

        let matchRangesExtract = matchRanges.CreateExtract MatchRangesExtract linesExtract
        matchRangesExtract.Init ()

        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

    [<Test>]
    member _.CreateExtract_Extract () =
        let lines =
            [|"abc"; "def"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract =
            [| |]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)

        let matchRangesExtract = matchRanges.CreateExtract MatchRangesExtract linesExtract
        matchRangesExtract.Init ()

        matchRangesExtract.Extract "abc"

        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

    // CreateExtract, Extract, ... ---------------------------------------------

    [<Test>]
    member _.CreateExtract_Extract_ClearSearch_ReSearch () =
        let lines =
            [|"abc"; "def"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract =
            [| |]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)

        let matchRangesExtract = matchRanges.CreateExtract MatchRangesExtract linesExtract
        matchRangesExtract.Init ()

        matchRangesExtract.Extract "abc"

        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.ClearSearch ()

        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
        |]

        matchRangesExtract.ReSearch ()

        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

    [<Test>]
    member _.CreateExtract_Extract_Search_ReExtract () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract =
            [| |]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)

        let matchRangesExtract = matchRanges.CreateExtract MatchRangesExtract linesExtract
        matchRangesExtract.Init ()

        matchRangesExtract.Extract "abc"

        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.Search "def"

        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 3) (1, 5)
        |]

        matchRangesExtract.ReExtract ()

        assertLines linesExtract [|
            "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 3) (1, 5)
        |]

    [<Test>]
    member _.CreateExtract_Extract_Search_ClearExtract () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract =
            [| |]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)

        let matchRangesExtract = matchRanges.CreateExtract MatchRangesExtract linesExtract
        matchRangesExtract.Init ()

        matchRangesExtract.Extract "abc"

        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.Search "def"

        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 3) (1, 5)
        |]

        matchRangesExtract.ClearExtract ()

        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
            textRange (2, 3) (2, 5)
        |]

    // UpdateAfterReload -------------------------------------------------------

    [<Test>]
    member _.CreateExtract_UpdateAfterReload () =
        let lines =
            [|"abc"; "abc"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract =
            [| |]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)

        let matchRangesExtract = matchRanges.CreateExtract MatchRangesExtract linesExtract
        matchRangesExtract.Init ()

        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
        |]

        matchRangesExtract.UpdateAfterReload ()

        assertTextRanges matchRangesExtract [|
        |]

    [<Test>]
    member _.CreateExtract_Extract_UpdateAfterReload () =
        let lines =
            [|"abc"; "abc"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract =
            [| |]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)

        let matchRangesExtract = matchRanges.CreateExtract MatchRangesExtract linesExtract
        matchRangesExtract.Init ()

        matchRangesExtract.Extract @"(?m)(?<=abc\n)abc"

        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.UpdateAfterReload ()

        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

    [<Test>]
    member _.CreateExtract_Extract_ReSearch_UpdateAfterReload () =
        let lines =
            [|"abc"; "abc"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract =
            [| |]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)

        let matchRangesExtract = matchRanges.CreateExtract MatchRangesExtract linesExtract
        matchRangesExtract.Init ()

        matchRangesExtract.Extract @"(?m)(?<=abc\n)abc"

        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.ReSearch ()

        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.UpdateAfterReload ()

        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
        |]
