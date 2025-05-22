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

    let assertTextRanges (matchRanges: MatchRanges) (expected: TextRange array) =

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

        matchRanges.Search "abc"

        assertTextRanges matchRanges [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]

        matchRanges.ClearSearch ()

        assertTextRanges matchRanges [|
        |]

        matchRanges.ReSearch ()

        assertTextRanges matchRanges [|
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

        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.ClearSearch ()

        assertTextRanges matchRanges [|
        |]

        matchRangesExtract.ReSearch ()

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

        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.Search "def"

        assertTextRanges matchRangesExtract [|
            textRange (1, 3) (1, 5)
        |]

        matchRangesExtract.ReExtract ()

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

        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.Search "def"

        assertTextRanges matchRangesExtract [|
            textRange (1, 3) (1, 5)
        |]

        matchRangesExtract.ClearExtract ()

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

        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.UpdateAfterReload ()

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

        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.ReSearch ()

        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.UpdateAfterReload ()

        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
        |]
