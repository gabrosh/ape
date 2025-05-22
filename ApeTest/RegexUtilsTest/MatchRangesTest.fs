module SearchTest

open NUnit.Framework

open DataTypes
open MatchRanges
open Position
open TextRange
open UserMessages

let contextRef = TestUtils.makeContextRef 80 25

[<TestFixture>]
type MatchRangesTest () =
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

    // Search, ClearSearch, ReSearch -------------------------------------------

    [<Test>]
    member _.Search_ClearSearch_ReSearch () =
        let lines =
            [|"abc"; "def"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)

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
