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

    let assertTextRanges (matchRanges: MatchRanges) (expected: TextRange seq) =
        let actual =
            matchRanges.GetAllFromAllGroups ()
            |> Seq.map snd
            |> Seq.collect id            

        Assert.AreEqual (expected, actual)

    // ReSearch without Search has no effect -----------------------------------
    
    [<Test>]
    member _.ReSearch_without_Search_or_Extract () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)
        assertTextRanges matchRanges [| |]

        matchRanges.ReSearch ()
        assertTextRanges matchRanges [| |]

    // Search memorizes regex for ReSearch -------------------------------------

    [<Test>]
    member _.ReSearch_after_Search_ClearSearch () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)
        assertTextRanges matchRanges [| |]
        
        matchRanges.Search "abc"
        assertTextRanges matchRanges [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]

        matchRanges.ClearSearch ()
        assertTextRanges matchRanges [| |]

        matchRanges.ReSearch ()
        assertTextRanges matchRanges [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]

    // Secondary search --------------------------------------------------------
    
    [<Test>]
    member _.Secondary_Search () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)
        assertTextRanges matchRanges [| |]
        
        matchRanges.Search "abc"
        assertTextRanges matchRanges [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]
        
        matchRanges.ToggleSndSearch ()
        
        matchRanges.Search "def"
        assertTextRanges matchRanges [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
            textRange (1, 0) (1, 2)
            textRange (2, 3) (2, 5)
        |]
               
        matchRanges.ToggleSndSearch ()

        matchRanges.ClearSearch ()
        assertTextRanges matchRanges [|
            textRange (1, 0) (1, 2)
            textRange (2, 3) (2, 5)
        |]

        matchRanges.ToggleSndSearch ()

        matchRanges.ClearSearch ()
        assertTextRanges matchRanges [|
        |]

    // UpdateAfterChange -------------------------------------------------------

    [<Test>]
    member _.UpdateAfterChange_without_Search () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)
        assertTextRanges matchRanges [| |]

        matchRanges.UpdateAfterChange ()
        assertTextRanges matchRanges [| |]

    [<Test>]
    member _.UpdateAfterChange_after_Search () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)
        assertTextRanges matchRanges [| |]

        matchRanges.Search "abc"
        assertTextRanges matchRanges [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]

        matchRanges.UpdateAfterChange ()
        assertTextRanges matchRanges [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]
