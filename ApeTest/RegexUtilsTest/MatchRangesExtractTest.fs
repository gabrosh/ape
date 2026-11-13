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
        let actual =
            matchRanges.GetAllFromAllGroups ()
            |> Seq.map snd
            |> Seq.collect id            

        Assert.AreEqual (expected, actual)

    // two ways to create an extract -------------------------------------------

    [<Test>]
    member _.CreateExtract_Extract () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)

        matchRangesExtract.Extract "abc"
        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

    [<Test>]
    member _.Search_CreateExtract_ReExtract () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let matchRanges = MatchRanges (myUserMessages, lines)        
        matchRanges.Search "abc"
        
        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract, matchRanges)

        matchRangesExtract.ReExtract ()
        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]
        
    // Search memorizes regex for ReSearch -------------------------------------

    [<Test>]
    member _.ReSearch_after_Search_ClearSearch () =
        let lines =
            [|"abc"; "abc"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.Search @"(?m)(?<=abc\n)abc"
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
            textRange (2, 0) (2, 2)
        |]

        matchRangesExtract.ClearSearch ()
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.ReSearch ()
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
            textRange (2, 0) (2, 2)
        |]

    // Extract memorizes regex for ReSearch ------------------------------------

    [<Test>]
    member _.ReSearch_after_Extract_ClearSearch () =
        let lines =
            [|"abc"; "abc"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.Extract @"(?m)(?<=abc\n)abc"
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
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.ReSearch ()
        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
        |]

    // ReSearch without Search or Extract doesn't search -----------------------
    
    [<Test>]
    member _.ReSearch_without_Search_or_Extract () =
        let lines =
            [|"abc"; "abc"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.ReSearch ()
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [| |]

    // Search sets regex for ClearExtract and ReExtract ------------------------

    [<Test>]
    member _.ClearExtract_ReExtract_after_Search () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

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

        matchRangesExtract.ReExtract ()
        assertLines linesExtract [|
            "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 3) (1, 5)
        |]

    // Extract sets regex for ClearExtract and ReExtract -----------------------

    [<Test>]
    member _.ClearExtract_ReExtract_after_Extract () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.Search "abc"
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]

        matchRangesExtract.Extract "def"
        assertLines linesExtract [|
            "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
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

        matchRangesExtract.ReExtract ()
        assertLines linesExtract [|
            "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 3) (1, 5)
        |]

    // ClearExtract after Search and ClearSearch doesn't search ----------------
    
    [<Test>]
    member _.ClearExtract_after_Search_ClearSearch () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.Search "abc"
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]

        matchRangesExtract.ClearSearch ()
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.ClearExtract ()
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

    // ReExtract after Search and ClearSearch doesn't search -------------------
    
    [<Test>]
    member _.ReExtract_after_Search_ClearSearch () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.Search "abc"
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
        |]

        matchRangesExtract.ClearSearch ()
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.ReExtract ()
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

    // ClearExtract after Extract and ClearSearch doesn't search ---------------
    
    [<Test>]
    member _.ClearExtract_after_Extract_ClearSearch () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.Extract "abc"
        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.ClearSearch ()
        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.ClearExtract ()
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

    // ReExtract after Extract and ClearSearch doesn't search ------------------
    
    [<Test>]
    member _.ReExtract_after_Extract_ClearSearch () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.Extract "abc"
        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.ClearSearch ()
        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.ReExtract ()
        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]

    // Secondary search --------------------------------------------------------
    
    [<Test>]
    member _.Secondary_Search () =
        let lines =
            [|"abc"; "def"; "abcdef"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [| |]
        
        matchRangesExtract.Extract "abc"
        assertLines linesExtract [|
            "abc"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]
        
        matchRangesExtract.ToggleSndSearch ()
        
        matchRangesExtract.Extract "def"
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (2, 0) (2, 2)
            textRange (1, 0) (1, 2)
            textRange (2, 3) (2, 5)
        |]
                       
        matchRangesExtract.ToggleSndSearch ()

        matchRangesExtract.ClearSearch ()
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
            textRange (2, 3) (2, 5)
        |]

        matchRangesExtract.ToggleSndSearch ()

        matchRangesExtract.ClearSearch ()
        assertLines linesExtract [|
            "abc"; "def"; "abcdef"
        |]
        assertTextRanges matchRangesExtract [|
        |]

    // UpdateAfterChange -------------------------------------------------------

    [<Test>]
    member _.UpdateAfterChange_without_Search_or_Extract () =
        let lines =
            [|"abc"; "abc"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [| |]

        matchRangesExtract.UpdateAfterChange ()
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [| |]

    [<Test>]
    member _.UpdateAfterChange_after_Search () =
        let lines =
            [|"abc"; "abc"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)

        matchRangesExtract.Search @"(?m)(?<=abc\n)abc"
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
            textRange (2, 0) (2, 2)
        |]

        matchRangesExtract.UpdateAfterChange ()
        assertLines linesExtract [|
            "abc"; "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
            textRange (2, 0) (2, 2)
        |]

    [<Test>]
    member _.UpdateAfterChange_after_Extract () =
        let lines =
            [|"abc"; "abc"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)

        matchRangesExtract.Extract @"(?m)(?<=abc\n)abc"
        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.UpdateAfterChange ()
        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

    [<Test>]
    member _.UpdateAfterChange_after_Extract_Search () =
        let lines =
            [|"abc"; "abc"; "abc"|]
            |> Seq.map stringToChars
            |> Lines

        let linesExtract = Lines []
        let matchRangesExtract = MatchRangesExtract (myUserMessages, lines, linesExtract)

        matchRangesExtract.Extract @"(?m)(?<=abc\n)abc"
        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (0, 0) (0, 2)
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.Search @"(?m)(?<=abc\n)abc"
        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
        |]

        matchRangesExtract.UpdateAfterChange ()
        assertLines linesExtract [|
            "abc"; "abc"
        |]
        assertTextRanges matchRangesExtract [|
            textRange (1, 0) (1, 2)
        |]
