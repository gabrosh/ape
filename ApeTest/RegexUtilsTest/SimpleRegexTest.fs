module SimpleRegexTest

open NUnit.Framework
open System.Collections.Generic
open System.Text.RegularExpressions

open DataTypes
open Position

[<TestFixture>]
type SimpleRegexTest () =

    // AddMatchesAsStringsSet

    [<Test>]
    member _.AddMatchesAsStringsSet_NoMatch () =
        // arrange

        let stringsSet = HashSet<string> ()
        let toSkip = HashSet<Position> ()

        let slr = SimpleRegex.AddMatchesAsStringsSet (
            Regex @"\w+", toSkip
        )
        slr.Init stringsSet

        // act

        let _ = slr.ProcessLine 0 (stringToChars "...   ")
        let _ = slr.ProcessLine 0 (stringToChars "   ...")

        slr.FinishProcessing ()

        // assert

        let expStringSet = HashSet<string> []

        Assert.AreEqual (expStringSet, stringsSet)

    [<Test>]
    member _.AddMatchesAsStringsSet_TwoMatches () =
        // arrange

        let stringsSet = HashSet<string> ()
        let toSkip = HashSet<Position> ()

        let slr = SimpleRegex.AddMatchesAsStringsSet (
            Regex @"\w+", toSkip
        )
        slr.Init stringsSet

        // act

        let _ = slr.ProcessLine 0 (stringToChars "...abc")
        let _ = slr.ProcessLine 0 (stringToChars "def...")

        slr.FinishProcessing ()

        // assert

        let expStringSet = HashSet<string> ["abc"; "def"]

        Assert.AreEqual (expStringSet, stringsSet)

    [<Test>]
    member _.AddMatchesAsStringsSet_NoMatchForNewLine () =
        // arrange

        let stringsSet = HashSet<string> ()
        let toSkip = HashSet<Position> ()

        let slr = SimpleRegex.AddMatchesAsStringsSet (
            Regex @"\n", toSkip
        )
        slr.Init stringsSet

        // act

        let _ = slr.ProcessLine 0 (stringToChars "...abc")
        let _ = slr.ProcessLine 0 (stringToChars "def...")

        slr.FinishProcessing ()

        // assert

        let expStringSet = HashSet<string> []

        Assert.AreEqual (expStringSet, stringsSet)
