module PromptHistoryTest

open NUnit.Framework

open DataTypes
open PromptHistory

[<TestFixture>]
type PromptHistoryTest () =

    // basic operations, without overwrite -------------------------------------

    [<Test>]
    member _.GetPrevious_GetNext () =
        let history = new PromptHistory ()

        Assert.IsFalse  (history.IsCurrentFromHistory)

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false

        Assert.AreEqual (
            [
                stringToChars "a"
                stringToChars "b"
            ],
            history.Lines
        )

        Assert.IsFalse  (history.IsCurrentFromHistory)

        Assert.AreEqual (Some (stringToChars "b"), history.GetPrevious ())

        Assert.IsTrue   (history.IsCurrentFromHistory)

        Assert.AreEqual (Some (stringToChars "a"), history.GetPrevious ())
        Assert.AreEqual (None                    , history.GetPrevious ())

        Assert.IsTrue   (history.IsCurrentFromHistory)

        Assert.AreEqual (Some (stringToChars "b"), history.GetNext     ())

        Assert.IsTrue   (history.IsCurrentFromHistory)

        Assert.AreEqual (Some (Chars.Empty      ), history.GetNext     ())

        Assert.IsFalse  (history.IsCurrentFromHistory)

        Assert.AreEqual (None                    , history.GetNext     ())

        Assert.IsFalse  (history.IsCurrentFromHistory)

    // advanced operations, without overwrite ----------------------------------

    [<Test>]
    member _.WhenLeaving_NoOperation_NotHistoryLine () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.WhenLeaving (stringToChars "" ) false

        Assert.AreEqual (
            [
                stringToChars "a"
                stringToChars "b"
                stringToChars "c"
            ],
            history.Lines
        )

    [<Test>]
    member _.WhenLeaving_NoOperation_HistoryLine () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.GetPrevious () |> ignore
        history.GetPrevious () |> ignore
        history.WhenLeaving (stringToChars "" ) false

        Assert.AreEqual (
            [
                stringToChars "a"
                stringToChars "b"
                stringToChars "c"
            ],
            history.Lines
        )

    [<Test>]
    member _.WhenLeaving_Add_NotHistoryLine () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.WhenLeaving (stringToChars "x") false

        Assert.AreEqual (
            [
                stringToChars "a"
                stringToChars "b"
                stringToChars "c"
                stringToChars "x"
            ],
            history.Lines
        )

    [<Test>]
    member _.WhenLeaving_Add_NotHistoryLine_RemoveDuplicates () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.WhenLeaving (stringToChars "a") false

        Assert.AreEqual (
            [
                stringToChars "b"
                stringToChars "c"
                stringToChars "a"
            ],
            history.Lines
        )

    [<Test>]
    member _.WhenLeaving_Add_HistoryLine () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.GetPrevious () |> ignore
        history.GetPrevious () |> ignore
        history.WhenLeaving (stringToChars "x") false

        Assert.AreEqual (
            [
                stringToChars "a"
                stringToChars "b"
                stringToChars "c"
                stringToChars "x"
            ],
            history.Lines
        )

    [<Test>]
    member _.WhenLeaving_Add_HistoryLine_RemoveDuplicates () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.GetPrevious () |> ignore
        history.GetPrevious () |> ignore
        history.WhenLeaving (stringToChars "a") false

        Assert.AreEqual (
            [
                stringToChars "b"
                stringToChars "c"
                stringToChars "a"
            ],
            history.Lines
        )

    // advanced operations, with overwrite -------------------------------------

    [<Test>]
    member _.WhenLeaving_Remove_NotHistoryLine () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.WhenLeaving (stringToChars "" ) true

        Assert.AreEqual (
            [
                stringToChars "a"
                stringToChars "b"
                stringToChars "c"
            ],
            history.Lines
        )

    [<Test>]
    member _.WhenLeaving_Remove_HistoryLine () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.GetPrevious () |> ignore
        history.GetPrevious () |> ignore
        history.WhenLeaving (stringToChars "" ) true

        Assert.AreEqual (
            [
                stringToChars "a"
                stringToChars "c"
            ],
            history.Lines
        )

    [<Test>]
    member _.WhenLeaving_Update_NotHistoryLine () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.WhenLeaving (stringToChars "x") true

        Assert.AreEqual (
            [
                stringToChars "a"
                stringToChars "b"
                stringToChars "c"
                stringToChars "x"
            ],
            history.Lines
        )

    [<Test>]
    member _.WhenLeaving_Update_NotHistoryLine_RemoveDuplicates () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.WhenLeaving (stringToChars "a") true

        Assert.AreEqual (
            [
                stringToChars "b"
                stringToChars "c"
                stringToChars "a"
            ],
            history.Lines
        )

    [<Test>]
    member _.WhenLeaving_Update_HistoryLine () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.GetPrevious () |> ignore
        history.GetPrevious () |> ignore
        history.WhenLeaving (stringToChars "x") true

        Assert.AreEqual (
            [
                stringToChars "a"
                stringToChars "c"
                stringToChars "x"
            ],
            history.Lines
        )

    [<Test>]
    member _.WhenLeaving_Update_HistoryLine_RemoveDuplicates () =
        let history = new PromptHistory ()

        history.WhenLeaving (stringToChars "a") false
        history.WhenLeaving (stringToChars "b") false
        history.WhenLeaving (stringToChars "c") false

        history.GetPrevious () |> ignore
        history.GetPrevious () |> ignore
        history.WhenLeaving (stringToChars "a") true

        Assert.AreEqual (
            [
                stringToChars "c"
                stringToChars "a"
            ],
            history.Lines
        )
