module CommandCompletionsTest

open NUnit.Framework

open Common
open CompletionItems
open DataTypes
open Position
open UserMessages

let contextRef = TestUtils.makeConsoleContextRef 80 25

[<TestFixture>]
type CommandCompletionsTest () =

    let myCompletions: ICompletionItems = new CompletionItems (
        contextRef, UserMessages (), CommandCompletion.getCompletions
    )

    // initialization

    let init lineStr cursor =
        myCompletions.TrySet [
            stringToChars lineStr, { line = 0; char = cursor }
        ]

    // simple assertions

    let assertResult
        (expected: (int * string) option)
        (result: CompletionAction option) =

        let expected' = expected |> Option.map (
            fun (toDelete, toInsert) -> {
                toDelete = toDelete
                toInsert = stringToChars toInsert
            }
        )

        Assert.AreEqual (expected', result)

    let assertInCompletion () =
        Assert.IsTrue  (myCompletions.IsInCompletion ())

    let assertNotInCompletion () =
        Assert.IsFalse (myCompletions.IsInCompletion ())

    // teardown

    [<TearDown>]
    member _.OneTimeTearDown () =
        myCompletions.Clear ()

    // GetNext, GetPrevious, IsInCompletion ------------------------------------

    [<Test>]
    member _.GetNext_GetPrevious_atEndOfLine () =
        init "m g" 3

        assertNotInCompletion ()

        assertResult (Some (1, "global")) (myCompletions.GetNext     ())

        assertInCompletion    ()

        assertResult (Some (6, "g"     )) (myCompletions.GetNext     ())
        assertResult (None              ) (myCompletions.GetNext     ())

        assertInCompletion    ()

        assertResult (Some (1, "global")) (myCompletions.GetPrevious ())

        assertInCompletion    ()

        assertResult (Some (6, "g"     )) (myCompletions.GetPrevious ())

        assertNotInCompletion ()

        assertResult (None              ) (myCompletions.GetPrevious ())

    [<Test>]
    member _.GetNext_GetPrevious_insideLine () =
        init "m g x" 3

        assertNotInCompletion ()

        assertResult (Some (1, "global")) (myCompletions.GetNext     ())

        assertInCompletion    ()

        assertResult (Some (6, "g"     )) (myCompletions.GetNext     ())
        assertResult (None              ) (myCompletions.GetNext     ())

        assertInCompletion    ()

        assertResult (Some (1, "global")) (myCompletions.GetPrevious ())

        assertInCompletion    ()

        assertResult (Some (6, "g"     )) (myCompletions.GetPrevious ())

        assertNotInCompletion ()

        assertResult (None              ) (myCompletions.GetPrevious ())

    [<Test>]
    member _.GetPrevious_without_GetNext () =
        init "m g" 3

        assertResult None (myCompletions.GetPrevious ())

        assertNotInCompletion ()

    [<Test>]
    member _.GetNext_atInvalidChar () =
        init "m" 0

        assertResult None (myCompletions.GetNext     ())

        assertNotInCompletion ()

    // GetCompletionsRow -------------------------------------------------------

    [<Test>]
    member _.GetCompletionsRow () =
        init "m g" 3

        myCompletions.GetNext () |> ignore

        let s, offset, length = myCompletions.GetCompletionsRow ()

        Assert.AreEqual ("global #key", s)
        Assert.AreEqual (0, offset)
        Assert.AreEqual (6, length)

        myCompletions.GetNext () |> ignore

        let s, offset, length = myCompletions.GetCompletionsRow ()

        Assert.AreEqual ("global #key", s)
        Assert.AreEqual (7, offset)
        Assert.AreEqual (4, length)

    // Clear -------------------------------------------------------------------

    [<Test>]
    member _.Clear () =
        init "m g" 3

        myCompletions.GetNext () |> ignore

        assertInCompletion    ()

        myCompletions.Clear ()

        assertNotInCompletion ()
