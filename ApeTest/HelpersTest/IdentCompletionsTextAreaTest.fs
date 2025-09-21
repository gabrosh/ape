module IdentCompletionsTextAreaTest

open NUnit.Framework

open Common
open CompletionItems
open DataTypes
open Position
open UserMessages

let contextRef = TestUtils.makeConsoleContextRef 80 25

[<TestFixture>]
type IdentCompletionsTextAreaTest () =

    let mutable myLines = ResizeArray<Chars> ()

    let myCompletions: ICompletionItems = new CompletionItems (
        contextRef, UserMessages (),
        IdentCompletion.getCompletions false (fun () -> myLines)
    )

    // initialization

    let init lineStr cursor =
        myLines <- ResizeArray [
            stringToChars lineStr
            stringToChars "true"
        ]
        myCompletions.TrySet [
            stringToChars lineStr, cursor
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
    member _.TearDown () =
        myCompletions.Clear ()

    // GetNext, GetPrevious, IsInCompletion ------------------------------------

    [<Test>]
    member _.GetNext_GetPrevious_atEndOfLine () =
        init "e t" { line = 0; char = 3 }

        assertNotInCompletion ()

        assertResult (Some (1, "true")) (myCompletions.GetNext     ())

        assertInCompletion    ()

        assertResult (None            ) (myCompletions.GetNext     ())

        assertInCompletion    ()

        assertResult (Some (4, "t"   )) (myCompletions.GetPrevious ())

        assertNotInCompletion ()

        assertResult (None            ) (myCompletions.GetPrevious ())

    [<Test>]
    member _.GetNext_GetPrevious_insideLine () =
        init "e t x" { line = 0; char = 3 }

        assertNotInCompletion ()

        assertResult (Some (1, "true")) (myCompletions.GetNext     ())

        assertInCompletion    ()

        assertResult (None            ) (myCompletions.GetNext     ())

        assertInCompletion    ()

        assertResult (Some (4, "t"   )) (myCompletions.GetPrevious ())

        assertNotInCompletion ()

        assertResult (None            ) (myCompletions.GetPrevious ())

    [<Test>]
    member _.GetPrevious_without_GetNext () =
        init "e t" { line = 0; char = 3 }

        assertResult None (myCompletions.GetPrevious ())

        assertNotInCompletion ()

    [<Test>]
    member _.GetNext_atInvalidChar () =
        init "e" { line = 0; char = 0 }

        assertResult None (myCompletions.GetNext     ())

        assertNotInCompletion ()

    // GetCompletionsRow -------------------------------------------------------

    [<Test>]
    member _.GetCompletionsRow () =
        init "e t" { line = 0; char = 3 }

        myCompletions.GetNext () |> ignore

        let s, offset, length = myCompletions.GetCompletionsRow ()

        Assert.AreEqual ("true", s)
        Assert.AreEqual (0, offset)
        Assert.AreEqual (4, length)

    // Clear -------------------------------------------------------------------

    [<Test>]
    member _.Clear () =
        init "e t" { line = 0; char = 3 }

        myCompletions.GetNext () |> ignore

        assertInCompletion    ()

        myCompletions.Clear ()

        assertNotInCompletion ()
