module CommandCompletionsTest

open System
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

    let assertRowStr expectedStr =
        let s, _offset, _length = myCompletions.GetCompletionsRow ()

        Assert.AreEqual (expectedStr, s) 

    // setup. teardown

    static member val PrevCurrentDirectory = ""
        with get, set

    [<OneTimeSetUp>]
    member _.OneTimeSetup () =
        CommandCompletionsTest.PrevCurrentDirectory <-
            System.IO.Directory.GetCurrentDirectory()

        System.IO.Directory.SetCurrentDirectory(
            IO.Path.Combine("Data", "FilePathCompletion")
        )
        
    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
        System.IO.Directory.SetCurrentDirectory
            CommandCompletionsTest.PrevCurrentDirectory
        
    [<TearDown>]
    member _.TearDown () =
        myCompletions.Clear ()

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

        myCompletions.Clear   ()

        assertNotInCompletion ()

    // GetNext, GetPrevious, IsInCompletion ------------------------------------

    [<Test>]
    member _.GetNext_GetPrevious_atEndOfLine () =
        init "m g" 3

        assertNotInCompletion ()

        assertResult          (Some (1, "global")) (myCompletions.GetNext     ())
        assertRowStr          "global #key"
        assertInCompletion    ()
        assertResult          (Some (6, "g"     )) (myCompletions.GetNext     ())
        assertResult          (None              ) (myCompletions.GetNext     ())
        assertInCompletion    ()
        assertResult          (Some (1, "global")) (myCompletions.GetPrevious ())
        assertInCompletion    ()
        assertResult          (Some (6, "g"     )) (myCompletions.GetPrevious ())
        assertNotInCompletion ()
        assertResult          (None              ) (myCompletions.GetPrevious ())

    [<Test>]
    member _.GetNext_GetPrevious_insideLine () =
        init "m g x" 3

        assertNotInCompletion ()
        assertResult          (Some (1, "global")) (myCompletions.GetNext     ())
        assertRowStr          "global #key"
        assertInCompletion    ()
        assertResult          (Some (6, "g"     )) (myCompletions.GetNext     ())
        assertResult          (None              ) (myCompletions.GetNext     ())
        assertInCompletion    ()
        assertResult          (Some (1, "global")) (myCompletions.GetPrevious ())
        assertInCompletion    ()
        assertResult          (Some (6, "g"     )) (myCompletions.GetPrevious ())
        assertNotInCompletion ()
        assertResult          (None              ) (myCompletions.GetPrevious ())

    [<Test>]
    member _.GetPrevious_without_GetNext () =
        init "m g" 3

        assertResult          None (myCompletions.GetPrevious ())
        assertNotInCompletion ()

    [<Test>]
    member _.GetNext_atInvalidChar () =
        init "m" 0

        assertResult          None (myCompletions.GetNext     ())
        assertNotInCompletion ()

    // #filePath ---------------------------------------------------------------

    [<Test>]
    member _.GetNext_GetPrevious_filePath_1 () =
        init "e true utf-8 abz" 16

        assertResult (Some (3, "abz"    )) (myCompletions.GetNext ())
        assertRowStr "#filePath"
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_GetPrevious_filePath_2 () =
        init "e true utf-8 " 13

        assertResult (Some (0, "ab"     )) (myCompletions.GetNext ())
        assertRowStr "#filePath:+2"
        assertResult (Some (2, "abx.txt")) (myCompletions.GetNext ())
        assertResult (Some (7, "aby.txt")) (myCompletions.GetNext ())
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_GetPrevious_filePath_3 () =
        init "e true utf-8 a" 14

        assertResult (Some (1, "ab"     )) (myCompletions.GetNext ())
        assertRowStr "#filePath:+2"
        assertResult (Some (2, "abx.txt")) (myCompletions.GetNext ())
        assertResult (Some (7, "aby.txt")) (myCompletions.GetNext ())
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_GetPrevious_filePath_4 () =
        init "e true utf-8 ab" 15

        assertResult (Some (2, "abx.txt")) (myCompletions.GetNext ())
        assertRowStr "#filePath:2"
        assertResult (Some (7, "aby.txt")) (myCompletions.GetNext ())
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_GetPrevious_filePath_5 () =
        init "e true utf-8 abx" 16

        assertResult (Some (3, "abx.txt")) (myCompletions.GetNext ())
        assertRowStr "#filePath:1"
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_GetPrevious_filePath_wildCards_1 () =
        init "e true utf-8 a*.txt" 19

        assertResult (Some (6, "ab"     )) (myCompletions.GetNext ())
        assertRowStr "#filePath:+2"
        assertResult (Some (2, "abx.txt")) (myCompletions.GetNext ())
        assertResult (Some (7, "aby.txt")) (myCompletions.GetNext ())
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_GetPrevious_filePath_wildCards_2 () =
        init "e true utf-8 ab?.txt" 20

        assertResult (Some (7, "ab"     )) (myCompletions.GetNext ())
        assertRowStr "#filePath:+2"
        assertResult (Some (2, "abx.txt")) (myCompletions.GetNext ())
        assertResult (Some (7, "aby.txt")) (myCompletions.GetNext ())
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_GetPrevious_filePath_wildCards_3 () =
        init "e true utf-8 abx*" 17

        assertResult (Some (4, "abx.txt")) (myCompletions.GetNext ())
        assertRowStr "#filePath:1"
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_GetPrevious_filePath_wildCards_4 () =
        init "e true utf-8 abx?" 17

        assertResult (Some (4, "abx?"   )) (myCompletions.GetNext ())
        assertRowStr "#filePath"
        assertResult (None               ) (myCompletions.GetNext ())
