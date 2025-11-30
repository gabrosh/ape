module CommandCompletionsTest

open System
open NUnit.Framework

open CompletionItems
open ConsoleInterop_Specific
open DataTypes
open Position
open UserMessages

let contextRef = TestUtils.makeConsoleContextRef 80 25

let private isUnixOS () =
    match osPlatform with
    | RuntimeOSPlatform.Undefined
    | RuntimeOSPlatform.Windows   -> false
    | RuntimeOSPlatform.Linux
    | RuntimeOSPlatform.FreeBSD
    | RuntimeOSPlatform.OSX       -> true

let private amendLineStr (s: string) =
    let s' =
        if isUnixOS () then
            if s.Contains "@\"" then
                s.Replace ("\\", "/")
            elif s.Contains "\"" then
                s.Replace ("\\\\", "/")
            else
                s
        else
            s

    (s', s.Length - s'.Length)

[<TestFixture>]
type CommandCompletionsTest () =

    let myCompletions = new CompletionItems (
        contextRef, UserMessages (), CommandCompletion.getCompletions
    )

    // initialization

    let init lineStr cursor =
        myCompletions.TrySet [
            stringToChars lineStr, { line = 0; char = cursor }
        ]

    let initForFilePath lineStr =
        let lineStr', delta = amendLineStr lineStr

        let cursor = lineStr'.Length

        myCompletions.TrySet [
            stringToChars lineStr', { line = 0; char = cursor }
        ]

        delta

    // simple assertions

    let assertResult
        (expected: (int * string) option)
        (result: CompletionAction option) =

        let expected' = expected |> Option.map (
            fun (toDelete, toInsert) ->
                let toInsert', _delta = amendLineStr toInsert

                {
                    toDelete = toDelete
                    toInsert = stringToChars toInsert'
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
            System.IO.Directory.GetCurrentDirectory ()

        System.IO.Directory.SetCurrentDirectory (
            IO.Path.Combine ("Data", "FilePathCompletion")
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

    // #filePath - basic -------------------------------------------------------

    [<Test>]
    member _.GetNext_filePath_1 () =
        let _d = initForFilePath "e true utf-8 abz"

        assertResult (Some (3, "abz"    )) (myCompletions.GetNext ())
        assertRowStr "#filePath"
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_filePath_2 () =
        let _d = initForFilePath "e true utf-8 "

        assertResult (Some (0 , "ab"         )) (myCompletions.GetNext ())
        assertRowStr "#filePath:+4"
        assertResult (Some (2 , "@\"ab z.txt")) (myCompletions.GetNext ())
        assertResult (Some (10, "abx.txt"    )) (myCompletions.GetNext ())
        assertResult (Some (7 , "aby.tx"     )) (myCompletions.GetNext ())
        assertResult (Some (6 , "aby.txt"    )) (myCompletions.GetNext ())
        assertResult (None                    ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_filePath_3 () =
        let _d = initForFilePath "e true utf-8 a"

        assertResult (Some (1 , "ab"         )) (myCompletions.GetNext ())
        assertRowStr "#filePath:+4"
        assertResult (Some (2 , "@\"ab z.txt")) (myCompletions.GetNext ())
        assertResult (Some (10, "abx.txt"    )) (myCompletions.GetNext ())
        assertResult (Some (7 , "aby.tx"     )) (myCompletions.GetNext ())
        assertResult (Some (6 , "aby.txt"    )) (myCompletions.GetNext ())
        assertResult (None                    ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_filePath_4 () =
        let _d = initForFilePath "e true utf-8 ab"

        // equalsWithPlatformCase commonPrefix argInCompl = true
        assertResult (Some (2 , "@\"ab z.txt")) (myCompletions.GetNext ())
        assertRowStr "#filePath:4"
        assertResult (Some (10, "abx.txt"    )) (myCompletions.GetNext ())
        assertResult (Some (7 , "aby.tx"     )) (myCompletions.GetNext ())
        assertResult (Some (6 , "aby.txt"    )) (myCompletions.GetNext ())
        assertResult (None                    ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_filePath_5 () =
        let _d = initForFilePath "e true utf-8 abx"

        // filePaths.Length = 1
        assertResult (Some (3, "abx.txt")) (myCompletions.GetNext ())
        assertRowStr "#filePath:1"
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_filePath_6 () =
        let _d = initForFilePath "e true utf-8 aby"

        // equalsWithPlatformCase commonPrefix filePaths[0] = true
        assertResult (Some (3, "aby.tx" )) (myCompletions.GetNext ())
        assertRowStr "#filePath:2"
        assertResult (Some (6, "aby.txt")) (myCompletions.GetNext ())
        assertResult (None               ) (myCompletions.GetNext ())

    // #filePath - wildCards ---------------------------------------------------

    [<Test>]
    member _.GetNext_filePath_wildCards_1 () =
        let _d = initForFilePath "e true utf-8 a*.txt"

        assertResult (Some (6 , "ab"         )) (myCompletions.GetNext ())
        assertRowStr "#filePath:+3"
        assertResult (Some (2 , "@\"ab z.txt")) (myCompletions.GetNext ())
        assertResult (Some (10, "abx.txt"    )) (myCompletions.GetNext ())
        assertResult (Some (7 , "aby.txt"    )) (myCompletions.GetNext ())
        assertResult (None                    ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_filePath_wildCards_2 () =
        let _d = initForFilePath "e true utf-8 ab?.txt"

        assertResult (Some (7, "ab"     )) (myCompletions.GetNext ())
        assertRowStr "#filePath:+2"
        assertResult (Some (2, "abx.txt")) (myCompletions.GetNext ())
        assertResult (Some (7, "aby.txt")) (myCompletions.GetNext ())
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_filePath_wildCards_3 () =
        let _d = initForFilePath "e true utf-8 abx.t*"

        assertResult (Some (6, "abx.txt")) (myCompletions.GetNext ())
        assertRowStr "#filePath:1"
        assertResult (None               ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_filePath_wildCards_4 () =
        let _d = initForFilePath "e true utf-8 abx.tx?"

        assertResult (Some (7, "abx.txt")) (myCompletions.GetNext ())
        assertRowStr "#filePath:1"
        assertResult (None               ) (myCompletions.GetNext ())

    // #filePath - quoted, atQuoted --------------------------------------------

    [<Test>]
    member _.GetNext_filePath_quoted () =
        let d = initForFilePath "e true utf-8 \".\\\\"

        assertResult (Some (4  - d, "\".\\\\ab"      )) (myCompletions.GetNext ())
        assertRowStr "#filePath:+4"
        assertResult (Some (6  - d, "\".\\\\ab z.txt")) (myCompletions.GetNext ())
        assertResult (Some (12 - d, "\".\\\\abx.txt" )) (myCompletions.GetNext ())
        assertResult (Some (11 - d, "\".\\\\aby.tx"  )) (myCompletions.GetNext ())
        assertResult (Some (10 - d, "\".\\\\aby.txt" )) (myCompletions.GetNext ())
        assertResult (None                            ) (myCompletions.GetNext ())

    [<Test>]
    member _.GetNext_filePath_atQuoted () =
        let d = initForFilePath "e true utf-8 @\".\\"

        assertResult (Some (4  - d, "@\".\\ab"      )) (myCompletions.GetNext ())
        assertRowStr "#filePath:+4"
        assertResult (Some (6  - d, "@\".\\ab z.txt")) (myCompletions.GetNext ())
        assertResult (Some (12 - d, "@\".\\abx.txt" )) (myCompletions.GetNext ())
        assertResult (Some (11 - d, "@\".\\aby.tx"  )) (myCompletions.GetNext ())
        assertResult (Some (10 - d, "@\".\\aby.txt" )) (myCompletions.GetNext ())
        assertResult (None                           ) (myCompletions.GetNext ())
