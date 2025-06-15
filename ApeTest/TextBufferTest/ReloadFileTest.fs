module TextBufferReloadFileTest

open NUnit.Framework
open System
open System.IO
open System.Text

open FileUtils
open Registers
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef 80 25

let testFilePath = Path.GetTempFileName ()

[<TestFixture>]
type ReloadFileTest () =
    let myBuffer = new TextAreaBuffer (contextRef, UserMessages (), Registers (), "")

    // initialization

    let init () =
        myBuffer.FilePath <- testFilePath

    let toTuple2 (a: 'T array) =
        a[0], a[1]

    let writeChars (chars: string) =
        File.WriteAllText (testFilePath, chars, Encoding.UTF8)

    let deleteTestFile () =
        if System.IO.File.Exists(testFilePath) then
            System.IO.File.Delete(testFilePath)

    // command and simple assertions

    let assertLines expLines =
        Assert.AreEqual (expLines, myBuffer.Lines)

    let assertFormat (expFileFormat: FileFormat) (fileFormat: FileFormat) =
        Assert.AreEqual (expFileFormat, fileFormat)

    let assertEnding (expEndsWithNewLine: bool) (endsWithNewLine: bool) =
        Assert.AreEqual (expEndsWithNewLine, endsWithNewLine)

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()
      deleteTestFile ()

    // ReloadFile --------------------------------------------------------------

    [<TestCase( [|"a"    ; "\r\nb"    |], [|FileFormat.none; FileFormat.dos |], [|false; false|] )>]  // dos
    [<TestCase( [|"a\r\n"; "b"        |], [|FileFormat.dos ; FileFormat.dos |], [|true ; false|] )>]  // dos
    [<TestCase( [|"a"    ; "\r\nb\r\n"|], [|FileFormat.none; FileFormat.dos |], [|false; true |] )>]  // dos
    [<TestCase( [|"a\r\n"; "b\r\n"    |], [|FileFormat.dos ; FileFormat.dos |], [|true ; true |] )>]  // dos
                                                                  
    [<TestCase( [|"a"    ; "\nb"      |], [|FileFormat.none; FileFormat.unix|], [|false; false|] )>]  // unix
    [<TestCase( [|"a\n"  ; "b"        |], [|FileFormat.unix; FileFormat.unix|], [|true ; false|] )>]  // unix
    [<TestCase( [|"a"    ; "\nb\n"    |], [|FileFormat.none; FileFormat.unix|], [|false; true |] )>]  // unix
    [<TestCase( [|"a\n"  ; "b\n"      |], [|FileFormat.unix; FileFormat.unix|], [|true ; true |] )>]  // unix
                                                                  
    [<TestCase( [|"a"    ; "\rb"      |], [|FileFormat.none; FileFormat.mac |], [|false; false|] )>]  // mac
    [<TestCase( [|"a\r"  ; "b"        |], [|FileFormat.mac ; FileFormat.mac |], [|true ; false|] )>]  // mac
    [<TestCase( [|"a"    ; "\rb\r"    |], [|FileFormat.none; FileFormat.mac |], [|false; true |] )>]  // mac
    [<TestCase( [|"a\r"  ; "b\r"      |], [|FileFormat.mac ; FileFormat.mac |], [|true ; true |] )>]  // mac
                                                           
    [<TestCase( [|"a\r\n"; "b\n"      |], [|FileFormat.dos ; FileFormat.dos |], [|true ; true |] )>]  // dos unix
    [<TestCase( [|"a\r\n"; "b\r"      |], [|FileFormat.dos ; FileFormat.dos |], [|true ; true |] )>]  // dos mac
    [<TestCase( [|"a\n"  ; "b\r\n"    |], [|FileFormat.unix; FileFormat.dos |], [|true ; true |] )>]  // unix dos
    [<TestCase( [|"a\r"  ; "b\r\n"    |], [|FileFormat.mac ; FileFormat.dos |], [|true ; true |] )>]  // mac  dos
    [<TestCase( [|"a\n"  ; "b\r"      |], [|FileFormat.unix; FileFormat.dos |], [|true ; true |] )>]  // unix mac
                                                                            
    [<TestCase( [|"a\r"  ; "\nb"      |], [|FileFormat.mac        ; FileFormat.dos |], [|true ; false|] )>]  // mac -> dos
    member _.ReloadFile fileContents expFileFormats expEndsWithNewLines  =
        let fileContent1       , fileContent2        = toTuple2 fileContents
        let expFileFormat1     , expFileFormat2      = toTuple2 expFileFormats
        let expEndsWithNewLine1, expEndsWithNewLine2 = toTuple2 expEndsWithNewLines

        let expFileFormat1 =
            if expFileFormat1 = FileFormat.none then
                FileUtils.defaultFileFormat
            else
                expFileFormat1

        init ()

        writeChars fileContent1

        let fileFormat, endsWithNewLine = myBuffer.LoadFile "utf-8" true false

        assertLines  [|"a"|]
        assertFormat expFileFormat1 fileFormat
        assertEnding expEndsWithNewLine1 endsWithNewLine

        writeChars (fileContent1 + fileContent2)

        let fileFormat, endsWithNewLine = myBuffer.ReloadFile "utf-8" true

        assertLines  [|"a"; "b"|]
        assertFormat expFileFormat2 fileFormat
        assertEnding expEndsWithNewLine2 endsWithNewLine

    [<TestCase( [|"a\n"; "\rb"|], [|FileFormat.unix; FileFormat.dos|], [|true ; false|] )>]  // unix mac
    member _.ReloadFile_UnixMacJoined fileContents expFileFormats expEndsWithNewLines  =
        let fileContent1       , fileContent2        = toTuple2 fileContents
        let expFileFormat1     , expFileFormat2      = toTuple2 expFileFormats
        let expEndsWithNewLine1, expEndsWithNewLine2 = toTuple2 expEndsWithNewLines

        init ()

        writeChars fileContent1

        let fileFormat, endsWithNewLine = myBuffer.LoadFile "utf-8" true false

        assertLines  [|"a"|]
        assertFormat expFileFormat1 fileFormat
        assertEnding expEndsWithNewLine1 endsWithNewLine

        writeChars (fileContent1 + fileContent2)

        let fileFormat, endsWithNewLine = myBuffer.ReloadFile "utf-8" true

        assertLines  [|"a"; ""; "b"|]
        assertFormat expFileFormat2 fileFormat
        assertEnding expEndsWithNewLine2 endsWithNewLine
