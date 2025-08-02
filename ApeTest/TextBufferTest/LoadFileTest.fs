module TextBufferLoadFileTest

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
type LoadFileTest () =
    let myBuffer = new TextAreaBuffer (contextRef, UserMessages (), Registers (), "")

    // initialization

    let init () =
        myBuffer.FilePath <- testFilePath

    let toTuple2 (a: 'T array) =
        a[0], a[1]

    let writeChars (chars: string) =
        File.WriteAllText (testFilePath, chars, Encoding.UTF8)

    let writeCharsLatin1 (chars: string) =
        File.WriteAllText (testFilePath, chars, Encoding.Latin1)

    let deleteTestFile () =
        if System.IO.File.Exists(testFilePath) then
            System.IO.File.Delete(testFilePath)

    // command and simple assertions

    let assertLines (expLines: string array) =
        Assert.AreEqual (expLines, myBuffer.Lines)

    let assertFormat (expFileFormat: FileFormat) (fileFormat: FileFormat) =
        Assert.AreEqual (expFileFormat, fileFormat)

    let assertEnding (expEndsWithNewLine: bool) (endsWithNewLine: bool) =
        Assert.AreEqual (expEndsWithNewLine, endsWithNewLine)

    let assertNonTranslatableByes (expNonTranslatableBytes: bool) =
        let reloadFileParams = myBuffer.ReloadFileParams

        Assert.IsTrue reloadFileParams.IsSome
        Assert.AreEqual (
            expNonTranslatableBytes, reloadFileParams.Value.nonTranslatableBytes
        )

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()
      deleteTestFile ()

    // LoadFile ----------------------------------------------------------------

    [<TestCase( "a\r\nb\r\nc"    , FileFormat.dos , false )>]  // dos
    [<TestCase( "a\nb\nc"        , FileFormat.unix, false )>]  // unix
    [<TestCase( "a\rb\rc"        , FileFormat.mac , false )>]  // mac

    [<TestCase( "a\r\nb\r\nc\r\n", FileFormat.dos , true  )>]  // dos
    [<TestCase( "a\nb\nc\n"      , FileFormat.unix, true  )>]  // unix
    [<TestCase( "a\rb\rc\r"      , FileFormat.mac , true  )>]  // mac

    [<TestCase( "a\r\nb\nc"      , FileFormat.dos , false )>]  // dos  unix
    [<TestCase( "a\r\nb\rc"      , FileFormat.dos , false )>]  // dos  mac
    [<TestCase( "a\nb\r\nc"      , FileFormat.dos , false )>]  // unix dos
    [<TestCase( "a\rb\r\nc"      , FileFormat.dos , false )>]  // mac  dos
    [<TestCase( "a\nb\rc"        , FileFormat.dos , false )>]  // unix mac
    member _.LoadFile fileContent expFileFormat expEndsWithNewLine =
        init ()

        writeChars fileContent

        let fileFormat, endsWithNewLine = myBuffer.LoadFile "utf-8" true false

        assertLines  [|"a"; "b"; "c"|]
        assertFormat expFileFormat fileFormat
        assertEnding expEndsWithNewLine endsWithNewLine

    [<TestCase( "a\n\rb\r\nc", FileFormat.dos , false  )>]  // unix mac dos
    member _.LoadFile_UnixMacJoined fileContent expFileFormat expEndsWithNewLine =
        init ()

        writeChars fileContent

        let fileFormat, endsWithNewLine = myBuffer.LoadFile "utf-8" true false

        assertLines  [|"a"; ""; "b"; "c"|]
        assertFormat expFileFormat fileFormat
        assertEnding expEndsWithNewLine endsWithNewLine

    [<TestCase( "abc"   , "abc"     , false )>]
    [<TestCase( "a\xFFc", "a\uFFFDc", true  )>]
    member _.LoadFile_NonTranslatable fileContent expLine expNonTranslatable =
        init ()

        writeCharsLatin1 fileContent

        let _fileFormat, _endsWithNewLine = myBuffer.LoadFile "utf-8" true false

        assertLines [| expLine |]
        assertNonTranslatableByes expNonTranslatable
