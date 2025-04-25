module FileUtils

open System
open System.Text
open System.Collections.Immutable

open ConsoleInterop_Specific

// encodings

// Ensure all available encodings are loaded.
let _ = Encoding.RegisterProvider CodePagesEncodingProvider.Instance

let encodingsArray =
    seq {
        for enc in Encoding.GetEncodings () do
            yield enc.Name
        yield "utf-8-bom"
        yield "utf-16-bom"
        yield "utf-32-bom"
    }
    |> Array.ofSeq
    |> Array.sort

let defaultEncoding = "utf-8"

let encodingsSet = Set.ofArray encodingsArray

let suggestedEncodings = [|
    "utf-8"
    "utf-8-bom"
    "windows-1250"
    "windows-1252"
    "iso-8859-1"
    "iso-8859-2"
|]

// file formats

type FileFormat =
    | dos  = 1
    | unix = 2
    | mac  = 3

let defaultFileFormat =
    match osPlatform with
    | RuntimeOSPlatform.Undefined
    | RuntimeOSPlatform.Windows   -> FileFormat.dos
    | RuntimeOSPlatform.Linux
    | RuntimeOSPlatform.FreeBSD
    | RuntimeOSPlatform.OSX       -> FileFormat.unix

let fileFormatsArray =
    System.Enum.GetNames typeof<FileFormat>

let getNewLineSeparator fileFormat =
    match fileFormat with
    | FileFormat.dos  -> "\r\n"
    | FileFormat.unix -> "\n"
    | FileFormat.mac  -> "\r"
    | _ ->
        invalidOp "Wrong enum value"

let decideFileFormat fileFormatAcc =
    let hasCR = (fileFormatAcc &&& int '\r') = int '\r'
    let hasLF = (fileFormatAcc &&& int '\n') = int '\n'

    if hasCR && not hasLF then
        FileFormat.mac
    elif hasLF && not hasCR then
        FileFormat.unix
    else
        FileFormat.dos

// others

type private Chars = ImmutableArray<char>

// the default values for IO.StreamReader and IO.StreamWriter
let private readBufferSize  = 4096
let private writeBufferSize = 4096

let getReadOptions () =
    let streamOptions = IO.FileStreamOptions ()

    streamOptions.Mode       <- IO.FileMode.Open
    streamOptions.Access     <- IO.FileAccess.Read
    streamOptions.Share      <- IO.FileShare.ReadWrite
    streamOptions.BufferSize <- readBufferSize

    streamOptions

let getWriteOptions () =
    let streamOptions = IO.FileStreamOptions ()

    streamOptions.Mode       <- IO.FileMode.Create
    streamOptions.Access     <- IO.FileAccess.Write
    streamOptions.Share      <- IO.FileShare.Read
    streamOptions.BufferSize <- writeBufferSize

    streamOptions

let private getEncoding (encoding: string) (strictEncoding: bool) =
    match encoding with
    | "utf-8"      -> Text.UTF8Encoding    (false,        strictEncoding) :> Encoding
    | "utf-8-bom"  -> Text.UTF8Encoding    (true ,        strictEncoding) :> Encoding
    | "utf-16"     -> Text.UnicodeEncoding (false, false, strictEncoding) :> Encoding
    | "utf-16-bom" -> Text.UnicodeEncoding (false, true , strictEncoding) :> Encoding
    | "utf-32"     -> Text.UTF32Encoding   (false, false, strictEncoding) :> Encoding
    | "utf-32-bom" -> Text.UTF32Encoding   (false, true , strictEncoding) :> Encoding

    | _            -> Text.Encoding.GetEncoding encoding

/// Appends content of the file to lines.
let readFile
    (filePath: string) (encoding: string) (strictEncoding: bool)
    (lines: ResizeArray<Chars>) =

    use stream = new IO.StreamReader (
        filePath, getEncoding encoding strictEncoding, true, getReadOptions ()
    )

    let inputBuffer = Array.zeroCreate readBufferSize

    let inputLine = ImmutableArray.CreateBuilder<char> ()

    let mutable charsRead = 0

    let readData = (
        fun () ->
            charsRead <- stream.ReadBlock (inputBuffer, 0, readBufferSize)
            charsRead <> 0
    )

    let mutable toSkipNextLF    = false
    let mutable endsWithNewLine = false
    let mutable fileFormatAcc   = 0

    while readData () do
        let mutable i = 0
        let mutable startChar = 0

        while i < charsRead do
            let c = inputBuffer[i]

            if c = '\r' || c = '\n' then
                if c = '\r' || not toSkipNextLF then
                    inputLine.AddRange (
                        inputBuffer.AsSpan (startChar, i - startChar)
                    )
                    lines.Add (inputLine.DrainToImmutable ())
                startChar <- i + 1

                fileFormatAcc <- fileFormatAcc ||| int c

            toSkipNextLF <- c = '\r'

            i <- i + 1

        // any characters after the last new line separator in the block ?
        if startChar < i then
            inputLine.AddRange (
                inputBuffer.AsSpan (startChar, i - startChar)
            )
            endsWithNewLine <- false
        else
            endsWithNewLine <- true

    if inputLine.Count <> 0 then
        lines.Add (inputLine.DrainToImmutable ())

    let fileFormat = decideFileFormat fileFormatAcc

    (fileFormat, endsWithNewLine)

/// Writes content of lines to file.
let writeFile
    (filePath: string) (encoding: string) (fileFormat: FileFormat) (endWithNewLine: bool)
    (lines: ResizeArray<Chars>) =

    let newLineSeparator = getNewLineSeparator fileFormat

    use stream = new IO.StreamWriter (
        filePath, getEncoding encoding false, getWriteOptions ()
    )

    let m = lines.Count - 1

    for i = 0 to m - 1 do
        stream.Write (lines[i].AsSpan ())
        stream.Write newLineSeparator

    stream.Write (lines[m].AsSpan ())
    if endWithNewLine then
        stream.Write newLineSeparator

/// Returns true if file with given path already exists.
let fileExists (filePath: string) =
    IO.File.Exists filePath
