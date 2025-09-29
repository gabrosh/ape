module FileUtils

open System
open System.Text
open System.Collections.Immutable

open ConsoleInterop_Specific

/// File extension added to the file path when creating an extract buffer.
let extractFileExt = ".x"

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
    | none = 0  // Used only in test cases definitions as a substitution for either
                // defaultFileFormat or None, which are not constant expressions.
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

let private getFileFormat fileFormatAcc =
    let hasCR = (fileFormatAcc &&& int '\r') = int '\r'
    let hasLF = (fileFormatAcc &&& int '\n') = int '\n'

    if hasCR && hasLF then
        Some FileFormat.dos
    elif hasCR then
        Some FileFormat.mac
    elif hasLF then
        Some FileFormat.unix
    else
        None

let private getNewLineSeparator fileFormat =
    match fileFormat with
    | FileFormat.dos  -> "\r\n"
    | FileFormat.unix -> "\n"
    | FileFormat.mac  -> "\r"
    | _ ->
        invalidOp "Wrong enum value"

// others

type Chars = ImmutableArray<char>
type Lines = ResizeArray<Chars>

[<Struct>]
type ReloadFileParams = {
    fileOffset:           int64
    fileFormatAcc:        int
    toSkipNextLF:         bool
    endsWithNewLine:      bool
    nonTranslatableBytes: bool
}

let ReloadFileParams_Zero = {
    fileOffset           = 0
    fileFormatAcc        = 0
    toSkipNextLF         = false
    endsWithNewLine      = false
    nonTranslatableBytes = false
}

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

/// Provides the core functionality of readFile function.
let private readFileAux
    (stream: IO.StreamReader)
    (reloadFileParams: ReloadFileParams option) (lines: Lines) =

    let inputBuffer = Array.zeroCreate readBufferSize
    let inputLine = ImmutableArray.CreateBuilder<char> ()

    match reloadFileParams with
    | Some rfp ->
        if not rfp.endsWithNewLine then
            inputLine.AddRange lines[lines.Count - 1]
            lines.RemoveAt(lines.Count - 1)
    | None ->
        ()

    let a, b, c, d =
        reloadFileParams
        |> Option.map (
            fun x -> (x.fileFormatAcc, x.toSkipNextLF, x.endsWithNewLine, x.nonTranslatableBytes)
        )
        |> Option.defaultValue (
            0, false, false, false
        )

    let mutable fileFormatAcc        = a
    let mutable toSkipNextLF         = b
    let mutable endsWithNewLine      = c
    let mutable nonTranslatableBytes = d

    let mutable charsRead = 0

    let readData = (
        fun () ->
            charsRead <- stream.ReadBlock (inputBuffer, 0, readBufferSize)
            charsRead <> 0
    )

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
            elif c = '\uFFFD' then
                nonTranslatableBytes <- true

            toSkipNextLF <- c = '\r'

            i <- i + 1

        // Any characters after the last new line separator in the block ?
        if startChar < i then
            inputLine.AddRange (
                inputBuffer.AsSpan (startChar, i - startChar)
            )
            endsWithNewLine <- false
        else
            endsWithNewLine <- true

    if inputLine.Count <> 0 then
        lines.Add (inputLine.DrainToImmutable ())

    let reloadFileParams = {
        fileOffset           = stream.BaseStream.Position
        fileFormatAcc        = fileFormatAcc
        toSkipNextLF         = toSkipNextLF
        endsWithNewLine      = endsWithNewLine
        nonTranslatableBytes = nonTranslatableBytes
    }

    let fileFormat =
        getFileFormat fileFormatAcc |> Option.defaultValue defaultFileFormat

    (fileFormat, endsWithNewLine, Some reloadFileParams)

/// Opens given file with encoding for reading and returns it as a StreamReader.
let openFileForReading (filePath: string) (encoding: string) =
    new IO.StreamReader (
        filePath, getEncoding encoding false, true, getReadOptions ()
    )

/// Appends content of given stream to lines, taking into account reloadFileParams
/// if provided. In that case it assumes that lines is not empty, and the last line
/// may be extended based on reloadFileParams and read input.
let readFile
    (stream: IO.StreamReader)
    (reloadFileParams: ReloadFileParams option)
    (lines: Lines) =

    match reloadFileParams with
    | Some reloadFileParams ->
        // Can seek beyond the length of the file - no bytes are read then.
        stream.BaseStream.Seek (reloadFileParams.fileOffset, IO.SeekOrigin.Begin)
            |> ignore
        stream.DiscardBufferedData ()
    | None ->
        ()

    readFileAux stream reloadFileParams lines

/// Writes content of lines to file.
let writeFile
    (filePath: string) (encoding: string)
    (fileFormat: FileFormat) (endWithNewLine: bool)
    (lines: Lines) =

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

/// Returns true if entry with given path is a directory.
let isDirectory (path: string) =
    try
        let attrs = IO.File.GetAttributes path
        let isDirectory = IO.FileAttributes.Directory
        attrs &&& isDirectory = isDirectory
    with
        | :? System.IO.FileNotFoundException ->
            false
