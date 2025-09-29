module TextAreaFileWriter

[<Sealed>]
type TextAreaFileWriter () =

    static member WriteFile filePath encoding fileFormat endWithNewLine lines =
        TextAreaFileWriter.WriteFileAux filePath encoding fileFormat endWithNewLine lines

    // private

    static member private WriteFileAux filePath encoding fileFormat endWithNewLine lines =
        try
            if FileUtils.isDirectory filePath then
                Error "The specified path is a directory."
            else
                Ok (FileUtils.writeFile filePath encoding fileFormat endWithNewLine lines)
        with
        | :? System.IO.DirectoryNotFoundException as ex ->
            Error ex.Message
        | :? System.UnauthorizedAccessException as ex ->
            Error ex.Message
