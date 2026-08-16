module TextAreaFileWriter

open DataTypes

[<Sealed>]
type TextAreaFileWriter () =

    // prompt history editing mechanism
    static member val FakeWrite: string -> Lines -> unit option
      = fun _ _ -> None
        with get, set

    static member WriteFile filePath encoding fileFormat endWithNewLine lines =
        match TextAreaFileWriter.FakeWrite filePath lines with
        | Some () ->
            Ok ()
        | _ ->
            TextAreaFileWriter.WriteRealFile filePath encoding fileFormat endWithNewLine lines
    
    // private

    static member private WriteRealFile filePath encoding fileFormat endWithNewLine lines =
        TextAreaFileWriter.WriteFileAux filePath encoding fileFormat endWithNewLine lines

    static member private WriteFileAux filePath encoding fileFormat endWithNewLine lines =
        try
            if FileUtils.isDirectory filePath then
                Error "The specified path is a directory."
            else
                Ok (FileUtils.writeFile filePath encoding fileFormat endWithNewLine lines)
        with
        | :? System.ArgumentException as ex ->
            Error ex.Message
        | :? System.IO.PathTooLongException as ex ->
            Error ex.Message
        | :? System.NotSupportedException as ex ->
            Error ex.Message
    //  | :? System.IO.FileNotFoundException as ex ->
    //      Error ex.Message
        | :? System.IO.DirectoryNotFoundException as ex ->
            Error ex.Message
        | :? System.IO.IOException as ex ->
            Error ex.Message
        | :? System.UnauthorizedAccessException as ex ->
            Error ex.Message
