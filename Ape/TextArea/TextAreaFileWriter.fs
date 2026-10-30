module TextAreaFileWriter

open DataTypes
open FileUtils
open IVirtualFile

[<Sealed>]
type TextAreaFileWriter () =

    // prompt history editing mechanism
    static member val VirtualFile: IVirtualFile = DefaultVirtualFile()
        with get, set

    static member WriteFile filePath encoding fileFormat endWithNewLine lines =
        match TextAreaFileWriter.VirtualFile.Write filePath lines with
        | Some () ->
            Ok ()
        | _ ->
            TextAreaFileWriter.WriteRealFile filePath encoding fileFormat endWithNewLine lines

    /// Returns state of the file on disk.
    static member GetDiskFileState filePath =
        match TextAreaFileWriter.VirtualFile.GetDiskFileState filePath with
        | Some diskFileState ->
            diskFileState
        | _ ->
            getDiskFileState filePath

    /// Returns true if the file on disk has changed since the last time it was read.
    static member IsDiskFileChanged (filePath: string) (lastState: DiskFileState) =
        if lastState <> DiskFileState_Default filePath then
            let currentState = TextAreaFileWriter.GetDiskFileState filePath
            currentState.name = lastState.name &&
                currentState <> lastState
        else
            false
    
    // private

    static member private WriteRealFile filePath encoding fileFormat endWithNewLine lines =
        TextAreaFileWriter.WriteFileAux filePath encoding fileFormat endWithNewLine lines

    static member private WriteFileAux filePath encoding fileFormat endWithNewLine lines =
        try
            if FileUtils.isDirectory filePath then
                Error "The specified path is a directory."
            else
                FileUtils.writeFile filePath encoding fileFormat endWithNewLine lines
                Ok ()
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
