module TextAreaFileReader

open System

open Context
open DataTypes
open UserMessages
open WrappedRef

[<Sealed>]
type TextAreaFileReader (
    myContextRef:   IWrappedRef<MainContext>,
    myUserMessages: UserMessages,
    myLines:        Lines
) =
    let mutable myContext = myContextRef.Value
    let handleContextChanged () = myContext <- myContextRef.Value
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    // ReloadFile mechanism
    let mutable myReloadFileParams: FileUtils.ReloadFileParams option = None

    // prompt history editing mechanism
    static member val FakeRead: string -> Lines option
      = fun _ -> None
        with get, set
    
    // only for testing purposes
    member _.ReloadFileParams = myReloadFileParams

    member this.LoadStrings (lines: string seq) resetFun =
        myLines.Clear ()

        try
            this.LoadStringsAux lines myLines
        finally
            this.AssureNonZeroLinesCount ()
            resetFun ()

    member private this.LoadLines (lines: Lines) resetFun =
        myLines.Clear ()

        try
            this.LoadLinesAux lines myLines
        finally
            this.AssureNonZeroLinesCount ()
            resetFun ()

    member this.LoadFile filePath encoding strictEncoding resetFun =
        match TextAreaFileReader.FakeRead filePath with
        | Some lines ->
            this.LoadLines lines resetFun
            Ok (FileUtils.defaultFileFormat, false)
        | _ ->
            this.LoadRealFile filePath encoding strictEncoding resetFun
    
    member this.ReloadFile filePath encoding strictEncoding resetFun =
        match TextAreaFileReader.FakeRead filePath with
        | Some lines ->
            this.LoadLines lines resetFun
            Ok (FileUtils.defaultFileFormat, false)
        | _ ->
            this.ReloadRealFile filePath encoding strictEncoding resetFun
    
    // private

    member private this.LoadRealFile filePath encoding strictEncoding resetFun =
        match this.OpenFileForReading filePath encoding with
        | Ok stream' ->
            use stream = stream'

            myLines.Clear ()

            let reloadFileParams = None

            try
                let fileFormat, endsWithNewLine, reloadFileParams' =
                    this.LoadFileAux filePath stream strictEncoding reloadFileParams

                TestMines.checkMine (nameof this.LoadFile)

                myReloadFileParams <- reloadFileParams'

                Ok (fileFormat, endsWithNewLine)
            finally
                this.AssureNonZeroLinesCount ()
                resetFun ()

        | Error e ->
            Error e

    member private this.ReloadRealFile filePath encoding strictEncoding resetFun =
        match this.OpenFileForReading filePath encoding with
        | Ok stream' ->
            use stream = stream'

            if not myContext.reloadAsLogFile then
                myLines.Clear ()

            let reloadFileParams =
                if myContext.reloadAsLogFile then
                    myReloadFileParams
                else
                    None

            try
                let fileFormat, endsWithNewLine, reloadFileParams' =
                    this.LoadFileAux filePath stream strictEncoding reloadFileParams

                TestMines.checkMine (nameof this.ReloadFile)

                myReloadFileParams <- reloadFileParams'

                Ok (fileFormat, endsWithNewLine)
            finally
                this.AssureNonZeroLinesCount ()
                resetFun ()

        | Error e ->
            Error e

    member private _.LoadStringsAux lines result =
        for line in lines do
            result.Add (stringToChars line)

    member private _.LoadLinesAux lines result =
        for line in lines do
            result.Add line

    member private _.OpenFileForReading filePath encoding =
        try
            if FileUtils.isDirectory filePath then
                Error "The specified path is a directory."
            else
                Ok (FileUtils.openFileForReading filePath encoding)
        with
        | :? System.ArgumentException as ex ->
            Error ex.Message
        | :? System.IO.PathTooLongException as ex ->
            Error ex.Message
        | :? System.NotSupportedException as ex ->
            Error ex.Message
        | :? System.IO.FileNotFoundException as ex ->
            Error ex.Message
        | :? System.IO.DirectoryNotFoundException as ex ->
            Error ex.Message
        | :? System.IO.IOException as ex ->
            Error ex.Message
        | :? System.UnauthorizedAccessException as ex ->
            Error ex.Message

    member private _.LoadFileAux filePath stream strictEncoding reloadFileParams =
        let result = FileUtils.readFile stream reloadFileParams myLines

        if strictEncoding then
            match result with
            | _, _, Some reloadFileParams' ->
                if reloadFileParams'.nonTranslatableBytes then
                    myUserMessages.RegisterMessage (
                        UserMessages.formatMessage WARNING_NON_TRANSLATABLE_BYTES filePath
                    )
            | _ ->
                ()

        result

    member private _.AssureNonZeroLinesCount () =
        if myLines.Count = 0 then
            myLines.Add Chars.Empty

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable.Dispose ()
