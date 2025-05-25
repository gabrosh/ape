module BuffersRegistry

open System

open Context
open TextAreaBuffer
open TextAreaBufferExtract
open UserMessages
open WrappedRef

let noFilePath = "no_name.txt"

type private Item = {
    settings:       Settings.Settings
    keyMappings:    KeyMappings.KeyMappings
    mainContextRef: WrappedRef<MainContext>
    buffer:         ITextAreaBuffer.ITextAreaBuffer
}

let private getFullName filePath =
    (IO.Path.GetFullPath filePath).ToLower ()

[<Sealed>]
type BuffersRegistry (
    myContextRef:        IWrappedRef<ConsoleContext>,
    myUserMessages:      UserMessages,
    myGlobalSettings:    Settings.Settings,
    myGlobalKeyMappings: KeyMappings.KeyMappings,
    myRegisters:         Registers.Registers
) as this_ =
    let myItems = ResizeArray<Item> ()
    let mutable myCurrentIndex = -1

    do
        this_.AddTextAreaBuffer noFilePath

    member _.CurrentSettings       = myItems[myCurrentIndex].settings
    member _.CurrentKeyMappings    = myItems[myCurrentIndex].keyMappings
    member _.CurrentMainContextRef = myItems[myCurrentIndex].mainContextRef
    member _.CurrentBuffer         = myItems[myCurrentIndex].buffer

    /// Updates main context of the current buffer.
    member this.UpdateCurrentMainContext () =
        this.CurrentMainContextRef.Value <-
            makeMainContext myContextRef.Value this.CurrentSettings

    /// Adds an empty TextAreaBuffer at the end of the registry and sets it as the current one.
    member _.AddTextAreaBuffer (filePath: string) =
        let bufferSettings = Settings.makeBufferSettings myGlobalSettings

        let bufferKeyMappings = KeyMappings.makeBufferKeyMappings myGlobalKeyMappings

        let mainContextRef = WrappedRef (makeMainContext myContextRef.Value bufferSettings)

        let buffer = new TextAreaBuffer (
            mainContextRef, myUserMessages, myRegisters, filePath
        )

        myCurrentIndex <- myItems.Count

        myItems.Add {
            settings       = bufferSettings
            keyMappings    = bufferKeyMappings
            mainContextRef = mainContextRef
            buffer         = buffer
        }

    /// Adds an empty TextAreaBufferExtract at the end of the registry and sets it as the current one.
    member _.AddTextAreaBufferExtract
        (parentBuffer: TextAreaBuffer) (parentSettings: Settings.Settings)
        (fileName: string) (extractOnConstr: bool)
      =

        let bufferSettings = Settings.cloneSettings parentSettings

        let bufferKeyMappings = KeyMappings.makeBufferKeyMappings myGlobalKeyMappings

        let mainContextRef = WrappedRef (makeMainContext myContextRef.Value bufferSettings)

        let buffer = new TextAreaBufferExtract (
            parentBuffer, mainContextRef, myUserMessages, myRegisters, fileName, extractOnConstr
        )
        buffer.Init ()

        parentBuffer.RegisterChild buffer

        myCurrentIndex <- myItems.Count

        myItems.Add {
            settings       = bufferSettings
            keyMappings    = bufferKeyMappings
            mainContextRef = mainContextRef
            buffer         = buffer
        }

    /// Deletes the current buffer from the registry.
    /// Switches to the next buffer or the first one if the deleted buffer was the last.
    member this.DeleteBuffer () =
        let buffer = this.CurrentBuffer
        myItems.RemoveAt myCurrentIndex
        (buffer :> IDisposable).Dispose ()

        if myItems.Count = 0 then
            this.AddTextAreaBuffer noFilePath
            myCurrentIndex <- 0
        elif myCurrentIndex = myItems.Count then
            myCurrentIndex <- 0

    /// Switches to given buffer.
    member this.ToBuffer buffer =
        let index = this.GetIndex buffer
        if index <> -1 then
            myCurrentIndex <- index
        
    /// Switches to the next buffer or the first one if the current buffer is the last.
    member _.ToNextBuffer () =
        if myCurrentIndex < myItems.Count - 1 then
            myCurrentIndex <- myCurrentIndex + 1
        elif myItems.Count = 1 then
            myUserMessages.RegisterMessage INFO_ONLY_ONE_BUFFER
        else
            myUserMessages.RegisterMessage INFO_BACK_AT_FIRST_BUFFER
            myCurrentIndex <- 0

    /// Switches to the previous buffer or the last one if the current buffer is the first.
    member _.ToPrevBuffer () =
        if myCurrentIndex > 0 then
            myCurrentIndex <- myCurrentIndex - 1
        elif myItems.Count = 1 then
            myUserMessages.RegisterMessage INFO_ONLY_ONE_BUFFER
        else
            myUserMessages.RegisterMessage INFO_BACK_AT_LAST_BUFFER
            myCurrentIndex <- myItems.Count - 1

    /// Returns true if any of the buffers is changed.
    member this.IsAnyBufferChanged () =
        this.GetIndexOfFirstChanged () <> -1

    /// Switches to the first changed buffer if it is different from the current one.
    member this.ToFirstChanged () =
        let index = this.GetIndexOfFirstChanged ()
        if index <> -1 then
            myCurrentIndex <- index

    /// Returns true if buffer with given filePath exists.
    member this.HasBufferWithFilePath filePath =
        this.GetIndexByFilePath filePath <> -1

    /// Switches to the buffer with given filePath.
    member this.ToBufferWithFilePath filePath =
        let index = this.GetIndexByFilePath filePath
        if index <> -1 then
            myCurrentIndex <- index

    member private _.GetIndex buffer =
        myItems.FindIndex (
            fun item ->
                item.buffer = buffer
        )

    member private _.GetIndexOfFirstChanged () =
        myItems.FindIndex (
            fun item -> item.buffer.IsBufferChanged
        )

    member private _.GetIndexByFilePath filePath =
        let fullName = getFullName filePath

        myItems.FindIndex (
            fun item ->
                getFullName item.buffer.FilePath = fullName
        )

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            for item in myItems do
                (item.buffer :> IDisposable).Dispose ()
            myItems.Clear ()
