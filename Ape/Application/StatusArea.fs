module StatusArea

open System

open Common
open ConsoleKeys
open Context
open DataTypes
open WrappedRef

let private getFindCharStateString isExtending state =
    match state with
    | LeftToCharState     -> if isExtending then "^F" else "^f"
    | LeftUntilCharState  -> if isExtending then "^T" else "^t"
    | RightToCharState    -> if isExtending then "F"  else "f"
    | RightUntilCharState -> if isExtending then "T"  else "t"

let private getRegisterStateString state =
    match state with
    | ToSelectRegisterState false      -> "'"
    | ToSelectRegisterState true       -> "\""
    | SelectedRegisterState (false, c) -> $"'{c}"
    | SelectedRegisterState (true , c) -> $"\"{c}"

let private getModeString mode =
    match mode with
    | NormalMode _
        -> " Normal "
    | InsertMode _
        -> " Insert "
    // not used - prompt modes are not indicated in the status line this way
    | PromptNormalMode _
    | PromptInsertMode _
        -> " PROMPT "

let private getKeyPrefixString keyPrefix =
    match keyPrefix with
    | Some keyPrefix ->
        let c = keyPrefixToChar keyPrefix |> Option.get
        $"{c} "
    | None ->
        ""

let private getStateString mode =
    match mode with

    | NormalMode NormalMainState
        -> ""
    | NormalMode (CountState count)
        -> $"%d{count} "

    | NormalMode (RegisterState (0     , state))
        -> $"%s{getRegisterStateString state} "
    | NormalMode (RegisterState (count , state))
        -> $"%d{count} %s{getRegisterStateString state} "

    | NormalMode (FindCharState (0     , isExtending, state))
        -> $"%s{getFindCharStateString isExtending state} "
    | NormalMode (FindCharState (count , isExtending, state))
        -> $"%d{count} %s{getFindCharStateString isExtending state} "

    | NormalMode (FillWithCharState 0)
        -> "r "
    | NormalMode (FillWithCharState count)
        -> $"%d{count} r "

    | NormalMode (GoToState isExtending)
        -> if isExtending then "G " else "g "

    | NormalMode ViewState
        -> "v "

    | InsertMode InsertMainState
        -> ""
    | InsertMode (InsertPasteState false)
        -> "r "
    | InsertMode (InsertPasteState true )
        -> "R "

    // not used - prompt modes are not indicated in the status line this way
    | PromptNormalMode _
    | PromptInsertMode _
        -> ""

/// Represents a two-line status area. Provides DisplayChars to be rendered into
/// the individual lines of the status area according to several input parameters.

[<Sealed>]
type StatusArea (
    myContextRef: IWrappedRef<MainContext>
) =
    let mutable myRenderingContext =
        makeRendereringContext myContextRef.Value
    let handleContextChanged () =
        myRenderingContext <- makeRendereringContext myContextRef.Value
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    /// Returns the first display row of the status line.
    member this.GetFirstDisplayRow
        (bufferName: string) (statusChar: char) isRecording cursorPos =

        let displayRow = ResizeArray myRenderingContext.windowWidth

        let statusColors = myRenderingContext.colorScheme.status

        let statusString: string = statusChar.ToString ()
        let isRecordingColors = this.GetIsRecordingColors isRecording
        let cursorString: string = this.GetCursorString cursorPos
        
        let othersLength = statusString.Length + 1 + cursorString.Length
        let filePathMaxLength = myRenderingContext.windowWidth - othersLength
        let bufferNameString = this.GetBufferNameString bufferName filePathMaxLength
        
        this.WriteTo displayRow statusString statusColors
        this.WriteTo displayRow " " isRecordingColors
        this.WriteTo displayRow bufferNameString statusColors
        this.WriteTo displayRow cursorString statusColors

        displayRow.GetRange (0, myRenderingContext.windowWidth)
    
    /// Returns the second display row of the status line.
    member this.GetSecondDisplayRow mode keyPrefix message =
        let colorScheme = myRenderingContext.colorScheme
        let windowWidth = myRenderingContext.windowWidth

        let displayRow = ResizeArray windowWidth

        let modeColors =
            match mode with
            | NormalMode _ ->
                colorScheme.statusNormalMode
            | InsertMode _ ->
                colorScheme.normal
            | _ ->
                invalidOp ""

        let keyPrefixString = getKeyPrefixString keyPrefix
        let stateString     = getStateString mode
        let modeString      = getModeString mode

        let leadingSpace =
            if keyPrefixString.Length <> 0 || stateString.Length <> 0 then
                " "
            else
                ""

        let messageWidth = windowWidth - (
            leadingSpace.Length + keyPrefixString.Length + stateString.Length + modeString.Length
        )

        let messageString, messageColors =
            UserMessages.getMessageStringAndColors colorScheme message messageWidth

        this.WriteTo displayRow messageString   messageColors
        this.WriteTo displayRow leadingSpace    colorScheme.normal
        this.WriteTo displayRow keyPrefixString colorScheme.normal
        this.WriteTo displayRow stateString     colorScheme.normal
        this.WriteTo displayRow modeString      modeColors

        displayRow.GetRange (0, windowWidth)

    /// Returns the prompt user message display row.
    member this.GetPromptMessageDisplayRow message =
        let colorScheme = myRenderingContext.colorScheme
        let windowWidth = myRenderingContext.windowWidth

        let displayRow = ResizeArray windowWidth

        let messageString, messageColors =
            UserMessages.getMessageStringAndColors colorScheme message windowWidth

        this.WriteTo displayRow messageString messageColors

        displayRow.GetRange (0, windowWidth)

    // auxiliary

    member private _.GetIsRecordingColors isRecording =
        if isRecording then
            myRenderingContext.colorScheme.statusIsRecording
        else
            myRenderingContext.colorScheme.status

    member private _.GetCursorString (cursorPos: CursorPosForStatusArea) =
        let cp = {
            cursorPos with
                line        = posToUserPos cursorPos.line
                char_       = posToUserPos cursorPos.char_
                firstColumn = posToUserPos cursorPos.firstColumn
        }

        let s1 =
            if cp.selectionsCount > 1 then
                $" (%d{cp.selectionsCount}) "
            else
                " "

        let s2 = $"%5d{cp.line}:%-4d{cp.char_} %-4d{cp.firstColumn} "

        let s3 =
            // Is it tab character ?
            if cp.columnsCount <> 0 then
                if myRenderingContext.tabStop < 10 then
                    $"%-1d{cp.columnsCount} "
                else
                    $"%-2d{cp.columnsCount} "
            else
                if myRenderingContext.tabStop < 10 then
                    "  "
                else
                    "   "

        s1 + s2 + s3

    member private _.GetBufferNameString (bufferName: string) fillLength =
        if bufferName.Length <= fillLength then
            bufferName.PadRight fillLength
        elif fillLength > 2 then
            let s = Utils.endSubstring bufferName (fillLength - 2)
            ".." + s
        else
            ".."

    member private _.WriteTo displayRow s colors =
        for c in s do
            displayRow.Add { c = c; colors = colors }

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable.Dispose ()
