module ConsoleRenderer

open System
open System.Text

open ColorSchemes
open Common
open Context
open DataTypes
open KeyDispatching
open Prompt
open PromptStatus
open StatusArea
open TextArea
open UserMessages
open WrappedRef

type private DisplayRows = ResizeArray<ResizeArray<DisplayChar>>

let private getFgColor color =
    match color with
    | IndColor index ->
        $"\u001b[{30 + int index}m"
    | RGBColor (r, g, b) ->
        $"\u001b[38;2;{r};{g};{b}m"

let private getBgColor color =
    match color with
    | IndColor index ->
        $"\u001b[{40 + int index}m"
    | RGBColor (r, g, b) ->
        $"\u001b[48;2;{r};{g};{b}m"

/// Returns a terminal sequence specifying given foreground and background colors.
let getColorsSequence (colors: CharColors) =
    getFgColor colors.fg + getBgColor colors.bg

let private getPositionSequence line column =
    $"\x1b[{line+1};{column+1}H"

let private getDisplayString context (displayRows: DisplayRows) cursorTop =
    let sbResult = StringBuilder ()

    let mutable colors = context.colorScheme.normal
    let sbAux = StringBuilder context.windowWidth

    sbResult.Append (getPositionSequence cursorTop 0) |> ignore

    let mutable isFirstRows = true

    for displayRow in displayRows do
        if not isFirstRows then
            sbAux.Append '\n' |> ignore
        else
            isFirstRows <- false

        for dc in displayRow do
            let newColors = dc.colors
            if newColors <> colors then
                if sbAux.Length <> 0 then
                    sbResult.Append (getColorsSequence colors) |> ignore
                    sbResult.Append sbAux |> ignore
                    sbAux.Clear () |> ignore
                colors <- newColors

            sbAux.Append dc.c |> ignore

    if sbAux.Length <> 0 then
        sbResult.Append (getColorsSequence colors) |> ignore
        sbResult.Append sbAux |> ignore
        sbAux.Clear () |> ignore

    sbResult.Append (getPositionSequence cursorTop 0) |> ignore

    sbResult.ToString ()

[<Sealed>]
type ConsoleRenderer (
    myContextRef:   IWrappedRef<MainContext>,
    myUserMessages: UserMessages,
    myTextArea:     TextArea,
    myPrompt:       Prompt,
    myStatusArea:   StatusArea
) =
    let mutable myRenderingContext =
        makeRenderingContext myContextRef.Value
    let handleContextChanged () =
        myRenderingContext <- makeRenderingContext myContextRef.Value
    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    /// Returns areas to render indicators derived only from the application state.
    /// Areas to render after individual commands are determined by these commands.
    member _.GetAreasToRenderIndicators mode =
        let optional =
            if isPromptMode mode then
                if myPrompt.IsInCompletion () then
                    toRenderPrompt + toRenderPromptCompletions
                else
                    toRenderPrompt
            else
                if myTextArea.IsInCompletion () then
                    toRenderTextCompletions
                else
                    toRenderNothing

        toRenderStatus + optional

    /// Returns areas to render all derived only from the application state.
    /// Areas to render after individual commands are determined by these commands.
    member _.GetAreasToRenderAll mode =
        let optional =
            if isPromptMode mode then
                if myPrompt.IsInCompletion () then
                    toRenderPrompt + toRenderPromptCompletions
                else
                    toRenderPrompt
            else
                if myTextArea.IsInCompletion () then
                    toRenderTextCompletions
                else
                    toRenderNothing

        toRenderText + toRenderStatus + optional

    /// Render content of the areas specified by areasToRender.
    member this.Render mode keyPrefix isRecording (areasToRender: Set<AreaToRender>) =
        let message = myUserMessages.RetrieveMessage ()

        let toRenderStatus            = areasToRender.Contains ToRenderStatus
        let toRenderText              = areasToRender.Contains ToRenderText
        let toRenderTextCompletions   = areasToRender.Contains ToRenderTextCompletions
        let toRenderPrompt            = areasToRender.Contains ToRenderPrompt
        let toRenderPromptCompletions = areasToRender.Contains ToRenderPromptCompletions

        let firstStatusRowMessage, secondStatusRowMessage =
            if isPromptMode mode then
                (message, None)
            else
                (None, message)
                
        if toRenderText then
            this.RenderText mode

        if firstStatusRowMessage.IsSome then
            this.RenderFirstStatusRowForMessage firstStatusRowMessage            
        elif toRenderTextCompletions then
            this.RenderCompletions (myTextArea.GetCompletionsRow ())
        elif toRenderPromptCompletions then
            this.RenderCompletions (myPrompt.GetCompletionsRow ())
        elif toRenderStatus then
            this.RenderFirstStatusRowForStatus isRecording

        if toRenderPrompt then
            this.RenderPrompt mode keyPrefix
        elif toRenderStatus then
            this.RenderSecondStatusRow
                secondStatusRowMessage mode keyPrefix
           
    /// Renders content of the text area to the console.
    member private this.RenderText _mode =
        myTextArea.AdaptDisplayPosition ()

        let displayRows = myTextArea.GetDisplayRows ()
        
        this.RenderRows displayRows 0

    /// Renders content of the first row of status area for status to the console.
    member private this.RenderFirstStatusRowForStatus isRecording =
        let statusChar = myTextArea.StatusChar
        let bufferName = myTextArea.BufferName
        let cursorPos  = myTextArea.GetCursorPosForStatusArea ()

        let displayRows = ResizeArray [
            myStatusArea.GetFirstDisplayRowForStatus
                statusChar isRecording bufferName cursorPos
        ]

        this.RenderRows displayRows (myRenderingContext.statusAreaRow + 0)

    /// Renders content of the first row of status area for message to the console.
    member private this.RenderFirstStatusRowForMessage message =
        let displayRows = ResizeArray [
            myStatusArea.GetFirstDisplayRowForMessage message
        ]

        this.RenderRows displayRows (myRenderingContext.statusAreaRow + 0)

    /// Renders content of the second row of status area to the console.
    member private this.RenderSecondStatusRow message mode keyPrefix =
        let displayRows = ResizeArray [
            myStatusArea.GetSecondDisplayRow message mode keyPrefix
        ]

        this.RenderRows displayRows (myRenderingContext.statusAreaRow + 1)

    /// Renders content of prompt completions to the console.
    member private this.RenderCompletions completionsRow =
        let colorScheme = myRenderingContext.colorScheme
        let windowWidth = myRenderingContext.windowWidth

        let colors       = colorScheme.completion
        let colorsActive = colorScheme.activeCompletion

        let s, offset, length = completionsRow
        let s = s.PadRight windowWidth
        let s = s.Substring (0, windowWidth)

        let displayRow = ResizeArray (
            s |> Seq.indexed |> Seq.map (
                fun (i, c) ->
                    let colors =
                        if i >= offset && i < offset + length then
                            colorsActive;
                        else
                            colors
                    { c = c; colors = colors }
            )
        )

        let displayRows = ResizeArray [displayRow]
        this.RenderRows displayRows myRenderingContext.completionsRow

    /// Renders content of prompt status and prompt to the console.
    member private this.RenderPrompt mode keyPrefix =
        myPrompt.AdaptDisplayPosition ()

        let displayRow = ResizeArray myRenderingContext.windowWidth

        let displayChars = getPromptModeDisplayChars myRenderingContext.colorScheme mode keyPrefix
        displayRow.AddRange displayChars

        let promptDisplayRows = myPrompt.GetDisplayRows ()
        displayRow.AddRange promptDisplayRows[0]

        let displayRows = ResizeArray [displayRow]
        this.RenderRows displayRows myRenderingContext.promptRow

    member private _.RenderRows (displayRows: DisplayRows) cursorTop =
        Console.Write (
            getDisplayString myRenderingContext displayRows cursorTop
        )

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable.Dispose ()
