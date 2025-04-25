module CompletionItems

open System

open Common
open CompletionUtils
open Context
open DataTypes
open Position
open UserMessages
open WrappedRef

/// CompletionItems manages completion items.

type CompletionItems (
    myContextRef:   IWrappedRef<ConsoleContext>,
    myUserMessages: UserMessages,
    myGetCompletionsFun: GetCompletionsFun
) as this_ =

    let mutable myItems        = ResizeArray ()
    let mutable myItemsString  = ""
    let mutable myItemsIndices = ResizeArray<(int * int)> ()
    let mutable myCurrent      = 0

    let handleContextChanged () =
        if myItems.Count <> 0 then
            this_.SetItemsStringAndIndices ()

    let myContextChangedDisposable =
        myContextRef.Subscribe handleContextChanged

    interface ICompletionItems with
        member this.TrySet (itemsToComplete: (Chars * Position) seq) =
            if myItems.Count <> 0 then
                invalidOp "TrySet not preceded by Clear"

            match myGetCompletionsFun itemsToComplete with
            | Ok (prefixToComplete, completions) -> 
                myItems <- ResizeArray (seq {
                    yield  (Complete prefixToComplete)
                    yield! completions
                })
                this.SetItemsStringAndIndices ()
                myCurrent <- 0
            | Error e ->
                myUserMessages.RegisterMessage (makeErrorMessage e)

        member _.Clear () =
            myItems        <- ResizeArray ()
            myItemsString  <- ""
            myItemsIndices <- ResizeArray ()
            myCurrent      <- 0

        member _.IsInCompletion () =
            myCurrent <> 0

        member this.GetNext () =
            if myCurrent < myItems.Count - 1 then
                let previous = myCurrent
                myCurrent <- myCurrent + 1
                Some (this.GetResultLine previous)
            else
                None

        member this.GetPrevious () =
            if myCurrent > 0 then
                let previous = myCurrent
                myCurrent <- myCurrent - 1
                Some (this.GetResultLine previous)
            else
                None

        member _.GetCompletionsRow () =
            if myCurrent = 0 then
                invalidOp ""

            let width = myContextRef.Value.windowWidth
            let offset, length = myItemsIndices[myCurrent - 1]
            let lineOffset = offset - offset % width

            let sLength = min width (myItemsString.Length - lineOffset)
            let s = myItemsString.Substring (lineOffset, sLength)

            (s, offset - lineOffset, length)

    member private _.SetItemsStringAndIndices () =
        let items = ResizeArray (
            myItems |> Seq.map getStringForCompletionList
        )
        let strings, indices =
            getItemsStringAndIndices myContextRef.Value.windowWidth items
        myItemsString  <- strings
        myItemsIndices <- indices

    member private this.GetResultLine (previous: int) =
        let toDelete = this.GetStringForComplete previous
        let toInsert = this.GetStringForComplete myCurrent

        {
            toDelete = toDelete.Length
            toInsert = stringToChars toInsert
        }

    member private _.GetStringForComplete (itemIndex: int) : string =
        match myItems[itemIndex] with
        | Complete x -> x
        | ListOnly _ -> getStringFromComplete myItems[0]

    // IDisposable

    interface IDisposable with
        member _.Dispose () =
            myContextChangedDisposable.Dispose ()
