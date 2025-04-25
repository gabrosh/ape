namespace PromptDelegators

open Common
open Position

type PromptBasic (
    inBasicState: BasicState,
    mySelections: Selections.Selections,
    myDispatcher: PromptDispatcher.PromptDispatcher
) =
    inherit PromptDelegator (inBasicState)

    // basic commands

    member this.PerformOnSelection isExtending command =
        this.LoadBasicState ()

        this.DelegateBasicCommand isExtending command

        this.StoreBasicState ()

    // delegating

    // a simplified version of this method exists in PromptModifying class
    member private this.DelegateBasicCommand isExtending command =
        let state = {
            selection         = mySelections[0]
            displayLine       = this.DisplayPos.line
            displayLineRow    = this.DisplayPos.lineRow
            displayColumn     = this.DisplayPos.column
            isSingleSelection = true
            prevCommand       = Some command
        }

        let outState = myDispatcher.PerformCommand state command

        if outState.toUpdateSelection then
            let position = { line = outState.line; char = outState.char }
            mySelections[0] <-
                if not isExtending then
                    mySelections[0].WithCursorAndAnchorEqual position
                else
                    mySelections[0].WithCursor position

        this.DisplayPos <- {
            line    = outState.displayLine
            lineRow = outState.displayLineRow
            column  = outState.displayColumn
        }
