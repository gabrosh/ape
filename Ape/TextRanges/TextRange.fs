module TextRange

open Position
open TextRangesModifier

// TextRange

type TextRange = {
    first: Position
    last:  Position
}
with
    // applying multi-line insertion

    member private this.MoveRight (insertSpec: InsertSpec) =
        {
            first = applyInsertToPos insertSpec this.first
            last  = applyInsertToPos insertSpec this.last
        }

    member private this.Widen (insertSpec: InsertSpec) =
        {
            first = this.first
            last  = applyInsertToPos insertSpec this.last
        }

    // applying multi-line deletion

    member private this.MoveLeft (deleteSpec: DeleteSpec) =
        {
            first = applyDeleteToPos deleteSpec this.first
            last  = applyDeleteToPos deleteSpec this.last
        }

    member private _this.Reduce (deleteSpec: DeleteSpec) =
        {
            first = deleteSpec.first
            last  = deleteSpec.first
        }

    member private this.CutLeft (deleteSpec: DeleteSpec) =
        {
            first = applyDeleteToPos deleteSpec deleteSpec.rightKept
            last  = applyDeleteToPos deleteSpec this.last
        }

    member private this.CutRight (deleteSpec: DeleteSpec) =
        {
            first = this.first
            last  = deleteSpec.leftKept
        }

    member private this.CutMiddle (deleteSpec: DeleteSpec) =
        {
            first = this.first
            last  = applyDeleteToPos deleteSpec this.last
        }

    interface ITextRange with
        member this.First = this.first
        member this.Last  = this.last

        /// Returns item prepared to be used in TextRangesModifier.
        member this.PrepareItem newLinesDelta =
            {
                first = this.first.Add newLinesDelta 0
                last  = this.last .Add newLinesDelta 0
            }

        /// Returns a new text range created by modifying text range according to insertSpec.
        member this.ApplyInsert (insertSpec: InsertSpec) =
            let target = insertSpec.target

            let first, last = this.first, this.last

            if   target > last         then this
            elif target < first        then this.MoveRight insertSpec
            elif target > first        then this.Widen insertSpec
            elif insertSpec.preferMove then this.MoveRight insertSpec
                                       else this.Widen insertSpec

        /// Returns a new text range created by modifying text range according to deleteSpec,
        /// and a flag specifying if the text range has been eliminated by modifying it.
        member this.ApplyDelete (deleteSpec: DeleteSpec) =
            let first     = deleteSpec.first
            let firstKept = deleteSpec.rightKept

            let selFirst, selLast = this.first, this.last

            if first > selLast then
                (false, this)

            elif firstKept <= selFirst then
                (false, this.MoveLeft  deleteSpec)

            elif first <= selFirst && firstKept >  selLast then
                (true , this.Reduce    deleteSpec)

            elif first <= selFirst && firstKept <= selLast then
                (false, this.CutLeft   deleteSpec)

            elif first >  selFirst && firstKept >  selLast then
                (false, this.CutRight  deleteSpec)
            else
                (false, this.CutMiddle deleteSpec)

        /// Returns a new text range created by modifying text range according to one-line insertSpec.
        member this.ApplyOneLineInsert (insertSpec: OneLineInsertSpec) =
            let first = this.first
            let last  = this.last

            let isFirstAffected =
                if insertSpec.preferMove then
                    insertSpec.line = first.line && insertSpec.target <= first.char
                else
                    insertSpec.line = first.line && insertSpec.target < first.char

            let isLastAffected =
                insertSpec.line = last.line && insertSpec.target <= last.char

            if isFirstAffected || isLastAffected then
                let first =
                    if isFirstAffected
                    then applyOneLineInsertToPos insertSpec first
                    else first
                let last =
                    if isLastAffected
                    then applyOneLineInsertToPos insertSpec last
                    else last

                {
                    first = first
                    last  = last
                }
            else
                this

        /// Returns a new text range created by modifying text range according to one-line deleteSpec,
        /// and a flag specifying if the text range has been eliminated by modifying it.
        member this.ApplyOneLineDelete (deleteSpec: OneLineDeleteSpec) =
            let first = this.first
            let last  = this.last

            let isFirstAffected =
                deleteSpec.line = first.line && deleteSpec.first < first.char

            let isLastAffected =
                deleteSpec.line = last.line && deleteSpec.first < last.char

            if isFirstAffected || isLastAffected then
                let first =
                    if isFirstAffected
                    then applyOneLineDeleteToPos deleteSpec first
                    else first
                let last =
                    if isLastAffected
                    then applyOneLineDeleteToPos deleteSpec last
                    else last

                let isEliminated = (first = last)

                let range = {
                    first = first
                    last  = last
                }

                (isEliminated, range)
            else
                (false, this)

let TextRange_Invalid = {
    first = Position_Invalid
    last  = Position_Invalid
}
