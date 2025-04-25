namespace TextAreaDelegator

open Commands.InCommands
open Common

type BasicState = {
    mutable displayPos:  DisplayPos
    mutable prevCommand: InCommand option
}

/// TextAreaDelegator is the base class for all text area delegators.

type TextAreaDelegator (
    myBasicState: BasicState
) =
    member val internal DisplayPos = DisplayPos_Invalid
        with get, set

    member val internal PrevCommand = None
        with get, set

    member internal this.LoadBasicState () =
        this.DisplayPos  <- myBasicState.displayPos
        this.PrevCommand <- myBasicState.prevCommand

    member internal this.StoreBasicState () =
        myBasicState.displayPos  <- this.DisplayPos
        myBasicState.prevCommand <- this.PrevCommand

        this.DisplayPos  <- DisplayPos_Invalid
        this.PrevCommand <- None
