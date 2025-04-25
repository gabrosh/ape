namespace PromptDelegators

open Common

type BasicState = {
    mutable displayPos: DisplayPos
}

/// PromptDelegator is the base class for all prompt delegators.

type PromptDelegator (
    myBasicState: BasicState
) =
    member val DisplayPos = DisplayPos_Invalid
        with get, set

    member internal this.LoadBasicState () =
        this.DisplayPos <- myBasicState.displayPos

    member internal this.StoreBasicState () =
        myBasicState.displayPos <- this.DisplayPos

        this.DisplayPos <- DisplayPos_Invalid
