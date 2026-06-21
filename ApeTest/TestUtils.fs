module TestUtils

open Context
open WrappedRef

let makeContextRef windowWidth windowHeight =
    WrappedRef {
        windowWidth      = windowWidth
        textAreaHeight   = windowHeight - 2
        statusAreaRow    = windowHeight - 2
        completionsRow   = windowHeight - 2
        promptRow        = windowHeight - 1

        colorScheme      = DefaultColors.lightScheme
        keySequenceSleep = 0
        readOnly         = false
        reloadAsLogFile  = false
        maxSavedUndos    = 25
        reSearchMatching = true
        recursionLimit   = 1000
        wrapLines        = false
        wrapAtWord       = false
        showLineNumbers  = false
        tabStop          = 4
        tabBySpaces      = true
        scrollOffset     = 3
        cursorBeforeEol  = false
    }

let makeContextRef_wrapLines windowWidth windowHeight wrapAtWord =
    WrappedRef {
        windowWidth      = windowWidth
        textAreaHeight   = windowHeight - 2
        statusAreaRow    = windowHeight - 2
        completionsRow   = windowHeight - 2
        promptRow        = windowHeight - 1

        colorScheme      = DefaultColors.lightScheme
        keySequenceSleep = 0
        readOnly         = false
        reloadAsLogFile  = false
        maxSavedUndos    = 25
        reSearchMatching = true
        recursionLimit   = 1000
        wrapLines        = true
        wrapAtWord       = wrapAtWord
        showLineNumbers  = false
        tabStop          = 4
        tabBySpaces      = true
        scrollOffset     = 3
        cursorBeforeEol  = false
    }

let makeConsoleContextRef windowWidth windowHeight  =
    WrappedRef {
        windowWidth  = windowWidth
        windowHeight = windowHeight
    }    

let makeExtraContextRef modeLength =
    WrappedRef {
        modeLength = modeLength
    }

let set_cursorBeforeEol (contextRef: WrappedRef<MainContext>) value =
    contextRef.Value <- {
        contextRef.Value with cursorBeforeEol = value
    }
