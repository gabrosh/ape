module Context

open ConsoleInterop_Common
open DataTypes
open Settings

type ConsoleContext = {
    windowWidth:         int
    windowHeight:        int
}

type SettingsContext = {
    colorScheme:         Colors.Scheme
    readOnly:            bool
    reloadAsLogFile:     bool
    maxSavedUndos:       int
    reSearchMatching:    bool
    recursionLimit:      int
    wrapLines:           bool
    wrapAtWord:          bool
    showLineNumbers:     bool
    tabStop:             int
    tabBySpaces:         bool
    scrollOffset:        int
    cursorBeforeEol:     bool
}

type MainContext = {
    colorScheme:         Colors.Scheme

    windowWidth:         int
    textAreaHeight:      int
    statusAreaRow:       int
    completionsRow:      int
    promptRow:           int

    readOnly:            bool
    reloadAsLogFile:     bool
    maxSavedUndos:       int
    reSearchMatching:    bool
    recursionLimit:      int
    wrapLines:           bool
    wrapAtWord:          bool
    showLineNumbers:     bool
    tabStop:             int
    tabBySpaces:         bool
    scrollOffset:        int
    cursorBeforeEol:     bool
}

type ExtraContext = {
    modeLength:          int
}

type AreaContext = {
    colorScheme:         Colors.Scheme

    linesCount:          int
    lineNumbersWidth:    int
    textWidth:           int
    areaHeight:          int

    wrapLines:           bool
    wrapAtWord:          bool
    showLineNumbers:     bool
    tabStop:             int
    tabBySpaces:         bool
    scrollOffsetRows:    int
    scrollOffsetColumns: int
    cursorBeforeEol:     bool
}

type RenderingContext = {
    colorScheme:         Colors.Scheme
    windowWidth:         int
    statusAreaRow:       int
    completionsRow:      int
    promptRow:           int

    tabStop:             int
}

// making various kinds of context

let makeConsoleContext windowSize =
    {
        windowWidth  = windowSize.width
        windowHeight = windowSize.height
    }

let private makeSettingsContext (settings: Settings) =
    let colorSchemeName = getValueString settings Name.colorScheme

    {
        colorScheme      = Colors.schemesMap[colorSchemeName]
        readOnly         = getValueBool settings Name.readOnly
        reloadAsLogFile  = getValueBool settings Name.reloadAsLogFile
        maxSavedUndos    = getValueInt  settings Name.maxSavedUndos
        reSearchMatching = getValueBool settings Name.reSearchMatching
        recursionLimit   = getValueInt  settings Name.recursionLimit
        wrapLines        = getValueBool settings Name.wrapLines
        wrapAtWord       = getValueBool settings Name.wrapAtWord
        showLineNumbers  = getValueBool settings Name.showLineNumbers
        tabStop          = getValueInt  settings Name.tabStop
        tabBySpaces      = getValueBool settings Name.tabBySpaces
        scrollOffset     = getValueInt  settings Name.scrollOffset
        cursorBeforeEol  = getValueBool settings Name.cursorBeforeEol
    }

let makeMainContext (consoleContext: ConsoleContext) (settings: Settings) =
    let windowWidth  = consoleContext.windowWidth
    let windowHeight = consoleContext.windowHeight

    let settingsContext = makeSettingsContext settings

    {
        colorScheme      = settingsContext.colorScheme

        windowWidth      = windowWidth
        textAreaHeight   = windowHeight - 2
        statusAreaRow    = windowHeight - 2
        completionsRow   = windowHeight - 2
        promptRow        = windowHeight - 1

        readOnly         = settingsContext.readOnly
        reloadAsLogFile  = settingsContext.reloadAsLogFile
        maxSavedUndos    = settingsContext.maxSavedUndos
        reSearchMatching = settingsContext.reSearchMatching
        recursionLimit   = settingsContext.recursionLimit
        wrapLines        = settingsContext.wrapLines
        wrapAtWord       = settingsContext.wrapAtWord
        showLineNumbers  = settingsContext.showLineNumbers
        tabStop          = settingsContext.tabStop
        tabBySpaces      = settingsContext.tabBySpaces
        scrollOffset     = settingsContext.scrollOffset
        cursorBeforeEol  = settingsContext.cursorBeforeEol
    }

let makeExtraContext (modeLength: int) =
    {
        modeLength = modeLength
    }

let makeTextAreaContext (mainContext: MainContext) (lines: Lines) =
    let lineNumbersWidth =
        getLineNumbersWidth mainContext.showLineNumbers lines.Count

    let textWidth  = mainContext.windowWidth - lineNumbersWidth
    let areaHeight = mainContext.textAreaHeight

    let maxScrollOffsetRows    = (areaHeight - 1) / 2
    let maxScrollOffsetColumns = (textWidth  - 1) / 2

    {
        colorScheme         = mainContext.colorScheme

        linesCount          = lines.Count
        lineNumbersWidth    = lineNumbersWidth
        textWidth           = textWidth
        areaHeight          = areaHeight

        wrapLines           = mainContext.wrapLines
        wrapAtWord          = mainContext.wrapAtWord
        showLineNumbers     = mainContext.showLineNumbers
        tabStop             = mainContext.tabStop
        tabBySpaces         = mainContext.tabBySpaces
        scrollOffsetRows    = min maxScrollOffsetRows    mainContext.scrollOffset
        scrollOffsetColumns = min maxScrollOffsetColumns mainContext.scrollOffset
        cursorBeforeEol     = mainContext.cursorBeforeEol
    }

let makePromptContext (mainContext: MainContext) (extraContext: ExtraContext) =
    let textWidth  = mainContext.windowWidth - extraContext.modeLength
    let areaHeight = 1

    let maxScrollOffsetRows    = (areaHeight - 1) / 2
    let maxScrollOffsetColumns = (textWidth  - 1) / 2

    {
        colorScheme         = mainContext.colorScheme

        linesCount          = 1
        lineNumbersWidth    = 0
        textWidth           = textWidth
        areaHeight          = areaHeight

        wrapLines           = false
        wrapAtWord          = false
        showLineNumbers     = false
        tabStop             = mainContext.tabStop
        tabBySpaces         = mainContext.tabBySpaces
        scrollOffsetRows    = min maxScrollOffsetRows    mainContext.scrollOffset
        scrollOffsetColumns = min maxScrollOffsetColumns mainContext.scrollOffset
        cursorBeforeEol     = mainContext.cursorBeforeEol
    }

let makeRendereringContext (mainContext: MainContext) =
    {
        colorScheme         = mainContext.colorScheme
        windowWidth         = mainContext.windowWidth
        statusAreaRow       = mainContext.statusAreaRow
        completionsRow      = mainContext.completionsRow
        promptRow           = mainContext.promptRow
        tabStop             = mainContext.tabStop
    }
