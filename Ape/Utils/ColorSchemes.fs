module ColorSchemes

type IndColor =
    | Black       = 0uy
    | DarkRed     = 1uy
    | DarkGreen   = 2uy
    | DarkYellow  = 3uy
    | DarkBlue    = 4uy
    | DarkMagenta = 5uy
    | DarkCyan    = 6uy
    | Gray        = 7uy
    | DarkGray    = 60uy
    | Red         = 61uy
    | Green       = 62uy
    | Yellow      = 63uy
    | Blue        = 64uy
    | Magenta     = 65uy
    | Cyan        = 66uy
    | White       = 67uy

let indColorNamesLowerMap = Map.ofArray [|
    ("black"      , IndColor.Black      )
    ("darkred"    , IndColor.DarkRed    )
    ("darkgreen"  , IndColor.DarkGreen  )
    ("darkyellow" , IndColor.DarkYellow )
    ("darkblue"   , IndColor.DarkBlue   )
    ("darkmagenta", IndColor.DarkMagenta)
    ("darkcyan"   , IndColor.DarkCyan   )
    ("gray"       , IndColor.Gray       )
    ("darkgray"   , IndColor.DarkGray   )
    ("red"        , IndColor.Red        )
    ("green"      , IndColor.Green      )
    ("yellow"     , IndColor.Yellow     )
    ("blue"       , IndColor.Blue       )
    ("magenta"    , IndColor.Magenta    )
    ("cyan"       , IndColor.Cyan       )
    ("white"      , IndColor.White      )
|]

type RGBColor = byte * byte * byte

type Color =
    | IndColor of IndColor
    | RGBColor of RGBColor

[<Struct>]
type CharColors = {
    fg: Color
    bg: Color
}

type Scheme = {
    normal:             CharColors
    lineNumber:         CharColors
    lineAfterEof:       CharColors
    selection:          CharColors
    matches:            CharColors array

    mainCursor:         CharColors
    nonMainCursor:      CharColors
    mainCursorAtEol:    CharColors
    nonMainCursorAtEol: CharColors

    status:             CharColors
    statusNormalMode:   CharColors
    statusIsRecording:  CharColors
    promptModePrefix:   CharColors
    promptInsertPaste:  CharColors
    promptRegister:     CharColors
    completion:         CharColors
    activeCompletion:   CharColors

    statusInfo:         CharColors
    statusWarning:      CharColors
    statusError:        CharColors
}

/// Color index of the main group
let mainGroupColor   = 0
/// Count of colored groups in one set
let coloredSetSize   = 3
/// Count of sets of colored groups
let coloredSetsCount = 2

let isMatchesOk (matches: CharColors array) =
    matches.Length = 1 + coloredSetsCount * coloredSetSize
    
let checkMatches (matches: CharColors array) =
    if isMatchesOk matches then
        matches
    else
        invalidOp "Wrong match colors count"
