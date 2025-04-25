module Colors

type Color =
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

let private black       = Color.Black
let private white       = Color.White

let private gray        = Color.Gray
let private red         = Color.Red
let private green       = Color.Green
let private blue        = Color.Blue
let private cyan        = Color.Cyan
let private magenta     = Color.Magenta
let private yellow      = Color.Yellow

let private darkGray    = Color.DarkGray
let private darkRed     = Color.DarkRed
let private darkGreen   = Color.DarkGreen
let private darkBlue    = Color.DarkBlue
let private darkCyan    = Color.DarkCyan
let private darkMagenta = Color.DarkMagenta
let private darkYellow  = Color.DarkYellow

/// Color index of the main group.
let mainGroupColor = 0

/// Count of supported colored groups. Must equal Scheme.matches.Length - 1.
let colorGroupsCount = 3

let private checkMatches (matches: CharColors array) =
    if matches.Length <> 1 + colorGroupsCount then
        invalidOp "Wrong match colors count"
    else
        matches

// supported color schemes

let darkScheme = {
    normal             = { fg = gray     ; bg = black      }
    lineNumber         = { fg = darkCyan ; bg = black      }
    lineAfterEof       = { fg = darkCyan ; bg = black      }
    selection          = { fg = white    ; bg = darkBlue   }

    matches            = checkMatches [|
                         { fg = white    ; bg = magenta    }
                         { fg = black    ; bg = green      }
                         { fg = black    ; bg = blue       }
                         { fg = black    ; bg = red        }
                         |]

    mainCursor         = { fg = black    ; bg = white      }
    nonMainCursor      = { fg = black    ; bg = cyan       }
    mainCursorAtEol    = { fg = black    ; bg = darkYellow }
    nonMainCursorAtEol = { fg = black    ; bg = darkGreen  }

    status             = { fg = gray     ; bg = darkGray   }
    statusNormalMode   = { fg = black    ; bg = darkCyan   }
    statusIsRecording  = { fg = white    ; bg = darkRed    }
    promptModePrefix   = { fg = darkCyan ; bg = black      }
    promptInsertPaste  = { fg = black    ; bg = gray       }
    promptRegister     = { fg = black    ; bg = green      }
    completion         = { fg = black    ; bg = white      }
    activeCompletion   = { fg = black    ; bg = cyan       }

    statusInfo         = { fg = black    ; bg = cyan       }
    statusWarning      = { fg = black    ; bg = yellow     }
    statusError        = { fg = white    ; bg = darkRed    }
}

let lightColorScheme = {
    normal             = { fg = black    ; bg = white      }
    lineNumber         = { fg = darkBlue ; bg = white      }
    lineAfterEof       = { fg = darkBlue ; bg = white      }
    selection          = { fg = black    ; bg = cyan       }

    matches            = checkMatches [|
                         { fg = white    ; bg = magenta    }
                         { fg = black    ; bg = green      }
                         { fg = black    ; bg = blue       }
                         { fg = black    ; bg = red        }
                         |]

    mainCursor         = { fg = white    ; bg = darkGray   }
    nonMainCursor      = { fg = white    ; bg = blue       }
    mainCursorAtEol    = { fg = white    ; bg = darkYellow }
    nonMainCursorAtEol = { fg = white    ; bg = darkGreen  }

    status             = { fg = black    ; bg = gray       }
    statusNormalMode   = { fg = black    ; bg = darkCyan   }
    statusIsRecording  = { fg = white    ; bg = darkRed    }
    promptModePrefix   = { fg = blue     ; bg = white      }
    promptInsertPaste  = { fg = black    ; bg = gray       }
    promptRegister     = { fg = black    ; bg = green      }
    completion         = { fg = gray     ; bg = black      }
    activeCompletion   = { fg = white    ; bg = darkBlue   }

    statusInfo         = { fg = black    ; bg = cyan       }
    statusWarning      = { fg = black    ; bg = yellow     }
    statusError        = { fg = white    ; bg = darkRed    }
}

let schemesArray = [|
    "dark"
    "light"
|]

let defaultScheme = "dark"

let schemesMap = Map [
    "dark"  , darkScheme
    "light" , lightColorScheme
]
