module DefaultColorSchemes

open ColorSchemes

let private indBlack       = IndColor IndColor.Black
let private indWhite       = IndColor IndColor.White
                           
let private indGray        = IndColor IndColor.Gray
let private indRed         = IndColor IndColor.Red
let private indGreen       = IndColor IndColor.Green
let private indBlue        = IndColor IndColor.Blue
let private indCyan        = IndColor IndColor.Cyan
let private indMagenta     = IndColor IndColor.Magenta
let private indYellow      = IndColor IndColor.Yellow
                           
let private indDarkGray    = IndColor IndColor.DarkGray
let private indDarkRed     = IndColor IndColor.DarkRed
let private indDarkGreen   = IndColor IndColor.DarkGreen
let private indDarkBlue    = IndColor IndColor.DarkBlue
let private indDarkCyan    = IndColor IndColor.DarkCyan
let private indDarkMagenta = IndColor IndColor.DarkMagenta
let private indDarkYellow  = IndColor IndColor.DarkYellow
            
let darkScheme = {
    normal             = { fg = indGray     ; bg = indBlack      }
    lineNumber         = { fg = indDarkCyan ; bg = indBlack      }
    lineAfterEof       = { fg = indDarkCyan ; bg = indBlack      }
    selection          = { fg = indWhite    ; bg = indDarkBlue   }

    matches            = checkMatches [|
                         { fg = indWhite    ; bg = indMagenta    }
                         { fg = indBlack    ; bg = indGreen      }
                         { fg = indBlack    ; bg = indBlue       }
                         { fg = indBlack    ; bg = indRed        }
                         { fg = indWhite    ; bg = indGreen      }
                         { fg = indWhite    ; bg = indBlue       }
                         { fg = indWhite    ; bg = indRed        }
                         |]

    mainCursor         = { fg = indBlack    ; bg = indWhite      }
    nonMainCursor      = { fg = indBlack    ; bg = indCyan       }
    mainCursorAtEol    = { fg = indBlack    ; bg = indDarkYellow }
    nonMainCursorAtEol = { fg = indBlack    ; bg = indDarkGreen  }

    status             = { fg = indGray     ; bg = indDarkGray   }
    statusNormalMode   = { fg = indBlack    ; bg = indDarkCyan   }
    statusIsRecording  = { fg = indWhite    ; bg = indDarkRed    }
    promptModePrefix   = { fg = indDarkCyan ; bg = indBlack      }
    promptInsertPaste  = { fg = indBlack    ; bg = indGray       }
    promptRegister     = { fg = indBlack    ; bg = indGreen      }
    completion         = { fg = indBlack    ; bg = indWhite      }
    activeCompletion   = { fg = indBlack    ; bg = indCyan       }

    statusInfo         = { fg = indBlack    ; bg = indCyan       }
    statusWarning      = { fg = indBlack    ; bg = indYellow     }
    statusError        = { fg = indWhite    ; bg = indDarkRed    }
}

let lightColorScheme = {
    normal             = { fg = indBlack    ; bg = indWhite      }
    lineNumber         = { fg = indDarkBlue ; bg = indWhite      }
    lineAfterEof       = { fg = indDarkBlue ; bg = indWhite      }
    selection          = { fg = indBlack    ; bg = indCyan       }

    matches            = checkMatches [|
                         { fg = indWhite    ; bg = indMagenta    }
                         { fg = indBlack    ; bg = indGreen      }
                         { fg = indBlack    ; bg = indBlue       }
                         { fg = indBlack    ; bg = indRed        }
                         { fg = indWhite    ; bg = indGreen      }
                         { fg = indWhite    ; bg = indBlue       }
                         { fg = indWhite    ; bg = indRed        }
                         |]

    mainCursor         = { fg = indWhite    ; bg = indDarkGray   }
    nonMainCursor      = { fg = indWhite    ; bg = indBlue       }
    mainCursorAtEol    = { fg = indWhite    ; bg = indDarkYellow }
    nonMainCursorAtEol = { fg = indWhite    ; bg = indDarkGreen  }

    status             = { fg = indBlack    ; bg = indGray       }
    statusNormalMode   = { fg = indBlack    ; bg = indDarkCyan   }
    statusIsRecording  = { fg = indWhite    ; bg = indDarkRed    }
    promptModePrefix   = { fg = indBlue     ; bg = indWhite      }
    promptInsertPaste  = { fg = indBlack    ; bg = indGray       }
    promptRegister     = { fg = indBlack    ; bg = indGreen      }
    completion         = { fg = indGray     ; bg = indBlack      }
    activeCompletion   = { fg = indWhite    ; bg = indDarkBlue   }

    statusInfo         = { fg = indBlack    ; bg = indCyan       }
    statusWarning      = { fg = indBlack    ; bg = indYellow     }
    statusError        = { fg = indWhite    ; bg = indDarkRed    }
}

let private rgbBlack       = RGBColor (0x0Cuy, 0x0Cuy, 0x0Cuy)
let private rgbWhite       = RGBColor (0xF2uy, 0xF2uy, 0xF2uy)

let private rgbGray        = RGBColor (0xCCuy, 0xCCuy, 0xCCuy)
let private rgbRed         = RGBColor (0xF0uy, 0x60uy, 0x60uy)
let private rgbGreen       = RGBColor (0x3Auy, 0xB0uy, 0x3Auy)
let private rgbBlue        = RGBColor (0x40uy, 0x80uy, 0xF0uy)
let private rgbCyan        = RGBColor (0x61uy, 0xD6uy, 0xD6uy)
let private rgbMagenta     = RGBColor (0xA0uy, 0x00uy, 0xA0uy)
let private rgbYellow      = RGBColor (0xF0uy, 0xF0uy, 0x20uy)

let private rgbDarkGray    = RGBColor (0x34uy, 0x34uy, 0x34uy)
let private rgbDarkRed     = RGBColor (0xC5uy, 0x0Fuy, 0x1Fuy)
let private rgbDarkGreen   = RGBColor (0x2Auy, 0x80uy, 0x2Auy)
let private rgbDarkBlue    = RGBColor (0x00uy, 0x37uy, 0xDAuy)
let private rgbDarkCyan    = RGBColor (0x3Auy, 0x96uy, 0xDDuy)
let private rgbDarkMagenta = RGBColor (0x88uy, 0x17uy, 0x98uy)
let private rgbDarkYellow  = RGBColor (0xC1uy, 0x9Cuy, 0x00uy)

let rgbDarkScheme = {
    normal             = { fg = rgbGray     ; bg = rgbBlack      }
    lineNumber         = { fg = rgbDarkCyan ; bg = rgbBlack      }
    lineAfterEof       = { fg = rgbDarkCyan ; bg = rgbBlack      }
    selection          = { fg = rgbWhite    ; bg = rgbDarkBlue   }

    matches            = checkMatches [|
                         { fg = rgbWhite    ; bg = rgbMagenta    }
                         { fg = rgbBlack    ; bg = rgbGreen      }
                         { fg = rgbBlack    ; bg = rgbBlue       }
                         { fg = rgbBlack    ; bg = rgbRed        }
                         { fg = rgbWhite    ; bg = rgbGreen      }
                         { fg = rgbWhite    ; bg = rgbBlue       }
                         { fg = rgbWhite    ; bg = rgbRed        }
                         |]

    mainCursor         = { fg = rgbBlack    ; bg = rgbWhite      }
    nonMainCursor      = { fg = rgbBlack    ; bg = rgbCyan       }
    mainCursorAtEol    = { fg = rgbBlack    ; bg = rgbDarkYellow }
    nonMainCursorAtEol = { fg = rgbBlack    ; bg = rgbDarkGreen  }

    status             = { fg = rgbGray     ; bg = rgbDarkGray   }
    statusNormalMode   = { fg = rgbBlack    ; bg = rgbDarkCyan   }
    statusIsRecording  = { fg = rgbWhite    ; bg = rgbDarkRed    }
    promptModePrefix   = { fg = rgbDarkCyan ; bg = rgbBlack      }
    promptInsertPaste  = { fg = rgbBlack    ; bg = rgbGray       }
    promptRegister     = { fg = rgbBlack    ; bg = rgbGreen      }
    completion         = { fg = rgbBlack    ; bg = rgbWhite      }
    activeCompletion   = { fg = rgbBlack    ; bg = rgbCyan       }

    statusInfo         = { fg = rgbBlack    ; bg = rgbCyan       }
    statusWarning      = { fg = rgbBlack    ; bg = rgbYellow     }
    statusError        = { fg = rgbWhite    ; bg = rgbDarkRed    }
}

let rgbLightColorScheme = {
    normal             = { fg = rgbBlack    ; bg = rgbWhite      }
    lineNumber         = { fg = rgbDarkBlue ; bg = rgbWhite      }
    lineAfterEof       = { fg = rgbDarkBlue ; bg = rgbWhite      }
    selection          = { fg = rgbBlack    ; bg = rgbCyan       }

    matches            = checkMatches [|
                         { fg = rgbWhite    ; bg = rgbMagenta    }
                         { fg = rgbBlack    ; bg = rgbGreen      }
                         { fg = rgbBlack    ; bg = rgbBlue       }
                         { fg = rgbBlack    ; bg = rgbRed        }
                         { fg = rgbWhite    ; bg = rgbGreen      }
                         { fg = rgbWhite    ; bg = rgbBlue       }
                         { fg = rgbWhite    ; bg = rgbRed        }
                         |]

    mainCursor         = { fg = rgbWhite    ; bg = rgbDarkGray   }
    nonMainCursor      = { fg = rgbWhite    ; bg = rgbBlue       }
    mainCursorAtEol    = { fg = rgbWhite    ; bg = rgbDarkYellow }
    nonMainCursorAtEol = { fg = rgbWhite    ; bg = rgbDarkGreen  }

    status             = { fg = rgbBlack    ; bg = rgbGray       }
    statusNormalMode   = { fg = rgbBlack    ; bg = rgbDarkCyan   }
    statusIsRecording  = { fg = rgbWhite    ; bg = rgbDarkRed    }
    promptModePrefix   = { fg = rgbBlue     ; bg = rgbWhite      }
    promptInsertPaste  = { fg = rgbBlack    ; bg = rgbGray       }
    promptRegister     = { fg = rgbBlack    ; bg = rgbGreen      }
    completion         = { fg = rgbGray     ; bg = rgbBlack      }
    activeCompletion   = { fg = rgbWhite    ; bg = rgbDarkBlue   }

    statusInfo         = { fg = rgbBlack    ; bg = rgbCyan       }
    statusWarning      = { fg = rgbBlack    ; bg = rgbYellow     }
    statusError        = { fg = rgbWhite    ; bg = rgbDarkRed    }
}
