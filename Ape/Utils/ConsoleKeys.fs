module ConsoleKeys

open System

type InputKey =
  | Backspace    = 8
  | Tab          = 9
  | Enter        = 13
  | Escape       = 27
  | Spacebar     = 32

  | Exclamation  = 33
  | Quotation    = 34
  | Hash         = 35
  | Dollar       = 36
  | Percent      = 37
  | Ampersand    = 38
  | Apostrophe   = 39
  | LeftRound    = 40
  | RightRound   = 41
  | Asterisk     = 42
  | Plus         = 43
  | Comma        = 44
  | Minus        = 45
  | Period       = 46
  | Slash        = 47

  | D0           = 48
  | D1           = 49
  | D2           = 50
  | D3           = 51
  | D4           = 52
  | D5           = 53
  | D6           = 54
  | D7           = 55
  | D8           = 56
  | D9           = 57

  | Colon        = 58
  | Semicolon    = 59
  | LessThan     = 60
  | Equal        = 61
  | GreaterThan  = 62
  | Question     = 63
  | At           = 64

  | A            = 65
  | B            = 66
  | C            = 67
  | D            = 68
  | E            = 69
  | F            = 70
  | G            = 71
  | H            = 72
  | I            = 73
  | J            = 74
  | K            = 75
  | L            = 76
  | M            = 77
  | N            = 78
  | O            = 79
  | P            = 80
  | Q            = 81
  | R            = 82
  | S            = 83
  | T            = 84
  | U            = 85
  | V            = 86
  | W            = 87
  | X            = 88
  | Y            = 89
  | Z            = 90

  | LeftSquare   = 91
  | Backslash    = 92
  | RightSquare  = 93
  | Caret        = 94
  | Underscore   = 95
  | Backtick     = 96

  | LeftCurly    = 123
  | Pipe         = 124
  | RightCurly   = 125
  | Tilde        = 126

  | PageUp       = 233
  | PageDown     = 234
  | End          = 235
  | Home         = 236
  | LeftArrow    = 237
  | UpArrow      = 238
  | RightArrow   = 239
  | DownArrow    = 240
  | Insert       = 241
  | Delete       = 242

  | F1           = 243
  | F2           = 244
  | F3           = 245
  | F4           = 246
  | F5           = 247
  | F6           = 248
  | F7           = 249
  | F8           = 250
  | F9           = 251
  | F10          = 252
  | F11          = 253
  | F12          = 254

let private specialToInputKey = Map [
    ConsoleKey.Backspace  , InputKey.Backspace
    ConsoleKey.Tab        , InputKey.Tab
    ConsoleKey.Enter      , InputKey.Enter
    ConsoleKey.Escape     , InputKey.Escape
    ConsoleKey.Spacebar   , InputKey.Spacebar
    ConsoleKey.PageUp     , InputKey.PageUp
    ConsoleKey.PageDown   , InputKey.PageDown
    ConsoleKey.End        , InputKey.End
    ConsoleKey.Home       , InputKey.Home
    ConsoleKey.LeftArrow  , InputKey.LeftArrow
    ConsoleKey.UpArrow    , InputKey.UpArrow
    ConsoleKey.RightArrow , InputKey.RightArrow
    ConsoleKey.DownArrow  , InputKey.DownArrow
    ConsoleKey.Insert     , InputKey.Insert
    ConsoleKey.Delete     , InputKey.Delete

    ConsoleKey.F1         , InputKey.F1
    ConsoleKey.F2         , InputKey.F2
    ConsoleKey.F3         , InputKey.F3
    ConsoleKey.F4         , InputKey.F4
    ConsoleKey.F5         , InputKey.F5
    ConsoleKey.F6         , InputKey.F6
    ConsoleKey.F7         , InputKey.F7
    ConsoleKey.F8         , InputKey.F8
    ConsoleKey.F9         , InputKey.F9
    ConsoleKey.F10        , InputKey.F10
    ConsoleKey.F11        , InputKey.F11
    ConsoleKey.F12        , InputKey.F12
]

let symbolToInputKey = Map [
    '!' , InputKey.Exclamation
    '"' , InputKey.Quotation
    '#' , InputKey.Hash
    '$' , InputKey.Dollar
    '%' , InputKey.Percent
    '&' , InputKey.Ampersand
    ''' , InputKey.Apostrophe
    '(' , InputKey.LeftRound
    ')' , InputKey.RightRound
    '*' , InputKey.Asterisk
    '+' , InputKey.Plus
    ',' , InputKey.Comma
    '-' , InputKey.Minus
    '.' , InputKey.Period
    '/' , InputKey.Slash

    ':' , InputKey.Colon
    ';' , InputKey.Semicolon
    '<' , InputKey.LessThan
    '=' , InputKey.Equal
    '>' , InputKey.GreaterThan
    '?' , InputKey.Question
    '@' , InputKey.At

    '[' , InputKey.LeftSquare
    '\\', InputKey.Backslash
    ']' , InputKey.RightSquare
    '^' , InputKey.Caret
    '_' , InputKey.Underscore
    '`' , InputKey.Backtick

    '{' , InputKey.LeftCurly
    '|' , InputKey.Pipe
    '}' , InputKey.RightCurly
    '~' , InputKey.Tilde
]

let specialToChar = Map [
    InputKey.Tab         , '\t'
    InputKey.Spacebar    , ' '
]

let symbolToChar = Map [
    InputKey.Exclamation , '!'
    InputKey.Quotation   , '"'
    InputKey.Hash        , '#'
    InputKey.Dollar      , '$'
    InputKey.Percent     , '%'
    InputKey.Ampersand   , '&'
    InputKey.Apostrophe  , '''
    InputKey.LeftRound   , '('
    InputKey.RightRound  , ')'
    InputKey.Asterisk    , '*'
    InputKey.Plus        , '+'
    InputKey.Comma       , ','
    InputKey.Minus       , '-'
    InputKey.Period      , '.'
    InputKey.Slash       , '/'

    InputKey.Colon       , ':'
    InputKey.Semicolon   , ';'
    InputKey.LessThan    , '<'
    InputKey.Equal       , '='
    InputKey.GreaterThan , '>'
    InputKey.Question    , '?'
    InputKey.At          , '@'

    InputKey.LeftSquare  , '['
    InputKey.Backslash   , '\\'
    InputKey.RightSquare , ']'
    InputKey.Caret       , '^'
    InputKey.Underscore  , '_'
    InputKey.Backtick    , '`'

    InputKey.LeftCurly   , '{'
    InputKey.Pipe        , '|'
    InputKey.RightCurly  , '}'
    InputKey.Tilde       , '~'
]

type Key =
    | NoModif      of InputKey
    | Ctrl         of InputKey
    | Alt          of InputKey
    | CtrlAlt      of InputKey
    | Shift        of InputKey
    | ShiftCtrl    of InputKey
    | ShiftAlt     of InputKey
    | ShiftCtrlAlt of InputKey
    | CharNoModif  of char

let private modNoModif      = enum<ConsoleModifiers> 0
let private modCtrl         = ConsoleModifiers.Control
let private modAlt          = ConsoleModifiers.Alt
let private modCtrlAlt      = ConsoleModifiers.Control ||| ConsoleModifiers.Alt
let private modShift        = ConsoleModifiers.Shift
let private modShiftCtrl    = ConsoleModifiers.Shift ||| modCtrl
let private modShiftAlt     = ConsoleModifiers.Shift ||| modAlt
let private modShiftCtrlAlt = ConsoleModifiers.Shift ||| modCtrlAlt

let private noModifOrShift modifs =
       modifs = modNoModif
    || modifs = modShift

let private includesEitherCtrlOrAlt modifs =
       modifs &&& modCtrlAlt = modCtrl
    || modifs &&& modCtrlAlt = modAlt

// getModifier... functions

let private modifierFuns = Map [
//                      NoClear        ClearShift ClearShiftAndCtrlAlt ForChar
    modNoModif      , ( NoModif      , NoModif  , NoModif ,            CharNoModif )
    modCtrl         , ( Ctrl         , Ctrl     , Ctrl    ,            CharNoModif )
    modAlt          , ( Alt          , Alt      , Alt     ,            CharNoModif )
    modCtrlAlt      , ( CtrlAlt      , CtrlAlt  , NoModif ,            CharNoModif )
    modShift        , ( Shift        , NoModif  , NoModif ,            CharNoModif )
    modShiftCtrl    , ( ShiftCtrl    , Ctrl     , Ctrl    ,            CharNoModif )
    modShiftAlt     , ( ShiftAlt     , Alt      , Alt     ,            CharNoModif )
    modShiftCtrlAlt , ( ShiftCtrlAlt , CtrlAlt  , NoModif ,            CharNoModif )
]

let private getModifierFun_NoClear modifs =
    let func, _, _, _ = modifierFuns[modifs]
    func

let private getModifierFun_ClearShift modifs =
    let _, func, _, _ = modifierFuns[modifs]
    func

let private getModifierFun_ClearShiftAndCtrlAlt modifs =
    let _, _, func, _ = modifierFuns[modifs]
    func

let private getModifierFun_ForChar modifs =
    let _, _, _, func = modifierFuns[modifs]
    func

// consoleKey or keyChar recognizers

let private aA_delta = 'a' - 'A'

let private consoleKeyToInputKey consoleKey =
    enum<InputKey> (int consoleKey)

let private keyCharToInputKey keyChar =
    enum<InputKey> (int keyChar)

/// Recognizes keys like Enter and shortcuts like Ctrl-Enter, AltCtrl-Enter.
let private recognizeSpecial modifs consoleKey _keyChar (_capsLock: bool) =
    let found, inputKey = specialToInputKey.TryGetValue consoleKey
    if found then
        Some (getModifierFun_NoClear modifs inputKey)
    else
        None

/// Recognizes shortcuts like Alt-a, Ctrl-a, Alt-A, Ctrl-A.
let private recognizeLetterKey modifs consoleKey _keyChar (capsLock: bool) =
    if ConsoleKey.A <= consoleKey && consoleKey <= ConsoleKey.Z
       && includesEitherCtrlOrAlt modifs
    then
        let inputKey = consoleKeyToInputKey consoleKey
        let modifs =
            if capsLock then
                modifs ^^^ ConsoleModifiers.Shift
            else
                modifs
        Some (getModifierFun_NoClear modifs inputKey)
    else
        None

/// Recognizes characters like a, A.
let private recognizeLetterChar modifs _consoleKey keyChar (_capsLock: bool) =
    if 'A' <= keyChar && keyChar <= 'Z'
        && noModifOrShift modifs
    then
        let inputKey = keyCharToInputKey keyChar
        Some (Shift inputKey)

    elif 'a' <= keyChar && keyChar <= 'z'
        && noModifOrShift modifs
    then
        let inputKey = keyCharToInputKey (keyChar - aA_delta)
        Some (NoModif inputKey)

    else
        None

/// Recognizes symbol characters from ASCII table.
let private recognizeSymbol modifs _consoleKey keyChar (_capsLock: bool) =
    let found, inputKey = symbolToInputKey.TryGetValue keyChar
    if found then
        Some (getModifierFun_ClearShiftAndCtrlAlt modifs inputKey)
    else
        None

/// Recognizes shortcuts like Alt-0, Ctrl-0.
let private recognizeDigitKey modifs consoleKey _keyChar (_capsLock: bool) =
    if ConsoleKey.D0 <= consoleKey && consoleKey <= ConsoleKey.D9
       && includesEitherCtrlOrAlt modifs
    then
        let inputKey = consoleKeyToInputKey consoleKey
        Some (getModifierFun_NoClear modifs inputKey)
    else
        None

/// Recognizes characters like 0.
let private recognizeDigitChar modifs _consoleKey keyChar (_capsLock: bool) =
    if '0' <= keyChar && keyChar <= '9'
        && noModifOrShift modifs
    then
        let inputKey = keyCharToInputKey keyChar
        Some (getModifierFun_ClearShift modifs inputKey)
    else
        None

/// Recognizes all other characters.
let private recognizeChar modifs _consoleKey keyChar _capsLock =
    Some (getModifierFun_ForChar modifs keyChar)

let private inputKeyRecognizers = [|
    recognizeSpecial
    recognizeLetterKey
    recognizeLetterChar
    recognizeSymbol      // Symbols typed on digit keys have priority before Shift-Alt-0 to Shift-Alt-9.
    recognizeDigitKey
    recognizeDigitChar
    recognizeChar
|]

/// Returns Key corresponding to ConsoleKeyInfo.
let keyInfoToKey (keyInfo: ConsoleKeyInfo) capsLock =
    let modifs     = keyInfo.Modifiers
    let consoleKey = keyInfo.Key
    let keyChar    = keyInfo.KeyChar

    inputKeyRecognizers |> Seq.pick (
        fun f -> f modifs consoleKey keyChar capsLock
    )

/// Key to char converters

let private convertSpecialToChar key =
    match key with
    | NoModif inputKey ->
        Utils.optionOfPair (specialToChar.TryGetValue inputKey)
    | _ ->
        None

let private convertLetterToChar key =
    match key with
    | NoModif inputKey
        when InputKey.A <= inputKey && inputKey <= InputKey.Z ->
            Some (char (int inputKey) + aA_delta)
    | Shift inputKey
        when InputKey.A <= inputKey && inputKey <= InputKey.Z ->
            Some (char (int inputKey))
    | _ ->
        None

let private convertSymbolToChar key =
    match key with
    | NoModif inputKey ->
        Utils.optionOfPair (symbolToChar.TryGetValue inputKey)
    | _ ->
        None

let private convertDigitToChar key =
    match key with
    | NoModif inputKey
        when InputKey.D0 <= inputKey && inputKey <= InputKey.D9 ->
            Some (char (int inputKey))
    | _ ->
        None

let private convertCharToChar key =
    match key with
    | CharNoModif c -> Some c
    | _             -> None

let private convertersToChar = [|
    convertSpecialToChar
    convertLetterToChar
    convertSymbolToChar
    convertDigitToChar
    convertCharToChar
|]

/// Returns char corresponding to key or None if there is no such char.
let keyToChar key =
    convertersToChar |> Seq.tryPick (
        fun f -> f key
    )

/// Returns char corresponding to keyPrefix or None if there is no such char.
let keyPrefixToChar keyPrefix =
    match keyPrefix with
    | Ctrl inputKey
        when InputKey.A <= inputKey && inputKey <= InputKey.Z ->
            Some (char (int inputKey) + aA_delta)
    | ShiftCtrl inputKey
        when InputKey.A <= inputKey && inputKey <= InputKey.Z ->
            Some (char (int inputKey))
    | _ ->
        None

// optional Shift with or without other modifs

let (|OptShift|_|) inputKey x =
    match x with
    | NoModif   k when k = inputKey -> Some false
    | Shift     k when k = inputKey -> Some true
    | _                             -> None

let (|OptShiftCtrl|_|) inputKey x =
    match x with
    | Ctrl      k when k = inputKey -> Some false
    | ShiftCtrl k when k = inputKey -> Some true
    | _                             -> None

let (|OptShiftAlt|_|) inputKey x =
    match x with
    | Alt       k when k = inputKey -> Some false
    | ShiftAlt  k when k = inputKey -> Some true
    | _                             -> None
