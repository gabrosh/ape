module GenttyKeys

open System

open ConsoleInterop_Specific
open ConsoleKeys

let private modNoModif      = ConsoleModifiers.None
let private modCtrl         = ConsoleModifiers.Control
let private modAlt          = ConsoleModifiers.Alt
let private modCtrlAlt      = ConsoleModifiers.Control ||| ConsoleModifiers.Alt
let private modShift        = ConsoleModifiers.Shift
let private modShiftCtrl    = ConsoleModifiers.Shift ||| modCtrl
let private modShiftAlt     = ConsoleModifiers.Shift ||| modAlt
let private modShiftCtrlAlt = ConsoleModifiers.Shift ||| modCtrlAlt

let private isNoModifOrShift modifs =
       modifs = modNoModif
    || modifs = modShift

let private includesEitherCtrlOrAlt modifs =
       modifs &&& modCtrlAlt = modCtrl
    || modifs &&& modCtrlAlt = modAlt

// getModifierFun... functions

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

/// Recognizes keys like Enter and shortcuts like Ctrl-Enter, CtrlAlt-Enter.
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

/// Recognizes letter characters like a, A.
let private recognizeLetterChar modifs _consoleKey keyChar (_capsLock: bool) =
    if 'A' <= keyChar && keyChar <= 'Z'
        && isNoModifOrShift modifs
    then
        let inputKey = keyCharToInputKey keyChar
        Some (Shift inputKey)

    elif 'a' <= keyChar && keyChar <= 'z'
        && isNoModifOrShift modifs
    then
        let inputKey = keyCharToInputKey (keyChar - aA_delta)
        Some (NoModif inputKey)

    else
        None

/// Recognizes symbol characters from ASCII table like &.
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

/// Recognizes digit characters like 0.
let private recognizeDigitChar modifs _consoleKey keyChar (_capsLock: bool) =
    if '0' <= keyChar && keyChar <= '9'
        && isNoModifOrShift modifs
    then
        let inputKey = keyCharToInputKey keyChar
        Some (getModifierFun_ClearShift modifs inputKey)
    else
        None

/// Recognizes all other characters like á.
let private recognizeOtherChar modifs _consoleKey keyChar (_capsLock: bool) =
    Some (getModifierFun_ForChar modifs keyChar)

let private inputKeyRecognizers = [|
    recognizeSpecial
    recognizeLetterKey
    recognizeLetterChar
    recognizeSymbol      // Symbols typed on digit keys have priority before Shift-Alt-0 to Shift-Alt-9.
    recognizeDigitKey
    recognizeDigitChar
    recognizeOtherChar
|]

/// Returns Key corresponding to ConsoleKeyInfo.
let keyInfoToKey (keyInfo: ConsoleKeyInfo) capsLock =
    let modifs     = keyInfo.Modifiers
    let consoleKey = keyInfo.Key
    let keyChar    = keyInfo.KeyChar

    inputKeyRecognizers |> Seq.pick (
        fun f -> f modifs consoleKey keyChar capsLock
    )

/// Reads one input key from the console.
let readKey () =
    let keyInfo = Console.ReadKey true
    let capsLock = consoleInterop.GetCapsLock ()
    keyInfoToKey keyInfo capsLock
