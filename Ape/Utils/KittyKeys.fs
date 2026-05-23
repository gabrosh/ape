module KittyKeys

open System

open ConsoleKeys
open KittyKeysAux

/// Initializes Kitty keyboard protocol support for the terminal.
/// Returns true if the protocol is supported by the terminal.
let initialize () =
    writeInitRequest ()
    let initResponse = readInitResponse ()
    supportsKittyProtocol initResponse

/// Deinitializes Kitty keyboard protocol support for the terminal.
let deinitialize () =
    writeDeinitRequest ()

// functional keys definitions

let functionalKeys = Map [
    ( ( 27    , 'u' ), InputKey.Escape      )  // ESCAPE
    ( ( 13    , 'u' ), InputKey.Enter       )  // ENTER
    ( ( 9     , 'u' ), InputKey.Tab         )  // TAB
    ( ( 127   , 'u' ), InputKey.Backspace   )  // BACKSPACE
    ( ( 8     , 'u' ), InputKey.Backspace   )  // BACKSPACE
    ( ( 2     , '~' ), InputKey.Insert      )  // INSERT
    ( ( 3     , '~' ), InputKey.Delete      )  // DELETE
    ( ( 1     , 'D' ), InputKey.LeftArrow   )  // LEFT
    ( ( 1     , 'C' ), InputKey.RightArrow  )  // RIGHT
    ( ( 1     , 'A' ), InputKey.UpArrow     )  // UP
    ( ( 1     , 'B' ), InputKey.DownArrow   )  // DOWN
    ( ( 5     , '~' ), InputKey.PageUp      )  // PAGE_UP
    ( ( 6     , '~' ), InputKey.PageDown    )  // PAGE_DOWN
    ( ( 1     , 'H' ), InputKey.Home        )  // HOME
    ( ( 7     , '~' ), InputKey.Home        )  // HOME
    ( ( 1     , 'F' ), InputKey.End         )  // END
    ( ( 8     , '~' ), InputKey.End         )  // END
//  ( ( 57358 , 'u' ), CAPS_LOCK            )  // CAPS_LOCK
//  ( ( 57359 , 'u' ), SCROLL_LOCK          )  // SCROLL_LOCK
//  ( ( 57360 , 'u' ), NUM_LOCK             )  // NUM_LOCK
//  ( ( 57361 , 'u' ), PRINT_SCREEN         )  // PRINT_SCREEN
//  ( ( 57362 , 'u' ), PAUSE                )  // PAUSE
//  ( ( 57363 , 'u' ), MENU                 )  // MENU
    ( ( 1     , 'P' ), InputKey.F1          )  // F1
    ( ( 11    , '~' ), InputKey.F1          )  // F1
    ( ( 1     , 'Q' ), InputKey.F2          )  // F2
    ( ( 12    , '~' ), InputKey.F2          )  // F2
    ( ( 1     , 'R' ), InputKey.F3          )  // F3
    ( ( 13    , '~' ), InputKey.F3          )  // F3
    ( ( 1     , 'S' ), InputKey.F4          )  // F4
    ( ( 14    , '~' ), InputKey.F4          )  // F4
    ( ( 15    , '~' ), InputKey.F5          )  // F5
    ( ( 17    , '~' ), InputKey.F6          )  // F6
    ( ( 18    , '~' ), InputKey.F7          )  // F7
    ( ( 19    , '~' ), InputKey.F8          )  // F8
    ( ( 20    , '~' ), InputKey.F9          )  // F9
    ( ( 21    , '~' ), InputKey.F10         )  // F10
    ( ( 23    , '~' ), InputKey.F11         )  // F11
    ( ( 24    , '~' ), InputKey.F12         )  // F12
//  ( ( 57376 , 'u' ), F13                  )  // F13
//  ( ( 57377 , 'u' ), F14                  )  // F14
//  ( ( 57378 , 'u' ), F15                  )  // F15
//  ( ( 57379 , 'u' ), F16                  )  // F16
//  ( ( 57380 , 'u' ), F17                  )  // F17
//  ( ( 57381 , 'u' ), F18                  )  // F18
//  ( ( 57382 , 'u' ), F19                  )  // F19
//  ( ( 57383 , 'u' ), F20                  )  // F20
//  ( ( 57384 , 'u' ), F21                  )  // F21
//  ( ( 57385 , 'u' ), F22                  )  // F22
//  ( ( 57386 , 'u' ), F23                  )  // F23
//  ( ( 57387 , 'u' ), F24                  )  // F24
//  ( ( 57388 , 'u' ), F25                  )  // F25
//  ( ( 57389 , 'u' ), F26                  )  // F26
//  ( ( 57390 , 'u' ), F27                  )  // F27
//  ( ( 57391 , 'u' ), F28                  )  // F28
//  ( ( 57392 , 'u' ), F29                  )  // F29
//  ( ( 57393 , 'u' ), F30                  )  // F30
//  ( ( 57394 , 'u' ), F31                  )  // F31
//  ( ( 57395 , 'u' ), F32                  )  // F32
//  ( ( 57396 , 'u' ), F33                  )  // F33
//  ( ( 57397 , 'u' ), F34                  )  // F34
//  ( ( 57398 , 'u' ), F35                  )  // F35

    ( ( 57399 , 'u' ), InputKey.D0          )  // KP_0
    ( ( 57400 , 'u' ), InputKey.D1          )  // KP_1
    ( ( 57401 , 'u' ), InputKey.D2          )  // KP_2
    ( ( 57402 , 'u' ), InputKey.D3          )  // KP_3
    ( ( 57403 , 'u' ), InputKey.D4          )  // KP_4
    ( ( 57404 , 'u' ), InputKey.D5          )  // KP_5
    ( ( 57405 , 'u' ), InputKey.D6          )  // KP_6
    ( ( 57406 , 'u' ), InputKey.D7          )  // KP_7
    ( ( 57407 , 'u' ), InputKey.D8          )  // KP_8
    ( ( 57408 , 'u' ), InputKey.D9          )  // KP_9
//  ( ( 57409 , 'u' ), KP_DECIMAL           )  // KP_DECIMAL
    ( ( 57410 , 'u' ), InputKey.Slash       )  // KP_DIVIDE
    ( ( 57411 , 'u' ), InputKey.Asterisk    )  // KP_MULTIPLY
    ( ( 57412 , 'u' ), InputKey.Minus       )  // KP_SUBTRACT
    ( ( 57413 , 'u' ), InputKey.Plus        )  // KP_ADD
    ( ( 57414 , 'u' ), InputKey.Enter       )  // KP_ENTER
//  ( ( 57415 , 'u' ), KP_EQUAL             )  // KP_EQUAL
//  ( ( 57416 , 'u' ), KP_SEPARATOR         )  // KP_SEPARATOR

//  ( ( 57428 , 'u' ), MEDIA_PLAY           )  // MEDIA_PLAY
//  ( ( 57429 , 'u' ), MEDIA_PAUSE          )  // MEDIA_PAUSE
//  ( ( 57430 , 'u' ), MEDIA_PLAY_PAUSE     )  // MEDIA_PLAY_PAUSE
//  ( ( 57431 , 'u' ), MEDIA_REVERSE        )  // MEDIA_REVERSE
//  ( ( 57432 , 'u' ), MEDIA_STOP           )  // MEDIA_STOP
//  ( ( 57433 , 'u' ), MEDIA_FAST_FORWARD   )  // MEDIA_FAST_FORWARD
//  ( ( 57434 , 'u' ), MEDIA_REWIND         )  // MEDIA_REWIND
//  ( ( 57435 , 'u' ), MEDIA_TRACK_NEXT     )  // MEDIA_TRACK_NEXT
//  ( ( 57436 , 'u' ), MEDIA_TRACK_PREVIOUS )  // MEDIA_TRACK_PREVIOUS
//  ( ( 57437 , 'u' ), MEDIA_RECORD         )  // MEDIA_RECORD
//  ( ( 57438 , 'u' ), LOWER_VOLUME         )  // LOWER_VOLUME
//  ( ( 57439 , 'u' ), RAISE_VOLUME         )  // RAISE_VOLUME
//  ( ( 57440 , 'u' ), MUTE_VOLUME          )  // MUTE_VOLUME
//  ( ( 57441 , 'u' ), LEFT_SHIFT           )  // LEFT_SHIFT
//  ( ( 57442 , 'u' ), LEFT_CONTROL         )  // LEFT_CONTROL
//  ( ( 57443 , 'u' ), LEFT_ALT             )  // LEFT_ALT
//  ( ( 57444 , 'u' ), LEFT_SUPER           )  // LEFT_SUPER
//  ( ( 57445 , 'u' ), LEFT_HYPER           )  // LEFT_HYPER
//  ( ( 57446 , 'u' ), LEFT_META            )  // LEFT_META
//  ( ( 57447 , 'u' ), RIGHT_SHIFT          )  // RIGHT_SHIFT
//  ( ( 57448 , 'u' ), RIGHT_CONTROL        )  // RIGHT_CONTROL
//  ( ( 57449 , 'u' ), RIGHT_ALT            )  // RIGHT_ALT
//  ( ( 57450 , 'u' ), RIGHT_SUPER          )  // RIGHT_SUPER
//  ( ( 57451 , 'u' ), RIGHT_HYPER          )  // RIGHT_HYPER
//  ( ( 57452 , 'u' ), RIGHT_META           )  // RIGHT_META
//  ( ( 57453 , 'u' ), ISO_LEVEL3_SHIFT     )  // ISO_LEVEL3_SHIFT
//  ( ( 57454 , 'u' ), ISO_LEVEL5_SHIFT     )  // ISO_LEVEL5_SHIFT
]

let functionalKeys_NumPad = Map [
    ( ( 57417 , 'u' ), InputKey.LeftArrow   )  // KP_LEFT
    ( ( 57418 , 'u' ), InputKey.RightArrow  )  // KP_RIGHT
    ( ( 57419 , 'u' ), InputKey.UpArrow     )  // KP_UP
    ( ( 57420 , 'u' ), InputKey.DownArrow   )  // KP_DOWN
    ( ( 57421 , 'u' ), InputKey.PageUp      )  // KP_PAGE_UP
    ( ( 57422 , 'u' ), InputKey.PageDown    )  // KP_PAGE_DOWN
    ( ( 57423 , 'u' ), InputKey.Home        )  // KP_HOME
    ( ( 57424 , 'u' ), InputKey.End         )  // KP_END
    ( ( 57425 , 'u' ), InputKey.Insert      )  // KP_INSERT
    ( ( 57426 , 'u' ), InputKey.Delete      )  // KP_DELETE
    ( ( 1     , 'E' ), InputKey.Begin       )  // KP_BEGIN
    ( ( 57427 , '~' ), InputKey.Begin       )  // KP_BEGIN
]

// getModifierFun... functions

let private modNoModif      = ConsoleModifiers.None
let private modCtrl         = ConsoleModifiers.Control
let private modAlt          = ConsoleModifiers.Alt
let private modCtrlAlt      = ConsoleModifiers.Control ||| ConsoleModifiers.Alt
let private modShift        = ConsoleModifiers.Shift
let private modShiftCtrl    = ConsoleModifiers.Shift   ||| modCtrl
let private modShiftAlt     = ConsoleModifiers.Shift   ||| modAlt
let private modShiftCtrlAlt = ConsoleModifiers.Shift   ||| modCtrlAlt

let private modifierFuns = Map [
//                      NoOper         ClearShift 
    modNoModif      , ( NoModif      , NoModif    )
    modCtrl         , ( Ctrl         , Ctrl       )
    modAlt          , ( Alt          , Alt        )
    modCtrlAlt      , ( CtrlAlt      , CtrlAlt    )
    modShift        , ( Shift        , NoModif    )
    modShiftCtrl    , ( ShiftCtrl    , Ctrl       )
    modShiftAlt     , ( ShiftAlt     , Alt        )
    modShiftCtrlAlt , ( ShiftCtrlAlt , CtrlAlt    )
]

// getKittyModifierFun... functions

let private getModifierFun_NoOper modifs =
    let func, _ = modifierFuns[modifs]
    func

let private kittyModNoModif      = KittyModifiers.None
let private kittyModCtrl         = KittyModifiers.Ctrl
let private kittyModAlt          = KittyModifiers.Alt
let private kittyModCtrlAlt      = KittyModifiers.Ctrl  ||| KittyModifiers.Alt
let private kittyModShift        = KittyModifiers.Shift
let private kittyModShiftCtrl    = KittyModifiers.Shift ||| kittyModCtrl
let private kittyModShiftAlt     = KittyModifiers.Shift ||| kittyModAlt
let private kittyModShiftCtrlAlt = KittyModifiers.Shift ||| kittyModCtrlAlt
let private kittyModCapsLock     = KittyModifiers.Caps_lock
let private kittyModNumLock      = KittyModifiers.Num_lock

let private kittyModifiersMask =
        KittyModifiers.Ctrl
    ||| KittyModifiers.Alt
    ||| KittyModifiers.Shift
    ||| KittyModifiers.Caps_lock
    ||| KittyModifiers.Num_lock

let private kittyModifierFuns = Map [
//                           NoOper         ClearShift 
    kittyModNoModif      , ( NoModif      , NoModif    )
    kittyModCtrl         , ( Ctrl         , Ctrl       )
    kittyModAlt          , ( Alt          , Alt        )
    kittyModCtrlAlt      , ( CtrlAlt      , CtrlAlt    )
    kittyModShift        , ( Shift        , NoModif    )
    kittyModShiftCtrl    , ( ShiftCtrl    , Ctrl       )
    kittyModShiftAlt     , ( ShiftAlt     , Alt        )
    kittyModShiftCtrlAlt , ( ShiftCtrlAlt , CtrlAlt    )
]

let private clear (modifsToClear: KittyModifiers) (modifs: KittyModifiers) =
    modifs &&& ~~~modifsToClear

let private getKittyModifierFun_NoModif (modifs: KittyModifiers) =
    let modifs' =
        modifs |> clear (KittyModifiers.Caps_lock ||| KittyModifiers.Num_lock)
    
    let func, _ = kittyModifierFuns[modifs']
    func

let private getKittyModifierFun_HandleCL (modifs: KittyModifiers) =
    let cl = modifs.HasFlag KittyModifiers.Caps_lock
    
    let shMask = if cl then KittyModifiers.Shift
                       else KittyModifiers.None
    
    let modifs' =
        (modifs |> clear (KittyModifiers.Caps_lock ||| KittyModifiers.Num_lock))
        ^^^ shMask

    let func, _ = kittyModifierFuns[modifs']
    func

let private getKittyModifierFun_HandleSHNL (modifs: KittyModifiers) =
    let sh = modifs.HasFlag KittyModifiers.Shift
    let nl = modifs.HasFlag KittyModifiers.Num_lock
    
    let shMask = if sh && nl then KittyModifiers.Shift
                             else KittyModifiers.None
    
    let modifs' =
        (modifs |> clear (KittyModifiers.Caps_lock ||| KittyModifiers.Num_lock))
        ^^^ shMask
    
    let func, _ = kittyModifierFuns[modifs']
    func

let private getKittyModifierFun_ClearShiftCLNL (modifs: KittyModifiers) =
    let modifs' =
        modifs |> clear (KittyModifiers.Caps_lock ||| KittyModifiers.Num_lock)
    
    let _, func = kittyModifierFuns[modifs']
    func

// escaped, keyChar or consoleKey recognizers

/// Recognizes keys like Enter and shortcuts like Ctrl-Enter, CtrlAlt-Enter.
let private recognizeSpecialConsoleKey (kittyKey: KittyKey) =
    match kittyKey with
    | ConsoleKey (consoleKey, modifs) ->
        let found, inputKey = specialToInputKey.TryGetValue consoleKey
        if found then
            Some (getModifierFun_NoOper modifs inputKey)
        else
            None
    | _ ->
        None

/// Recognizes keys like Enter.
let private recognizeSpecialChar (kittyKey: KittyKey) =
    match kittyKey with
    | KittyChar keyChar ->
        match keyChar with
        | '\x0D' -> Some (NoModif InputKey.Enter    )
        | '\x09' -> Some (NoModif InputKey.Tab      )
        | '\x08'
        | '\x7F' -> Some (NoModif InputKey.Backspace)
        | _      -> None
    | _ ->
        None

/// Recognizes letter characters like a, A.
let private recognizeLetterChar (kittyKey: KittyKey) =
    match kittyKey with
    | KittyChar keyChar ->
        if 'A' <= keyChar && keyChar <= 'Z' then
            let inputKey = keyCharToInputKey keyChar
            Some (Shift inputKey)
        elif 'a' <= keyChar && keyChar <= 'z' then
            let inputKey = keyCharToInputKey (keyChar - aA_delta)
            Some (NoModif inputKey)            
        else
            None
    | _ ->
        None

/// Recognizes symbol characters from ASCII table like &.
let private recognizeSymbolChar (kittyKey: KittyKey) =
    match kittyKey with
    | KittyChar keyChar ->
        let found, inputKey = symbolToInputKey.TryGetValue keyChar
        if found then
            Some (NoModif inputKey)
        else
            None
    | _ ->
        None

/// Recognizes digit characters like 0.
let private recognizeDigitChar (kittyKey: KittyKey) =
    match kittyKey with
    | KittyChar keyChar ->
        if '0' <= keyChar && keyChar <= '9' then
            let inputKey = keyCharToInputKey keyChar
            Some (NoModif inputKey)
        else
            None
    | _ ->
        None

/// Recognizes all other characters like á, €.
let private recognizeOtherChar (kittyKey: KittyKey) =
    match kittyKey with
    | KittyChar keyChar ->
        Some (CharNoModif keyChar)
    | _ ->
        None

/// Recognizes text characters like €, &.
///
/// In Windows Terminal:
/// u:e s: l: t:<8364> m:Alt e:u -> CharNoModif '€'
/// u:c s: l: t:& m:Alt e:u      -> CharNoModif '&'
/// 
let private recognizeTextEscaped (kittyKey: KittyKey) =
    match kittyKey with
    | KittyEscaped {
          shifted = Some shifted; text = Some text; endChar = 'u'
      } ->
        if text <> shifted then
            Some (CharNoModif text)
        else
            None
    | KittyEscaped {
          unicode = Some unicode; text = Some text; endChar = 'u'
      } ->
        if text <> unicode then
            Some (CharNoModif text)
        else
            None
    | _ ->
        None

/// Recognizes shortcuts like ShiftCtrlAlt-Enter;
/// keys like Enter and shortcuts like ShiftCtrlAlt-Enter on Numpad;
/// keys like LeftArrow and shortcuts like ShiftCtrlAlt-LeftArrow on Numpad.
/// (And possibly keys like Enter.)
/// 
/// u:<13> s: l: t: m:Shift, Alt, Ctrl e:u              -> ShiftCtrlAlt Enter
/// 
/// u:<57414> s: l: t: m:None e:u                       -> Enter
/// u:<57414> s: l: t: m:Shift, Alt, Ctrl e:u           -> ShiftCtrlAlt Enter
///
/// u:<57417> s: l: t: m:None e:u                       -> NoModif      LeftArrow
/// u:<57417> s: l: t: m:Shift, Alt, Ctrl e:u           -> ShiftCtrlAlt LeftArrow
/// u:<57417> s: l: t: m:Shift, Num_lock e:u            -> NoModif      LeftArrow
/// u:<57417> s: l: t: m:Shift, Alt, Ctrl, Num_lock e:u -> CtrlAlt      LeftArrow
/// 
let private recognizeSpecialEscaped (kittyKey: KittyKey) =
    match kittyKey with
    | KittyEscaped {
          unicode = Some keyChar; modifiers = modifs; endChar = endChar
      } ->
        match functionalKeys |> Map.tryFind (int keyChar, endChar) with
        | Some inputKey ->
            Some (getKittyModifierFun_NoModif modifs inputKey)
        | None ->
            match functionalKeys_NumPad |> Map.tryFind (int keyChar, endChar) with
            | Some inputKey ->
                Some (getKittyModifierFun_HandleSHNL modifs inputKey)
            | None ->
                None
    | _ ->
        None
    
/// Recognizes shortcuts like CtrlAlt-A, ShiftCtrlAlt-A.
/// (And possibly keys like A.)
/// 
/// u:a s: l: t: m:Alt, Ctrl e:u                   -> CtrlAlt A
/// u:a s:A l: t: m:Shift, Alt, Ctrl e:u           -> ShiftCtrlAlt A
///
/// u:a s: l: t: m:Alt, Ctrl, Caps_lock e:u        -> ShiftCtrlAlt A
/// u:a s: l: t: m:Shift, Alt, Ctrl, Caps_lock e:u -> CtrlAlt A
/// 
let private recognizeLetterEscaped (kittyKey: KittyKey) =
    match kittyKey with
    | KittyEscaped {
          unicode = Some keyChar; modifiers = modifs; endChar = 'u'
      } ->
        if 'a' <= keyChar && keyChar <= 'z' then
            let inputKey = keyCharToInputKey (keyChar - aA_delta)
            Some (getKittyModifierFun_HandleCL modifs inputKey)
        else
            None
    | _ ->
        None

/// Recognizes shortcuts with symbol characters from ASCII table like CtrlAlt-&.
/// (And possibly symbol characters from ASCII table like &.)
///
/// u:7 s:& l: t: m:Shift, Alt, Ctrl e:u -> CtrlAlt Ampersand
/// u:[ s: l: t: m:Alt, Ctrl e:u         -> CtrlAlt LeftSquare
/// 
let private recognizeSymbolEscaped (kittyKey: KittyKey) =
    let result =
        match kittyKey with
        | KittyEscaped {
              shifted = Some keyChar; modifiers = modifs; endChar = 'u'
          }
        | KittyEscaped {
              unicode = Some keyChar; modifiers = modifs; endChar = 'u'
          } ->
            Some (keyChar, modifs)
        | _ ->
            None
            
    match result with
    | Some (keyChar, modifs) ->
        let found, inputKey = symbolToInputKey.TryGetValue keyChar
        if found then
            Some (getKittyModifierFun_ClearShiftCLNL modifs inputKey)
        else
            None
    | None ->
        None

/// Recognizes shortcuts with digit characters like CtrlAlt-0;
/// shortcuts with digit characters like CtrlAlt-0 on NumPad.
/// (And possibly digit characters like 0.)
///
/// u:0 s: l: t: m:Alt, Ctrl e:u                 -> CtrlAlt D0
/// u:<57399> s: l: t: m:Alt, Ctrl, Num_lock e:u -> CtrlAlt D0
/// 
let private recognizeDigitEscaped (kittyKey: KittyKey) =
    match kittyKey with
    | KittyEscaped {
          shifted = Some keyChar; modifiers = modifs; endChar = 'u'
      }
    | KittyEscaped {
          unicode = Some keyChar; modifiers = modifs; endChar = 'u'
      } ->
        if '0' <= keyChar && keyChar <= '9' then
            let inputKey = keyCharToInputKey keyChar
            Some (getKittyModifierFun_ClearShiftCLNL modifs inputKey)
        else
            None
    | _ ->
        None

/// Returns undefined key.
let private recognizeAsUndefined (_kittyKey: KittyKey) =
    Some (CharNoModif '\x00')

let private inputKeyRecognizers = [|
    recognizeSpecialConsoleKey
    
    recognizeSpecialChar    
    recognizeLetterChar
    recognizeSymbolChar
    recognizeDigitChar
    
    recognizeOtherChar

    recognizeTextEscaped
    
    recognizeSpecialEscaped
    recognizeLetterEscaped
    recognizeSymbolEscaped
    recognizeDigitEscaped

    recognizeAsUndefined
|]

/// Returns Key corresponding to KittyKey.
let kittyKeyToKey (kittyKey: KittyKey) =
    let kittyKey' =
        match kittyKey with
        | KittyEscaped escaped ->
            KittyEscaped {
                escaped with modifiers = escaped.modifiers &&& kittyModifiersMask
            }
        | _ ->
            kittyKey
            
    inputKeyRecognizers |> Seq.pick (
        fun f -> f kittyKey'
    )

/// Reads one input key from the console.
let readKey () =
    let kittyKey = readInputKey ()
    kittyKeyToKey kittyKey

// testing

let run () =
    let mutable doContinue = true

    while doContinue do
        let kittyKey = readInputKey ()
        Console.WriteLine (getKittyKeyRepr kittyKey)        

        let key = kittyKeyToKey kittyKey
        Console.WriteLine key
        
        if key = NoModif InputKey.F10 then
            doContinue <- false
