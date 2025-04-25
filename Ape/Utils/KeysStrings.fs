module KeysStrings

type private Match = System.Text.RegularExpressions.Match
type private Regex = System.Text.RegularExpressions.Regex

open ConsoleKeys

let private lowercaseModifiers = Map [
    ""       , NoModif
    "c"      , Ctrl
    "a"      , Alt
//  "ca"     , CtrlAlt
    "s"      , Shift
    "sc"     , ShiftCtrl
    "sa"     , ShiftAlt
//  "sca"    , ShiftCtrlAlt
]

let private uppercaseModifiers = Map [
    ""       , Shift
    "c"      , ShiftCtrl
    "a"      , ShiftAlt
//  "ca"     , CtrlAlt
//  "s"      , Shift
//  "sc"     , ShiftCtrl
//  "sa"     , ShiftAlt
//  "sca"    , ShiftCtrlAlt
]

let private digitModifiers = Map [
    ""       , NoModif
    "c"      , Ctrl
    "a"      , Alt
//  "ca"     , CtrlAlt
//  "s"      , Shift
    "sc"     , ShiftCtrl
    "sa"     , ShiftAlt
//  "sca"    , ShiftCtrlAlt
]

let private specialModifiers = Map [
    ""       , NoModif
    "c"      , Ctrl
    "a"      , Alt
    "ca"     , CtrlAlt
    "s"      , Shift
    "sc"     , ShiftCtrl
    "sa"     , ShiftAlt
    "sca"    , ShiftCtrlAlt
]

let private symbolModifiers = Map [
    ""       , NoModif
    "c"      , Ctrl
    "a"      , Alt
]

let private charModifiers = Map [
    ""       , CharNoModif
]

let private specialKeyNames = Map [
    "bsp"    , InputKey.Backspace
    "tab"    , InputKey.Tab
    "ret"    , InputKey.Enter
    "esc"    , InputKey.Escape
    "sp"     , InputKey.Spacebar
    "pgup"   , InputKey.PageUp
    "pgdown" , InputKey.PageDown
    "end"    , InputKey.End
    "home"   , InputKey.Home
    "left"   , InputKey.LeftArrow
    "up"     , InputKey.UpArrow
    "right"  , InputKey.RightArrow
    "down"   , InputKey.DownArrow
    "ins"    , InputKey.Insert
    "del"    , InputKey.Delete

    "f1"     , InputKey.F1
    "f2"     , InputKey.F2
    "f3"     , InputKey.F3
    "f4"     , InputKey.F4
    "f5"     , InputKey.F5
    "f6"     , InputKey.F6
    "f7"     , InputKey.F7
    "f8"     , InputKey.F8
    "f9"     , InputKey.F9
    "f10"    , InputKey.F10
    "f11"    , InputKey.F11
    "f12"    , InputKey.F12
]

let private symbolKeyNames = Map [
    "dq"     , InputKey.Quotation
    "lt"     , InputKey.LessThan
    "gt"     , InputKey.GreaterThan
]

// auxiliary

let private aA_delta = 'a' - 'A'

let private keyCharToInputKey keyChar =
    enum<InputKey> (int keyChar)

let private inputKeyToKeyChar inputKey =
    char inputKey

// adding modifiers

type ModifiersMap = Map<string, InputKey -> Key>

// public for testing purposes
let MUST_BE_ONE_OR_TWO_KEY_SPECS = "Must be one or two key specifications"
let NO_KEY_AFTER_KEY_PREFIX      = "No key after key prefix"
let INVALID_KEY_PREFIX           = "Invalid key prefix"
let INVALID_KEY_SPEC             = "Invalid key specification"
let INVALID_KEY_MODIFIERS        = "Invalid key modifiers"
let INVALID_KEY_NAME             = "Invalid key name"

let private addModifiersToInputKey (modifiersMap: ModifiersMap) modifs inputKey =
    if modifiersMap.ContainsKey modifs then
        let f = modifiersMap[modifs]
        Ok (f inputKey)
    else
        Error INVALID_KEY_MODIFIERS

let private addModifiersToLowercase = addModifiersToInputKey lowercaseModifiers
let private addModifiersToUppercase = addModifiersToInputKey uppercaseModifiers
let private addModifiersToDigit     = addModifiersToInputKey digitModifiers
let private addModifiersToSpecial   = addModifiersToInputKey specialModifiers
let private addModifiersToSymbol    = addModifiersToInputKey symbolModifiers

let private addModifiersToKeyChar modifs keyChar =
    if charModifiers.ContainsKey modifs then
        let f = charModifiers[modifs]
        Ok (f keyChar)
    else
        Error INVALID_KEY_MODIFIERS

// keys string to key or keys array conversion

let private modifiedKeyToKey (modifs: string) (keyName: string) =
    if keyName.Length = 1 then
        let keyChar = keyName[0]

        if   'a' <= keyChar && keyChar <= 'z' then
            let keyChar = keyChar - aA_delta
            keyCharToInputKey keyChar |> addModifiersToLowercase modifs

        elif 'A' <= keyChar && keyChar <= 'Z' then
            keyCharToInputKey keyChar |> addModifiersToUppercase modifs

        elif '0' <= keyChar && keyChar <= '9' then
            keyCharToInputKey keyChar |> addModifiersToDigit     modifs

        elif symbolToInputKey.ContainsKey keyChar then
            symbolToInputKey[keyChar] |> addModifiersToSymbol    modifs

        else
            addModifiersToKeyChar modifs keyChar

    elif specialKeyNames.ContainsKey keyName then
        specialKeyNames[keyName] |> addModifiersToSpecial modifs

    elif symbolKeyNames.ContainsKey keyName then
        symbolKeyNames[keyName]  |> addModifiersToSymbol  modifs

    else
        Error INVALID_KEY_NAME

let private notModifiedKeyToKey (keyName: string) =
    modifiedKeyToKey "" keyName

/// Matches strings like "x", "<x>", "<c-x>", "<del>", "<c-del>".
let private sequenceRegex = Regex (
    @"\G(?'char'[^<>])|\G(<((?'modifs'[a-z]+)-)?(?'key'[^<>-]+)>)"
)

let private matchToKeyAux (m: Match) =
    let char_  = m.Groups["char"]
    let modifs = m.Groups["modifs"]
    let key    = m.Groups["key"]

    if char_.Success then
        notModifiedKeyToKey char_.Value
    elif key.Success then
        if modifs.Success then
            modifiedKeyToKey modifs.Value key.Value
        else
            notModifiedKeyToKey key.Value
    else
        invalidOp "Unexpected error during parsing a keys sequence"

let private matchToKey (m: Match) =
    let result = matchToKeyAux m

    match result with
    | Ok key  -> Ok (key, m.Index + m.Length)
    | Error e -> Error $"{e}: '{m.Value}'"

let private processMatch state (value: Result<Key * int, string>) =
    match state with
    | Ok (lst, _processedTo) ->
        match value with
        | Ok (key, processedTo) -> Ok (key :: lst, processedTo)
        | Error e               -> Error e
    | Error e                ->
        Error e

/// Returns an array of Keys created by parsing keysString
/// or Error if keyString is not in the required format.
let parseKeysString keysString =
    let result =
        sequenceRegex.Matches keysString
        |> Seq.map  matchToKey
        |> Seq.fold processMatch (Ok ([], 0))

    match result with
    | Ok (lst, processedTo) ->
        if processedTo = keysString.Length then
            Ok (lst |> List.rev |> List.toArray)
        else
            Error $"{INVALID_KEY_SPEC} at offset {processedTo}: '{keysString}'"
    | Error e ->
        Error e

let private parseKeyPrefixAndKeyStringAux keysString =
    let result = parseKeysString keysString

    match result with
    | Ok arr when arr.Length = 1
        -> Ok (None, arr[0])
    | Ok arr when arr.Length = 2
        -> Ok (Some arr[0], arr[1])
    | Ok _
        -> Error $"{MUST_BE_ONE_OR_TWO_KEY_SPECS}: '{keysString}'"
    | Error e
        -> Error e

/// Returns true if keyPrefix is a correct key prefix.
let isKeyPrefix keyPrefix =
    match keyPrefix with
    | OptShiftCtrl InputKey.B _
    | OptShiftCtrl InputKey.E _
        -> true
    | _
        -> false

/// Returns a single pair of Key prefix and Key created by parsing
/// keysString or Error if keyString is not in the required format.
let parseKeyPrefixAndKeyString keysString =
    let result = parseKeyPrefixAndKeyStringAux keysString

    match result with
    | Ok (None, key) ->
        if not (isKeyPrefix key) then
            result
        else
            Error $"{NO_KEY_AFTER_KEY_PREFIX}: '{keysString}'"
    | Ok (Some keyPrefix, _key) ->
        if isKeyPrefix keyPrefix then
            result
        else
            Error $"{INVALID_KEY_PREFIX}: '{keysString}'"
    | Error e         ->
        Error e

// key to key string conversion

let private keyNamesMap =
    Seq.append (specialKeyNames |> Map.toSeq)
               (symbolKeyNames  |> Map.toSeq)
    |> Seq.map (fun (x, y) -> y, x)
    |> Map

let private charsMap =
    Seq.append (ConsoleKeys.specialToChar |> Map.toSeq)
               (ConsoleKeys.symbolToChar  |> Map.toSeq)
    |> Map

let private isLetter (inputKey: InputKey) =
    InputKey.A  <= inputKey && inputKey <= InputKey.Z

let private isDigit (inputKey: InputKey) =
    InputKey.D0 <= inputKey && inputKey <= InputKey.D9

let private lowercaseToKeyName (inputKey: InputKey) =
    let c = inputKeyToKeyChar inputKey
    (System.Char.ToLower c).ToString ()

let private uppercaseToKeyName (inputKey: InputKey) =
    let c = inputKeyToKeyChar inputKey
    (System.Char.ToUpper c).ToString ()

let private nonLetterToKeyName withModifs (inputKey: InputKey) =
    match Map.tryFind inputKey keyNamesMap with
    | Some keyName ->
        if withModifs then keyName else "<" + keyName + ">"
    | None         ->
        if isDigit inputKey then
            (inputKeyToKeyChar inputKey).ToString ()
        else
            (Map.find inputKey charsMap).ToString ()

/// Returns string representation of given key.
let keyToKeyString (key: Key) =
    match key with
    | NoModif       inputKey
      when isLetter inputKey ->           lowercaseToKeyName inputKey
    | Ctrl          inputKey
      when isLetter inputKey -> "<c-"   + lowercaseToKeyName inputKey + ">"
    | Alt           inputKey
      when isLetter inputKey -> "<a-"   + lowercaseToKeyName inputKey + ">"
    | CtrlAlt       inputKey
      when isLetter inputKey -> "<ca-"  + lowercaseToKeyName inputKey + ">"

    | Shift         inputKey
      when isLetter inputKey ->           uppercaseToKeyName inputKey
    | ShiftCtrl     inputKey
      when isLetter inputKey -> "<c-"   + uppercaseToKeyName inputKey + ">"
    | ShiftAlt      inputKey
      when isLetter inputKey -> "<a-"   + uppercaseToKeyName inputKey + ">"
    | ShiftCtrlAlt  inputKey
      when isLetter inputKey -> "<ca-"  + uppercaseToKeyName inputKey + ">"

    | NoModif       inputKey ->           nonLetterToKeyName false inputKey
    | Ctrl          inputKey -> "<c-"   + nonLetterToKeyName true  inputKey + ">"
    | Alt           inputKey -> "<a-"   + nonLetterToKeyName true  inputKey + ">"
    | CtrlAlt       inputKey -> "<ca-"  + nonLetterToKeyName true  inputKey + ">"

    | Shift         inputKey -> "<s-"   + nonLetterToKeyName true  inputKey + ">"
    | ShiftCtrl     inputKey -> "<sc-"  + nonLetterToKeyName true  inputKey + ">"
    | ShiftAlt      inputKey -> "<sa-"  + nonLetterToKeyName true  inputKey + ">"
    | ShiftCtrlAlt  inputKey -> "<sca-" + nonLetterToKeyName true  inputKey + ">"

    | CharNoModif   c        ->           c.ToString ()
