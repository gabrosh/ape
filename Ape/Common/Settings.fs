module Settings

open System.Collections.Generic

type FUFF = FileUtils.FileFormat

let FUDFF = FileUtils.defaultFileFormat

// definitions

type Scope =
    | ``default`` = 1
    | ``global``  = 2
    |   buffer    = 3
    |   extract   = 4

let scopeToString (scope: Scope) =
    scope.ToString ()

let scopeStrings =
    [
        Scope.``global``
        Scope.  buffer
        Scope.  extract
    ]
    |> Seq.map scopeToString

let parseScope (s: string) =
    match s with
    | "global"  | "g" -> Ok Scope.``global``
    | "buffer"  | "b" -> Ok Scope.  buffer
    | "extract" | "e" -> Ok Scope.  extract
    | _               -> Error $"Invalid setting's scope: '{s}'"

type Name =
    | colorScheme      = 1
    | encoding         = 2
    | strictEncoding   = 3
    | fileFormat       = 4
    | newLineAtEof     = 5
    | readOnly         = 6
    | reloadAsLogFile  = 7
    | maxSavedUndos    = 8
    | reSearchMatching = 9
    | recursionLimit   = 10
    | wrapLines        = 11
    | wrapAtWord       = 12
    | showLineNumbers  = 13
    | tabStop          = 14
    | tabBySpaces      = 15
    | scrollOffset     = 16
    | cursorBeforeEol  = 17

let nameToString (name: Name) =
    name.ToString ()

let nameStrings =
    System.Enum.GetNames typeof<Name>
    |> Array.sort

let parseName (s: string) =
    let obj = Utils.optionOfPair (
        System.Enum.TryParse (typeof<Name>, s)
    )
    match obj with
    | Some obj -> Ok (obj :?> Name)
    | None     ->
        match s with
        | "cs"  -> Ok Name.colorScheme
        | "enc" -> Ok Name.encoding
        | "se"  -> Ok Name.strictEncoding
        | "ff"  -> Ok Name.fileFormat
        | "nae" -> Ok Name.newLineAtEof
        | "ro"  -> Ok Name.readOnly
        | "rlf" -> Ok Name.reloadAsLogFile
        | "msu" -> Ok Name.maxSavedUndos
        | "rsm" -> Ok Name.reSearchMatching
        | "rl"  -> Ok Name.recursionLimit
        | "wl"  -> Ok Name.wrapLines
        | "waw" -> Ok Name.wrapAtWord
        | "sln" -> Ok Name.showLineNumbers
        | "ts"  -> Ok Name.tabStop
        | "tbs" -> Ok Name.tabBySpaces
        | "so"  -> Ok Name.scrollOffset
        | "cbe" -> Ok Name.cursorBeforeEol
        | _     -> Error $"Invalid setting's name: '{s}'"

type Value =
    | Bool       of bool
    | Int        of int
    | String     of string
    | FileFormat of FUFF

let valueToString (value: Value) =
    match value with
    | Bool       false -> "false"
    | Bool       true  -> "true"
    | Int        x     -> x.ToString ()
    | String     x     -> x.ToString ()
    | FileFormat x     -> x.ToString ()

// Contains (value, isFixed) pair as a value.
type SettingsDict =
    Dictionary<Name, Value * bool>

type Settings = {
    scope:  Scope
    parent: Settings option
    dict:   SettingsDict
}

// specifications

type SettingSpec =
    | Bools       of default_: bool   * isValid: (bool   -> bool)
    | Ints        of default_: int    * isValid: (int    -> bool)
    | Strings     of default_: string * isValid: (string -> bool)
    | FileFormats of default_: FUFF   * isValid: (FUFF   -> bool)

let private all _value =
    true

let private isEqOrGt from value =
    from <= value

let private isInRange from to_ value =
    from <= value && value <= to_

let private isInSet values value =
    values |> Set.contains value

let private isInMap values value =
    values |> Map.containsKey value

let specsMap =
    Map [
        Name.colorScheme , Strings (
            default_ = Colors.defaultScheme, isValid = isInMap Colors.schemesMap
        )

        Name.encoding    , Strings (
            default_ = FileUtils.defaultEncoding, isValid = isInSet FileUtils.encodingsSet
        )

        Name.strictEncoding   , Bools       ( default_ = true  , isValid = all            )
        Name.fileFormat       , FileFormats ( default_ = FUDFF , isValid = all            )
        Name.newLineAtEof     , Bools       ( default_ = true  , isValid = all            )
        Name.readOnly         , Bools       ( default_ = false , isValid = all            )
        Name.reloadAsLogFile  , Bools       ( default_ = false , isValid = all            )
        Name.maxSavedUndos    , Ints        ( default_ = 25    , isValid = isEqOrGt  1    )
        Name.reSearchMatching , Bools       ( default_ = true  , isValid = all            )
        Name.recursionLimit   , Ints        ( default_ = 1000  , isValid = isEqOrGt  1    )
        Name.wrapLines        , Bools       ( default_ = false , isValid = all            )
        Name.wrapAtWord       , Bools       ( default_ = true  , isValid = all            )
        Name.showLineNumbers  , Bools       ( default_ = false , isValid = all            )
        Name.tabStop          , Ints        ( default_ = 4     , isValid = isInRange 1 99 )
        Name.tabBySpaces      , Bools       ( default_ = true  , isValid = all            )
        Name.scrollOffset     , Ints        ( default_ = 3     , isValid = isInRange 0 99 )
        Name.cursorBeforeEol  , Bools       ( default_ = false , isValid = all            )
    ]

// user input values validation

let private validateAux name value parse isValid constr =
    match parse value with
    | Ok x when isValid x -> Ok (constr x)
    | Ok _                -> Error $"Invalid {name} value: '{value}'"
    | Error e             -> Error $"Invalid {name} value: {e}"

let private validate name value =
    match specsMap[name] with
    | Bools       (_, isValid) -> validateAux name value Parsing.parseBool       isValid Bool
    | Ints        (_, isValid) -> validateAux name value Parsing.parseInt        isValid Int
    | Strings     (_, isValid) -> validateAux name value Parsing.parseString     isValid String
    | FileFormats (_, isValid) -> validateAux name value Parsing.parseFileFormat isValid FileFormat

[<TailCall>]
let rec private isValueFixedRec settings name =
    let ok, item = settings.dict.TryGetValue name

    if ok && snd item then
        true
    else
        match settings.parent with
        | Some parent ->
            isValueFixedRec parent name
        | None        ->
            false

// user commands

let private isScopeInvalid settings scope =
    scope = Scope.extract && settings.scope <> Scope.extract

let private isNameInScopeInvalid scope name =
    scope = Scope.extract && (
        match name with
        // Buffers extract is read-only.
        | Name.encoding
        | Name.strictEncoding
        | Name.fileFormat
        | Name.newLineAtEof
        | Name.reSearchMatching
        // Taken from the parent buffer.
        | Name.reloadAsLogFile ->
            true
        | _ ->
            false
    )

[<TailCall>]
let rec private setValueRec settings scope name value isFixed =
    if settings.scope = scope then
        settings.dict[name] <- (value, isFixed)
    else
        settings.dict.Remove name |> ignore

        match settings.parent with
        | Some parent ->
            setValueRec parent scope name value isFixed
        | None        ->
            invalidOp "Can't go beyond default scope"

let private setValueAux settings scope name value isFixed =
    let scope = scope |> Option.defaultValue settings.scope

    if settings.scope < Scope.buffer then
        invalidOp "Must start from buffer or extract buffer scope"
    if scope <= Scope.``default`` then
        invalidOp "Can't set value in default scope"

    if isScopeInvalid settings scope then
        Error $"Invalid setting's scope for this buffer: '{scope}'"
    elif isNameInScopeInvalid scope name then
        Error $"Invalid setting's name for {scope} scope: '{name}'"
    elif isValueFixedRec settings name then
        Error "Can't change fixed value."
    else
        match validate name value with
        | Ok x    ->
            setValueRec settings scope name x isFixed
            Ok ()
        | Error e ->
            Error e

/// Unsets the setting with given name up to given scope.
/// Sets value of the setting with given name in given scope.
/// Returns error message in the case of an invalid value
/// or if the current value of the setting is fixed.
/// If scope = None, the most local scope is assumed.
let setValue settings scope name value =
    setValueAux settings scope name value false

/// Unsets the setting with given name up to given scope.
/// Sets value of the setting with given name in given scope as fixed.
/// Returns error message in the case of an invalid value
/// or if the current value of the setting is fixed.
/// If scope = None, the most local scope is assumed.
let setValueAsFixed settings scope name value =
    setValueAux settings scope name value true

[<TailCall>]
let rec private unsetValueRec settings scope name =
    settings.dict.Remove name |> ignore

    if settings.scope <> scope then
        match settings.parent with
        | Some parent ->
            unsetValueRec parent scope name
        | None        ->
            invalidOp "Can't go beyond default scope"

/// Unsets the setting with given name up to given scope.
/// Returns error message if the current value of the setting is fixed.
/// If scope = None, the most local scope is assumed.
let unsetValue settings scope name =
    let scope = scope |> Option.defaultValue settings.scope

    if settings.scope < Scope.buffer then
        invalidOp "Must start from buffer or extract buffer scope"
    if scope <= Scope.``default`` then
        invalidOp "Can't unset value in default scope"

    if isScopeInvalid settings scope then
        Error $"Invalid setting's scope for this buffer: '{scope}'"
    elif isNameInScopeInvalid scope name then
        Error $"Invalid setting's name for {scope} scope: '{name}'"
    elif isValueFixedRec settings name then
        Error "Can't unset fixed value."
    else
        unsetValueRec settings scope name
        Ok ()

[<TailCall>]
let rec private getValueRec settings name =
    if settings.dict.ContainsKey name then
        fst settings.dict[name]
    else
        match settings.parent with
        | Some parent ->
            getValueRec parent name
        | None        ->
            invalidOp "Can't go beyond default scope"

/// Returns value of the setting with given name.
let getValue settings name =
    getValueRec settings name

// utility functions

/// Returns value of the Bool setting with given name.
let getValueBool settings name =
    match getValue settings name with
    | Bool x       -> x
    | _            -> invalidOp "Must be Bool"

/// Returns value of the Int setting with given name.
let getValueInt settings name =
    match getValue settings name with
    | Int x        -> x
    | _            -> invalidOp "Must be Int"

/// Returns value of the String setting with given name.
let getValueString settings name =
    match getValue settings name with
    | String x     -> x
    | _            -> invalidOp "Must be String"

/// Returns value of the FileFormat setting with given name.
let getValueFileFormat settings name =
    match getValue settings name with
    | FileFormat x -> x
    | _            -> invalidOp "Must be FileFormat"

let rec getSettingReprAux settings name (acc: string) =
    match settings with
    | Some settings ->
        let newAcc =
            if settings.dict.ContainsKey name then
                let scope = settings.scope.ToString ()
                let value = valueToString (fst settings.dict[name])
                let separ = if acc <> "" then ", " else ""
                $"{scope} '{value}'{separ}" + acc
            else
                acc
        getSettingReprAux settings.parent name newAcc

    | None -> acc

/// Returns display representation of the setting with given name.
let getSettingRepr settings name =
    let nameRepr = nameToString name
    let valuesRepr = getSettingReprAux (Some settings) name ""
    $"{nameRepr}: {valuesRepr}"

// settings construction

let private getDefaultValue spec =
    match spec with
    | Bools       (default_, _) -> Bool       default_
    | Ints        (default_, _) -> Int        default_
    | Strings     (default_, _) -> String     default_
    | FileFormats (default_, _) -> FileFormat default_

/// Returns new default settings with no parent.
let private makeDefaultSettings () =
    let mapSeq = seq {
        for item in specsMap do
            KeyValuePair (
                item.Key, (getDefaultValue item.Value, false)
            )
    }

    {
        scope  = Scope.``default``
        parent = None
        dict   = SettingsDict mapSeq
    }

/// common default settings
let private defaultSettings = makeDefaultSettings ()

/// Returns new global settings with defaultSettings as parent.
let makeGlobalSettings () =
    {
        scope  = Scope.``global``
        parent = Some defaultSettings
        dict   = SettingsDict ()
    }

/// Returns new buffer settings with given parent.
let makeBufferSettings parent =
    {
        scope  = Scope.buffer
        parent = Some parent
        dict   = SettingsDict ()
    }

/// Returns new extract buffer settings with given parent.
let makeBufferExtractSettings parent =
    {
        scope  = Scope.extract
        parent = Some parent
        dict   = SettingsDict ()
    }
