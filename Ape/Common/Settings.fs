module Settings

open System.Collections.Generic

type FUFF = FileUtils.FileFormat

let FUDFF = FileUtils.defaultFileFormat

// definitions

type Scope =
    | ``default`` = 1
    | ``global``  = 2
    |   buffer    = 3

let scopeToString (scope: Scope) =
    scope.ToString ()

let scopeStrings =
    [
        Scope.``global``
        Scope.  buffer
    ]
    |> Seq.map scopeToString

let parseScope (s: string) =
    match s with
    | "global" | "g" -> Ok Scope.``global``
    | "buffer" | "b" -> Ok Scope.  buffer
    | _              -> Error $"Invalid setting's scope: '{s}'"

type Name =
    | colorScheme      = 1
    | encoding         = 2
    | strictEncoding   = 3
    | fileFormat       = 4
    | newLineAtEof     = 5
    | readOnly         = 6
    | maxSavedUndos    = 7
    | reSearchMatching = 8
    | recursionLimit   = 9
    | wrapLines        = 10
    | wrapAtWord       = 11
    | showLineNumbers  = 12
    | tabStop          = 13
    | tabBySpaces      = 14
    | scrollOffset     = 15
    | cursorBeforeEol  = 16

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

type SettingsDict =
    Dictionary<Name, Value>

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

// user commands

let rec private setValueAux settings scope name value =
    if settings.scope = scope then
        settings.dict[name] <- value
    else
        settings.dict.Remove name |> ignore

        if settings.parent.IsSome then
            setValueAux settings.parent.Value scope name value
        else
            invalidOp "Can't go beyond default scope"

/// Unsets the setting with given name up to given scope.
/// Sets value of the setting with given name in given scope.
/// Returns error message in the case of an invalid value.
let setValue settings scope name value =
    if settings.scope <> Scope.buffer then
        invalidOp "Must start from buffer scope"
    if scope <= Scope.``default`` then
        invalidOp "Can't set value in default scope"

    match validate name value with
    | Ok x    ->
        setValueAux settings scope name x
        Ok ()
    | Error e ->
        Error e

let rec private unsetValueAux settings scope name =
    settings.dict.Remove name |> ignore

    if settings.scope <> scope then
        if settings.parent.IsSome then
            unsetValueAux settings.parent.Value scope name
        else
            invalidOp "Can't go beyond default scope"

/// Unsets the setting with given name up to given scope.
let unsetValue settings scope name =
    if settings.scope <> Scope.buffer then
        invalidOp "Must start from buffer scope"
    if scope <= Scope.``default`` then
        invalidOp "Can't unset value in default scope"

    unsetValueAux settings scope name

let rec private getValueAux settings name =
    if settings.dict.ContainsKey name then
        settings.dict[name]
    elif settings.parent.IsSome then
        getValueAux settings.parent.Value name
    else
        invalidOp "Can't go beyond default scope"

/// Returns value of the setting with given name.
let getValue settings name =
    getValueAux settings name

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
                let value = valueToString settings.dict[name]
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
                item.Key, (getDefaultValue item.Value)
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
