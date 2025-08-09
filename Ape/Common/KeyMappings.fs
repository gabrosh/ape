module KeyMappings

open System.Collections.Generic

open ConsoleKeys

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
    | _               -> Error $"Invalid key mapping's scope: '{s}'"

type Mode =
    | normal       = 1
    | insert       = 2
    | promptNormal = 3
    | promptInsert = 4

let modeToString (mode: Mode) =
    mode.ToString ()

let modeStrings =
    System.Enum.GetNames typeof<Mode>

let parseMode (s: string) =
    match s with
    | "normal"       | "n"  -> Ok Mode.normal
    | "insert"       | "i"  -> Ok Mode.insert
    | "promptNormal" | "pn" -> Ok Mode.promptNormal
    | "promptInsert" | "pi" -> Ok Mode.promptInsert
    | _              -> Error $"Invalid key mapping's mode: '{s}'"

type KeyMappingsDict =
    Dictionary<Mode * Key option * Key, Key array>

type KeyMappings = {
    scope:  Scope
    parent: KeyMappings option
    dict:   KeyMappingsDict
}

// user commands

let private isScopeInvalid keyMappings scope =
    scope = Scope.extract && keyMappings.scope <> Scope.extract

let rec private mapKeyRec keyMappings scope modeKeyTriple keySeq =
    if keyMappings.scope = scope then
        keyMappings.dict[modeKeyTriple] <- keySeq
    else
        keyMappings.dict.Remove (modeKeyTriple) |> ignore

        if keyMappings.parent.IsSome then
            mapKeyRec keyMappings.parent.Value scope modeKeyTriple keySeq
        else
            invalidOp "Can't go beyond default scope"

/// Unmaps modeKeyTriple up to given scope.
/// Maps keySeq to modeKeyTriple in given scope.
let mapKey keyMappings scope modeKeyTriple keySeq =
    let scope = scope |> Option.defaultValue keyMappings.scope

    if keyMappings.scope < Scope.buffer then
        invalidOp "Must start from buffer or extract buffer scope"
    if scope <= Scope.``default`` then
        invalidOp "Can't map key in default scope"

    if isScopeInvalid keyMappings scope then
        Error $"Invalid key mapping's scope for this buffer: '{scope}'"
    else
        mapKeyRec keyMappings scope modeKeyTriple keySeq
        Ok ()

let rec private unmapKeyRec keyMappings scope modeKeyTriple =
    keyMappings.dict.Remove modeKeyTriple |> ignore

    if keyMappings.scope <> scope then
        if keyMappings.parent.IsSome then
            unmapKeyRec keyMappings.parent.Value scope modeKeyTriple
        else
            invalidOp "Can't go beyond default scope"

/// Unmaps modeKeyTriple up to given scope.
let unmapKey keyMappings scope modeKeyTriple =
    let scope = scope |> Option.defaultValue keyMappings.scope

    if keyMappings.scope < Scope.buffer then
        invalidOp "Must start from buffer or extract buffer scope"
    if scope <= Scope.``default`` then
        invalidOp "Can't unmap value in default scope"

    if isScopeInvalid keyMappings scope then
        Error $"Invalid key mapping's scope for this buffer: '{scope}'"
    else
        unmapKeyRec keyMappings scope modeKeyTriple
        Ok ()

let rec private getKeySequenceAux keyMappings modeKeyTriple =
    if keyMappings.dict.ContainsKey modeKeyTriple then
        Some keyMappings.dict[modeKeyTriple]
    elif keyMappings.parent.IsSome then
        getKeySequenceAux keyMappings.parent.Value modeKeyTriple
    else
        None

/// Returns keySeq mapped to modeKeyTriple or None if there is no such mapping.
let getKeySequence keyMappings modeKeyTriple =
    getKeySequenceAux keyMappings modeKeyTriple

// key mappings construction

/// Returns new default key mappings with no parent.
let private makeDefaultKeyMappings () =
    {
        scope  = Scope.``default``
        parent = None
        dict   = KeyMappingsDict ()
    }

/// Common default key mappings.
let private defaultKeyMappings = makeDefaultKeyMappings ()

/// Returns new global key mappings with defaultKeyMappings as parent.
let makeGlobalKeyMappings () =
    {
        scope  = Scope.``global``
        parent = Some defaultKeyMappings
        dict   = KeyMappingsDict ()
    }

/// Returns new buffer key mappings with given parent.
let makeBufferKeyMappings parent =
    {
        scope  = Scope.buffer
        parent = Some parent
        dict   = KeyMappingsDict ()
    }

/// Returns new extract buffer key mappings with given parent.
let makeBufferExtractKeyMappings parent =
    {
        scope  = Scope.extract
        parent = Some parent
        dict   = KeyMappingsDict ()
    }
