module KeyMappings

open System.Collections.Generic

open ConsoleKeys

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
    | _              -> Error $"Invalid key mapping's scope: '{s}'"

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

let rec private mapKeyAux keyMappings scope modeKeyTriple keySeq =
    if keyMappings.scope = scope then
        keyMappings.dict[modeKeyTriple] <- keySeq
    else
        keyMappings.dict.Remove (modeKeyTriple) |> ignore

        if keyMappings.parent.IsSome then
            mapKeyAux keyMappings.parent.Value scope modeKeyTriple keySeq
        else
            invalidOp "Can't go beyond default scope"

/// Unmaps modeKeyTriple up to given scope.
/// Maps keySeq to modeKeyTriple in given scope.
let mapKey keyMappings scope modeKeyTriple keySeq =
    if keyMappings.scope <> Scope.buffer then
        invalidOp "Must start from buffer scope"
    if scope <= Scope.``default`` then
        invalidOp "Can't map key in default scope"

    mapKeyAux keyMappings scope modeKeyTriple keySeq

let rec private unmapKeyAux keyMappings scope modeKeyTriple =
    keyMappings.dict.Remove modeKeyTriple |> ignore

    if keyMappings.scope <> scope then
        if keyMappings.parent.IsSome then
            unmapKeyAux keyMappings.parent.Value scope modeKeyTriple
        else
            invalidOp "Can't go beyond default scope"

/// Unmaps modeKeyTriple up to given scope.
let unmapKey keyMappings scope modeKeyTriple =
    if keyMappings.scope <> Scope.buffer then
        invalidOp "Must start from buffer scope"
    if scope <= Scope.``default`` then
        invalidOp "Can't unmap key in default scope"

    unmapKeyAux keyMappings scope modeKeyTriple

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
