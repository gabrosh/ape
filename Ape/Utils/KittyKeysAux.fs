module KittyKeysAux

open System
open System.Text

[<Flags>]
type KittyModifiers =
    | None      = 0b00000000
    | Shift     = 0b00000001
    | Alt       = 0b00000010
    | Ctrl      = 0b00000100
    | Super     = 0b00001000
    | Hyper     = 0b00010000
    | Meta      = 0b00100000
    | Caps_lock = 0b01000000
    | Num_lock  = 0b10000000

type Escaped = {
    unicode:   char option
    shifted:   char option
    layout:    char option
    text:      char option
    modifiers: KittyModifiers
    endChar:   char
}

let Escaped_zero = {
    unicode   = None
    shifted   = None
    layout    = None
    text      = None
    modifiers = KittyModifiers.None
    endChar   = '\x00'
}   

type KittyKey =
    | KittyUndefined
    | KittyEscaped of Escaped
    | KittyChar    of char
    | ConsoleKey   of ConsoleKey * ConsoleModifiers

type private ProgressiveFlags =
    | None                       = 0b00000
    | DisambiguateEscapeCodes    = 0b00001
    | ReportEventTypes           = 0b00010
    | ReportAlternateKeys        = 0b00100
    | ReportAllKeysAsEscapeCodes = 0b01000
    | ReportAssociatedText       = 0b10000

let private initFlags =
    int (
            ProgressiveFlags.DisambiguateEscapeCodes
        ||| ProgressiveFlags.ReportAlternateKeys
        ||| ProgressiveFlags.ReportAssociatedText
    )

let writeInitRequest () =
    // Set progressive enhancements.
    Console.Write $"\x1b[>{initFlags}u"
    // Query progressive enhancements set above.
    Console.Write "\x1b[?u"
    // Query primary device attributes.
    Console.Write "\x1b[c"

let writeDeinitRequest () =
    Console.Write "\x1b[<u"

let readInitResponse () =
    let sb = StringBuilder ()

    let mutable keyChar = '\x00'

    while keyChar <> 'c' do
        let keyInfo = Console.ReadKey true
        keyChar <- keyInfo.KeyChar
        sb.Append keyChar |> ignore

    sb.ToString ()

let supportsKittyProtocol (initResponse: string) =
    // Are the progressive enhancements set?
    initResponse.StartsWith $"\x1b[?{initFlags}u"

let private parseKeyCode (s: string) =
    if s = "" then
        None
    else
        Some (char (UInt32.Parse s))
        
let private parseModifiers (s: string) =
    if s = "" then
        KittyModifiers.None
    else
        let bits = (Int32.Parse s) - 1
        enum<KittyModifiers> bits

let private parseKeyCodes (s: string) =
    match s.Split ':' with
    | [| "" |] ->
        (Some '\x01', None, None)
    | [| unicode |] ->
        (parseKeyCode unicode, None, None)
    | [| unicode; shifted |] ->
        (parseKeyCode unicode, parseKeyCode shifted, None)
    | [| unicode; shifted; layout |] ->
        (parseKeyCode unicode, parseKeyCode shifted, parseKeyCode layout)
    | _ ->
        (None, None, None)
        
let private parseText (s: string) =
    parseKeyCode s    

let parseEscaped (s: string) =
    let e  = s[s.Length - 1]
    let s' = s.Substring (0, s.Length - 1)
    
    match s'.Split ';' with
    | [| keyCodes |] ->
        let u, s, l = parseKeyCodes keyCodes
        {
            unicode = u; shifted = s; layout = l; text = None
            modifiers = KittyModifiers.None; endChar = e
        }

    | [| keyCodes; modifs |] ->
        let u, s, l = parseKeyCodes keyCodes
        let m = parseModifiers modifs
        {
            unicode = u; shifted = s; layout = l; text = None
            modifiers = m; endChar = e
        }

    | [| keyCodes; modifs; text |] ->
        let u, s, l = parseKeyCodes keyCodes
        let m = parseModifiers modifs
        let t = parseText text
        {
            unicode = u; shifted = s; layout = l; text = t
            modifiers = m; endChar = e
        }

    | _ ->
        Escaped_zero

let private readEscaped () =
    let sb = StringBuilder ()

    let mutable doContinue = true

    while doContinue do
        let keyInfo = Console.ReadKey true
        let keyChar = keyInfo.KeyChar

        sb.Append keyChar |> ignore

        match keyChar with
        | 'u' | '~'
        | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'H' | 'P' | 'Q' | 'R' | 'S' -> 
            doContinue <- false
        | _ ->
            ()

    //Console.WriteLine $"CSI {sb.ToString ()}"

    sb.ToString () |> parseEscaped

/// Reads one input key from the console.
let readInputKey () =
    let keyInfo = Console.ReadKey true
    let keyChar = keyInfo.KeyChar
    
    if keyChar = '\x1B' then
        let keyInfo' = Console.ReadKey true
        let keyChar' = keyInfo'.KeyChar
        
        if keyChar' = '\x5b' then
            KittyEscaped (readEscaped ())
        else
            KittyUndefined
            
    elif keyChar <> '\x00' then
        KittyChar keyChar
        
    else
        ConsoleKey (keyInfo.Key, keyInfo.Modifiers)

// representing KittyKey values for testing purposes

let private intToDecStr (v: int) =
    v.ToString ()

let private getCharRepr (c: char) =
    if Char.IsControl c then
        let decStr = intToDecStr (int c)
        $"<{decStr}>"
    elif int c > 127 then
        let decStr = intToDecStr (int c)
        $"<{decStr}>"        
    else
        c.ToString ()
        
let private getCharOptionRepr (c: char option) =
    c |> Option.map getCharRepr |> Option.defaultValue "" 
        
let private getParsedEscapedRepr pe =
    let u = getCharOptionRepr pe.unicode 
    let s = getCharOptionRepr pe.shifted 
    let l = getCharOptionRepr pe.layout  
    let t = getCharOptionRepr pe.text    
    
    $"u:{u} s:{s} l:{l} t:{t} m:{pe.modifiers} e:{pe.endChar}"

let getKittyKeyRepr (kittyKey: KittyKey) =
    match kittyKey with
    | KittyUndefined ->
        "Undefined"
    | KittyEscaped escaped ->
        getParsedEscapedRepr escaped
    | KittyChar keyChar ->
        getCharRepr keyChar
    | ConsoleKey (consoleKey, modifs) ->
        $"{consoleKey}, {modifs}"
