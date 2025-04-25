module CharCategories

open Context
open DataTypes

// frequently used characters' definitions

let charAfterEOL = ' '
let charAfterEOF = '~'

// character categories and word boundaries

type CharCategory =
    | WhiteSpace
    | Ident
    | Other

/// Returns character c's category.
let getCharCategory c =
    if System.Char.IsWhiteSpace c then
        WhiteSpace
    elif System.Char.IsLetterOrDigit c || c = '_' then
        Ident
    else
        Other

/// Returns true if chars starts with a non-white-space character.
let private startsWithNonWhiteSpace (chars: Chars) =
    if not chars.IsEmpty then
        (getCharCategory chars[0]) <> WhiteSpace
    else
        false

/// Returns true if char_ is at the first character of a word.
let isAtWordStart (lines: Lines) line char_ =
    let chars = lines[line]

    if char_ = chars.Length then
        false
    elif char_ = 0 then
        let current = getCharCategory chars[char_]
        current <> WhiteSpace
    else
        let current = getCharCategory chars[char_]
        let previous = getCharCategory chars[char_ - 1]
        current <> previous && current <> WhiteSpace

/// Returns true if char_ is at the last character of a word.
let isAtWordEnd (lines: Lines) line char_ =
    let chars = lines[line]

    if char_ = chars.Length then
        false
    elif char_ = chars.Length - 1 then
        let current = getCharCategory chars[char_]
        current <> WhiteSpace
    else
        let current = getCharCategory chars[char_]
        let next = getCharCategory chars[char_ + 1]
        current <> next && current <> WhiteSpace

/// Returns true if char_ is before the first character of a word.
let isBeforeWordStart (lines: Lines) line char_ =
    let chars = lines[line]

    if char_ = chars.Length - 1 then
        false
    elif char_ = chars.Length then
        if line + 1 < lines.Count then
            let nextChars = lines[line + 1]
            startsWithNonWhiteSpace nextChars
        else
            true
    else
        let current = getCharCategory chars[char_]
        let next = getCharCategory chars[char_ + 1]
        current <> next && next <> WhiteSpace

/// Returns true if char_ is after the last character of a word.
let isAfterWordEnd (lines: Lines) line char_ =
    let chars = lines[line]

    if char_ = 0 then
        if line > 0 then
            false
        else
            true
    elif char_ = chars.Length then
        let previous = getCharCategory chars[char_ - 1]
        previous <> WhiteSpace
    else
        let current = getCharCategory chars[char_]
        let previous = getCharCategory chars[char_ - 1]
        current <> previous && previous <> WhiteSpace

/// Returns true if char_ is at the first character of a word.
let isAtWordStartSingleLine (chars: Chars) char_ =
    if char_ = chars.Length then
        false
    elif char_ = 0 then
        let current = getCharCategory chars[char_]
        current <> WhiteSpace
    else
        let current = getCharCategory chars[char_]
        let previous = getCharCategory chars[char_ - 1]
        current <> previous && current <> WhiteSpace

/// Returns true if char_ is after the last character of a word.
let isAfterWordEndSingleLine (chars: Chars) char_ =
    if char_ = 0 then
        false
    elif char_ = chars.Length then
        let previous = getCharCategory chars[char_ - 1]
        previous <> WhiteSpace
    else
        let current = getCharCategory chars[char_]
        let previous = getCharCategory chars[char_ - 1]
        current <> previous && previous <> WhiteSpace

/// Returns true if char_ is at character c.
let isAtChar c (lines: Lines) line char_ =
    let chars = lines[line]

    if char_ = chars.Length then
        false
    else
        chars[char_] = c

// line wrapping

let private isWhiteSpace c =
    getCharCategory c = WhiteSpace

let private isSeparator c =
    [| ','; ';'; '.' |] |> Array.contains c

/// Returns length of the word starting at startChar and count of adjacent
/// characters which should be placed on the same line as the word.
let getWordLength (chars: Chars) startChar =
    let mutable i = startChar + 1

    while i < chars.Length && not (isAfterWordEndSingleLine chars i) do
        i <- i + 1

    let mutable adjChars = 0

    if i + 0 >= chars.Length || isWhiteSpace chars[i + 0] then
        adjChars <- adjChars + 1
    elif isSeparator chars[i + 0] then
        if i + 1 >= chars.Length || isWhiteSpace chars[i + 1] then
            adjChars <- adjChars + 2

    i - startChar, adjChars

// rendering

/// Returns number of spaces for filling a <tab> starting at column.
let getTabSpaces tabStop column =
    tabStop - column % tabStop

/// Returns character to be displayed for given cOpt.
let getCharToDisplay cOpt =
    match cOpt with
    | Char c when c = Utils.charTab -> Utils.charSpace
    | Char c                        -> c
    | EOL                           -> charAfterEOL
    | NoChar                        -> charAfterEOL

/// Returns colors for normal characters of line in or after linesCount.
let getNormalColors (context: AreaContext) linesCount line =
    if line < linesCount then
        context.colorScheme.normal
    else
        context.colorScheme.lineAfterEof

/// Returns the main cursor color corresponding to given cOpt.
let getMainCursorColors (context: AreaContext) cOpt =
    match cOpt with
    | EOL -> context.colorScheme.mainCursorAtEol
    | _   -> context.colorScheme.mainCursor

/// Returns non-main cursor color corresponding to given cOpt.
let getNonMainCursorColors (context: AreaContext) cOpt =
    match cOpt with
    | EOL -> context.colorScheme.nonMainCursorAtEol
    | _   -> context.colorScheme.nonMainCursor
