module ModifyingAuxiliary

open System.Collections.Immutable

open CharCategories
open Commands.OutCommands
open DataTypes

/// If given char is transformed, it returns (true, <new_char>),
/// otherwise it returns (false, <original_char>).
let private charToUppercase c =
    if System.Char.IsLower c then
        (true, System.Char.ToUpper c)
    else
        (false, c)

/// If given char is transformed, it returns (true, <new_char>),
/// otherwise it returns (false, <original_char>).
let private charInvertCase c =
    if System.Char.IsLower c then
        (true, System.Char.ToUpper c)
    elif System.Char.IsUpper c then
        (true, System.Char.ToLower c)
    else
        (false, c)

/// If given char is transformed, it returns (true, <new_char>),
/// otherwise it returns (false, <original_char>).
let private charChangeTo toChar c =
    (c <> toChar, toChar)

/// If any of given chars had to be transformed, it returns
/// (Some <new_chars>), otherwise it returns None.
let private transformChars f (_line: int) from to_ (chars: Chars) =
    let builder = ImmutableArray.CreateBuilder (to_ - from)

    let mutable isChanged = false

    for i = from to to_ - 1 do
        let isCharChanged, c = f chars[i]
        isChanged <- isChanged || isCharChanged
        builder.Add c

    if isChanged then
        Some (builder.ToImmutableArray (), ModifyingOutCommands_EmptyReadOnly)
    else
        None

let toUppercase    = transformChars charToUppercase
let invertCase     = transformChars charInvertCase
let fillWithChar c = transformChars (charChangeTo c)

let private getCharColumn tabStop (chars: Chars) (char_: int) =
    let mutable column = 0

    for i = 0 to char_ - 1 do
        let c = chars[i]

        if c = Utils.charTab then
            let tabSpaces = getTabSpaces tabStop column
            column <- column + tabSpaces
        else
            column <- column + 1

    column

/// Returns number of spaces for filling a new <tab> in chars at char_.
let getSpacesForTab tabStop (chars: Chars) (char_: int) =
    let column = getCharColumn tabStop chars char_
    getTabSpaces tabStop column

let private getInsertCommand line char_ chars preferMove =
    ApplyOneLineInsert {
        line       = line
        target     = char_
        chars      = chars
        preferMove = preferMove
    }

let private getDeleteCommand line char_ chars =
    ApplyOneLineDelete {
        line       = line
        first      = char_
        rightKept  = char_ + chars
    }

let private isEscapeNeeded c =
    match c with
    | '\\'
    | '*' | '+' | '?' | '|' | '^' | '$' | '.' | '#'
    | '{' | '}' | '[' | ']' | '(' | ')' ->
        true
    | _ ->
        false

/// If any of given chars had to be transformed, it returns
/// (Some <new_chars>), otherwise it returns None.
let tabsToSpaces tabStop (line: int) from to_ (chars: Chars) =
    let builder = ImmutableArray.CreateBuilder (to_ - from)

    let outCommands = ModifyingOutCommands ()
    let mutable column = getCharColumn tabStop chars from
    let mutable isChanged = false

    let addFillingSpaces count =
        for _ = 1 to count do
            builder.Add Utils.charSpace

    let addOutCommand chars =
        let char_ = from + builder.Count
        outCommands.Add (
            getInsertCommand line char_ chars false
        )

    for i = from to to_ - 1 do
        let c = chars[i]

        if c = Utils.charTab then
            let tabSpaces = getTabSpaces tabStop column
            // Transforming single character to multiple characters ?
            if tabSpaces > 1 then
                addOutCommand (tabSpaces - 1)
            addFillingSpaces tabSpaces
            column <- column + tabSpaces

            isChanged <- true
        else
            builder.Add c
            column <- column + 1

    if isChanged then
        Some (builder.ToImmutableArray (), outCommands.AsReadOnly ())
    else
        None

/// If any of given chars had to be transformed, it returns
/// (Some <new_chars>), otherwise it returns None.
let spacesToTabs tabStop (line: int) from to_ (chars: Chars) =
    let builder = ImmutableArray.CreateBuilder (to_ - from)

    let outCommands = ModifyingOutCommands ()
    let mutable column = getCharColumn tabStop chars from
    let mutable spaces = 0
    let mutable isChanged = false

    let addAccumulatedSpaces count =
        for _ = 1 to count do
            builder.Add Utils.charSpace

    let addOutCommand chars =
        let char_ = from + builder.Count
        outCommands.Add (
            getDeleteCommand line char_ chars
        )

    for i = from to to_ - 1 do
        let c = chars[i]

        if c = Utils.charSpace then
            column <- column + 1
            spaces <- spaces + 1
        else
            addAccumulatedSpaces spaces
            builder.Add c
            let count =
                if c = Utils.charTab then
                    getTabSpaces tabStop column
                else
                    1
            column <- column + count
            spaces <- 0

        if column % tabStop = 0 && spaces > 0 then
            // Transforming multiple characters to single character ?
            if spaces > 1 then
                addOutCommand (spaces - 1)
            builder.Add Utils.charTab
            spaces <- 0

            isChanged <- true

    addAccumulatedSpaces spaces

    if isChanged then
        Some (builder.ToImmutableArray (), outCommands.AsReadOnly ())
    else
        None

/// If any of given chars had to be transformed, it returns
/// (Some <new_chars>), otherwise it returns None.
let regexEscape (line: int) from to_ (chars: Chars) =
    let builder = ImmutableArray.CreateBuilder (to_ - from)

    let outCommands = ModifyingOutCommands ()
    let mutable isChanged = false

    let addOutCommand chars =
        let char_ = from + builder.Count
        outCommands.Add (
            getInsertCommand line char_ chars false
        )

    for i = from to to_ - 1 do
        let c = chars[i]

        if isEscapeNeeded c then
            addOutCommand 1
            builder.Add '\\'

            isChanged <- true

        builder.Add c

    if isChanged then
        Some (builder.ToImmutableArray (), outCommands.AsReadOnly ())
    else
        None

/// If any of given chars had to be transformed, it returns
/// (Some <new_chars>), otherwise it returns None.
let indent tabStop tabBySpaces (line: int) from to_ (chars: Chars) =
    let builder = ImmutableArray.CreateBuilder (to_ - from)

    if tabBySpaces then
        for _ = 1 to tabStop do
            builder.Add Utils.charSpace
    else
        builder.Add Utils.charTab

    let outCommands = ModifyingOutCommands [
        getInsertCommand line 0 builder.Count true
    ]

    builder.AddRange chars

    Some (builder.ToImmutableArray (), outCommands.AsReadOnly ())

/// If any of given chars had to be transformed, it returns
/// (Some <new_chars>), otherwise it returns None.
let unindent tabStop (line: int) _from _to (chars: Chars) =
    let mutable wasWhiteSpace = true
    let mutable column = 0
    let mutable i      = 0

    while wasWhiteSpace && column < tabStop && i < chars.Length do
        let c = chars[i]

        if c = Utils.charSpace then
            column <- column + 1
            i <- i + 1
        elif c = Utils.charTab then
            column <- column + getTabSpaces tabStop column
            i <- i + 1
        else
            wasWhiteSpace <- false

    if i <> 0 then
        let outCommands = ModifyingOutCommands [
            getDeleteCommand line 0 i
        ]

        Some (chars.RemoveRange (0, i), outCommands.AsReadOnly ())
    else
        None
