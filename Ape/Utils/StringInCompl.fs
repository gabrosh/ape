module StringInCompl

open System.Text

/// QuoteType is the type of the quotation prefix for a string to be completed.

type QuoteType =
    | NotQuoted  // string without any quotation prefix, eg. asd
    | Quoted     // string with quotation prefix, eg. "asd
    | AtQuoted   // string with at-quotation prefix, eg. @"asd

/// StringInCompl represents a string to be completed.

type StringInCompl = {
    quoteType : QuoteType  // quotation prefix of the string
    orig      : string     // string to be completed, without the quotation prefix
    unescaped : string     // unescaped version of the string
}

/// Used when the string to be completed was not recognized.
let noStringInCompl = {
    quoteType = NotQuoted
    orig      = ""
    unescaped = ""
}

/// Unescapes quoted string. Replaces every pair of \<char> with <char>,
/// with the exception of \t, which is replaced by tab character.
let unescapeQuoted (s: string) =
    let sb = StringBuilder ()

    let mutable toEscape = false

    for c in s do
        if toEscape then
            if c = 't'
            then sb.Append '\t' |> ignore
            else sb.Append c    |> ignore

            toEscape <- false
        elif c = '\\' then
            toEscape <- true
        else
            sb.Append c |> ignore

    sb.ToString ()

/// Unescapes at-quoted string. Replaces every pair of "<char> with <char>.
let unescapeAtQuoted (s: string) =
    let sb = StringBuilder ()

    let mutable toEscape = false

    for c in s do
        if toEscape then
            sb.Append c |> ignore

            toEscape <- false
        elif c = '"' then
            toEscape <- true
        else
            sb.Append c |> ignore

    sb.ToString ()

/// Escapes given string to be used as a quoted string.
let private escapeForQuoted (s: string) =
    let sb = StringBuilder ()

    for c in s do
        if c = '\\' then
            sb.Append "\\\\" |> ignore
        elif c = '\t' then
            sb.Append "\\" |> ignore
            sb.Append "t"  |> ignore
        else
            sb.Append c |> ignore

    sb.ToString ()

/// Escapes given string to be used as an at-quoted string.
let private escapeForAtQuoted (s: string) =
    let sb = StringBuilder ()

    for c in s do
        if c = '"' then
            sb.Append "\"\"" |> ignore
        else
            sb.Append c |> ignore

    sb.ToString ()

/// Returns true if given string contains a space character.
let private isQuotingNeeded (s: string) =
    s.Contains " "

/// Escapes given completed string according to quoteType.
let private escapeCompleted (quoteType: QuoteType) (s: string) =
    match quoteType with
    | NotQuoted ->
        if isQuotingNeeded s then
            escapeForAtQuoted s
        else
            s
    | Quoted    ->
        escapeForQuoted   s
    | AtQuoted  ->
        escapeForAtQuoted s

/// Adds quotation prefix to given completed string according to quoteType.
let private prefixCompleted (quoteType: QuoteType) (s: string) =
    let prefix =
        match quoteType with
        | NotQuoted ->
            if isQuotingNeeded s then
                "@\""
            else
                ""
        | Quoted    ->
            "\""
        | AtQuoted  ->
            "@\""

    prefix + s

/// Escapes given completed string according to quoteType and adds quotation prefix to it.
let adjustCompleted (quoteType: QuoteType) (s: string) =
    s
    |> escapeCompleted quoteType
    |> prefixCompleted quoteType
