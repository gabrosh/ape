module RegexUtils

open System
open System.Text

open TextRanges

type private Regex        = System.Text.RegularExpressions.Regex
type private RegexOptions = System.Text.RegularExpressions.RegexOptions

/// Regex identifying regular expressions that can match the new line character
let private canMatchNewLineIdent = Regex @"\\[ns]"

/// Returns true if regex can match the new line character.
let canMatchNewLine (regex: string) =
    canMatchNewLineIdent.IsMatch regex

/// Regex identifying multi-line regular expressions
let private isMultiLineRegexIdent = Regex @"^\(\?[^\)]*m"

/// Returns true if regex is multi-line expression.
let isMultiLineRegex (regex: string) =
    isMultiLineRegexIdent.IsMatch regex

/// Returns true if regex has any capturing group with lower-letter name.
let hasColoredGroups (regexObject: Regex) =
    regexObject.GetGroupNames () |> Seq.exists isColoredGroup

/// Returns color index for given group name.
let getColorIndex (s: string) =
    if isMainGroup s then
        Colors.mainGroupColor
    else
        let c = s[0]

        let delta =
            if   'A' <= c && c <= 'Z' then int (c - 'A')
            elif 'a' <= c && c <= 'z' then int (c - 'a')
            else invalidOp "Invalid group name"

        delta % Colors.colorGroupsCount + 1

/// Standard match timeout
let private matchTimeout = 10  // seconds

/// Returns a new regular expression object for given pattern.
let makeRegexObject pattern =
    Regex (
        pattern,
        RegexOptions.Compiled ||| RegexOptions.IgnoreCase,
        TimeSpan.FromSeconds (matchTimeout: int64)
    )

/// Returns a new regular expression object for given pattern.
/// RegularExpressions.RegexParseException is transformed to Error,
/// exceptions of other types are propagated without change.
let tryMakeRegexObject pattern =
    try
        Ok (makeRegexObject pattern)
    with
        | :? RegularExpressions.RegexParseException as ex ->
            Error ex.Message
