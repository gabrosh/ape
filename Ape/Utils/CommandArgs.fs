module CommandArgs

open StringInCompl

type private Regex = System.Text.RegularExpressions.Regex
type private Match = System.Text.RegularExpressions.Match

type ArgsMapSpec =
    int * string array

type ArgsMap =
    Map<string, string option>

type ArgsMapResult =
    Result<ArgsMap, string>

type ArgsForCompl =
    string array * StringInCompl

type ArgsForComplResult =
    Result<ArgsForCompl, string>

let ARGUMENTS_PARSING_ERROR = "Arguments parsing error"
let NOT_ENOUGH_ARGUMENTS    = "Not enough arguments"
let TOO_MANY_ARGUMENTS      = "Too many arguments"

/// If given string is quoted, it removes the quotations
/// at the beginning and end of it and unescapes it.
let private adjustArg (s: string) =
    if s.StartsWith '"' && s.EndsWith '"' then
        unescapeQuoted (
            s.Substring (1, s.Length - 2)
        )
    elif s.StartsWith "@\"" && s.EndsWith '"' then
        unescapeAtQuoted (
            s.Substring (2, s.Length - 3)
        )
    else
        s

/// If given string to be completed is quoted, it removes the quotation
/// at the beginning of it and unescapes it. Returns also the quotation type
/// and the original string.
let private adjustArgInCompl (s: string) =
    if s.StartsWith '"' then
        {
            quoteType = Quoted
            orig      = s
            unescaped = unescapeQuoted (
                s.Substring (1, s.Length - 1)
            )
        }
    elif s.StartsWith "@\"" then
        {
            quoteType = AtQuoted
            orig      = s
            unescaped = unescapeAtQuoted (
                s.Substring (2, s.Length - 2)
            )
        }
    else
        {
            quoteType = NotQuoted
            orig      = s
            unescaped = s
        }

[<Literal>]
let quoted          = """("(\\[\\"t]|[^\\"])*")"""
[<Literal>]
let quotedIncompl   = """("(\\[\\"t]|[^\\"])*)"""
[<Literal>]
let atQuoted        = """(@"("["]|[^"])*")"""
[<Literal>]
let atQuotedIncompl = """(@"("["]|[^"])*)"""
[<Literal>]
let other           = """(@|[^\s"@][^\s]*)"""
[<Literal>]
let otherIncompl    = """(|@|[^\s"@][^\s]*)"""

[<Literal>]
let alterns         = quoted        + "|" + atQuoted        + "|" + other
[<Literal>]
let alternsIncompl  = quotedIncompl + "|" + atQuotedIncompl + "|" + otherIncompl

[<Literal>]
let oneArg          = @"(\s+(?'ONE_ARG'"         + alterns        + "))";
[<Literal>]
let oneArgIncompl   = @"(\s+(?'ONE_ARG_INCOMPL'" + alternsIncompl + "))";

/// Splits args into an array of arguments.
let private splitArgsString (args: string) =

    let regex = Regex $@"^{oneArg}*\s*$"

    // Prepend one space to be consumed by the beginning of regex.
    let match_ = regex.Match (" " + args)

    if match_.Success then
        let oneArg' = match_.Groups["ONE_ARG"].Captures

        let args' =
            oneArg'
            |> Seq.map (fun x -> adjustArg x.Value)
            |> Seq.toArray

        Ok args'
    else
        Error $"{ARGUMENTS_PARSING_ERROR}: '{args}'"

/// Splits args into an array of completed arguments and a single incompleted one.
let private splitArgsStringForCompl (args: string) =

    let regex = Regex $@"^{oneArg}*{oneArgIncompl}$"

    // Prepend one space to be consumed by the beginning of regex.
    let match_ = regex.Match (" " + args)

    if match_.Success then
        let oneArg'        = match_.Groups["ONE_ARG"        ].Captures
        let oneArgIncompl' = match_.Groups["ONE_ARG_INCOMPL"].Captures

        let argsCompl =
            oneArg'
            |> Seq.map (fun x -> adjustArg x.Value)
            |> Seq.toArray

        let argsIncompl =
            oneArgIncompl'
            |> Seq.map (fun x -> adjustArgInCompl x.Value)
            |> Seq.toArray

        Ok (argsCompl, argsIncompl[0])
    else
        Error $"{ARGUMENTS_PARSING_ERROR}: '{args}'"

let private getArgsMapRightAux
    (args: string array) mandatoryCount (names: string array) =

    match args.Length with
    | n when n < mandatoryCount ->
        Error NOT_ENOUGH_ARGUMENTS
    | n when n > names.Length   ->
        Error TOO_MANY_ARGUMENTS
    | _ ->
        let delta = names.Length - args.Length

        let pairs =
            names |> Seq.indexed |> Seq.map (
                fun (i, name) ->
                    let argInd = i - delta
                    if argInd >= 0 then
                        (name, Some args[argInd])
                    else
                        (name, None)
            )

        Ok (Map pairs)

/// Splits args into a map of arguments. Mandatory
/// arguments start at the end of args and names.
let getArgsMapRight args mandatoryCount (names: string array) : ArgsMapResult =
    if not (mandatoryCount <= names.Length) then
        invalidOp "Invalid arguments: mandatoryCount > names.Length"

    let args' = splitArgsString args

    match args' with
    | Ok args' ->
        getArgsMapRightAux args' mandatoryCount names
        |> Result.mapError (
            fun e -> $"{e}: '{args}'"
        )
    | Error e ->
        Error e

let private getArgsForComplAux
    (argsCompl: string array) (argInCompl: StringInCompl) maxCount =

    if argsCompl.Length + 1 > maxCount then
        Error TOO_MANY_ARGUMENTS
    else
        Ok (argsCompl, argInCompl)

/// Splits args into an array of completed arguments and a single incompleted one.
/// The total count of arguments must not exceed maxCount.
let getArgsForCompl args maxCount : ArgsForComplResult =
    if not (maxCount >= 0) then
        invalidOp "Invalid arguments: maxCount < 0"

    let result = splitArgsStringForCompl args

    match result with
    | Ok (argsCompl, argIncompl) ->
        getArgsForComplAux argsCompl argIncompl maxCount
        |> Result.mapError (
            fun e -> $"{e}: '{args}'"
        )
    | Error e ->
        Error e
