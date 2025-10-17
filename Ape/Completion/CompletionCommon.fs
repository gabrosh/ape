module CompletionCommon

open System

open CommandArgs
open CompletionUtils
open StringInCompl

// empty context
type CompletionContext = unit

type CompleteFun =
    // context -> argsMap -> stringInCompl -> completions
    CompletionContext -> ArgsMap -> StringInCompl
        -> Completion seq

/// Indicates that completion is not available for the command.
let complete_none: CompleteFun list =
    []

/// Returns a new sequence with strings starting with commandInCompl (ignoring case), wrapped in Completed.
let keepCommandsStartingWith (commandInCompl: string) (seq: string seq) : Completion seq =
    seq
    |> Seq.filter (
        fun s -> s.ToLower().StartsWith(commandInCompl, StringComparison.OrdinalIgnoreCase)
    )
    |> Seq.map Completed

/// Returns a new sequence with strings starting with argInCompl.unescaped (ignoring case), adjusted and wrapped in Both.
let keepArgsStartingWith (argInCompl: StringInCompl) (seq: string seq) : Completion seq =
    seq
    |> Seq.filter (
        fun s -> s.ToLower().StartsWith(argInCompl.unescaped, StringComparison.OrdinalIgnoreCase)
    )
    |> Seq.map (
        fun s -> Both (
            s, [| s |> adjustCompleted argInCompl.quoteType |]
        )
    )

/// Returns a new sequence with all strings, adjusted and wrapped in Both.
let private wrapInBoth (argInCompl: StringInCompl) (seq: string seq) =
    seq
    |> Seq.map (
        fun s -> Both (
            s, [| s |> adjustCompleted argInCompl.quoteType |]
        )
    )

/// Returns suggested encodings according to argInCompl.
let getSuggestedEncodings (argInCompl: StringInCompl) =
    if argInCompl.unescaped.Length <> 0 then
        FileUtils.encodingsArray
        |> keepArgsStartingWith argInCompl
    else
        FileUtils.suggestedEncodings
        |> wrapInBoth argInCompl
