module CompletionCommon

open System

open CommandArgs
open CompletionUtils

// empty context
type CompletionContext = unit

type CompleteFun =
    // context -> argsMap -> argInCompl -> completions
    CompletionContext -> ArgsMap -> string
        -> Completion seq

let complete_none: CompleteFun list =
    []

/// Returns a new sequence with strings starting with value (ignoring case), wrapped in Complete.
let keepStartingWith (value: string) (seq: string seq) =
    seq
    |> Seq.filter (
        fun s -> s.ToLower().StartsWith(value, StringComparison.OrdinalIgnoreCase)
    )
    |> Seq.map Completed

/// Returns a new sequence with all strings, wrapped in Complete.
let wrapInComplete (seq: string seq) =
    seq
    |> Seq.map Completed

/// Returns suggested encodings according to argInCompl.
let getSuggestedEncodings (argInCompl: string) =
    if argInCompl.Length <> 0 then
        FileUtils.encodingsArray
        |> keepStartingWith argInCompl
    else
        FileUtils.suggestedEncodings
        |> wrapInComplete
