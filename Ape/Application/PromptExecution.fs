module PromptExecution

open Common

/// Executes regex command in promptLine.
/// Returns true if the application is to be exited.
let executePrompt
    userMessages textArea registers promptType promptLine isCurrentFromHistory toOverwrite =

    let mutable toExit = false

    if promptLine <> "" then
        match promptType with
        | CommandPrompt ->
            toExit <- CommandExecution.executeCommand userMessages textArea registers promptLine

        | SearchPrompt ( isForward,  isExtending) ->
            textArea.SearchMatching promptLine isForward isExtending

        | SelectPrompt  ->
            textArea.SelectMatching  promptLine
        | KeepPrompt    ->
            textArea.KeepMatching    promptLine
        | DiscardPrompt ->
            textArea.DiscardMatching promptLine
    else
        match promptType with
        | CommandPrompt -> ()

        | SearchPrompt (_isForward, _isExtending) ->
            if not isCurrentFromHistory then
                if toOverwrite then
                    textArea.ReSearchMatching ()
                else
                    textArea.ClearMatching ()

        | SelectPrompt  -> ()
        | KeepPrompt    -> ()
        | DiscardPrompt -> ()

    toExit
