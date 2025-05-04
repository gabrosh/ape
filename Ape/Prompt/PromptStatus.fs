module PromptStatus

open Common
open DataTypes
open ConsoleKeys

let private getPromptPrefixAndChar promptType =
    match promptType with
    | CommandPrompt               -> (""       , ':' )
    | SearchPrompt (true , false) -> (""       , '/' )
    | SearchPrompt (false, false) -> ("^"      , '/' )
    | SearchPrompt (true , true ) -> (""       , '?' )
    | SearchPrompt (false, true ) -> ("^"      , '?' )
    | ExtractPrompt               -> ("extract", ':' )
    | SelectPrompt                -> ("select" , ':' )
    | KeepPrompt                  -> ("keep"   , ':' )
    | DiscardPrompt               -> ("discard", ':' )

/// Returns count of chars displayed at the beginning of the prompt line.
let getPromptModeLength promptType =
    let prefix, _c = getPromptPrefixAndChar promptType
    prefix.Length + 1

let private makeDisplayChars (s: string) c (colorScheme: Colors.Scheme) colors =
    let result = ResizeArray<DisplayChar> ()

    for c in s do
        result.Add { c = c; colors = colorScheme.promptModePrefix }

    result.Add { c = c; colors = colors }

    result

/// Returns chars displayed at the beginning of the prompt line.
let getPromptModeDisplayChars colorScheme mode (keyPrefix: Key option) =
    match mode with
    | PromptInsertMode (promptType, PromptInsertMainState)
     -> if keyPrefix.IsSome then
            let p, _ = getPromptPrefixAndChar promptType
            let c = keyPrefix |> Option.get |> keyPrefixToChar |> Option.get
            makeDisplayChars p c colorScheme colorScheme.promptInsertPaste
        else
            let p, c = getPromptPrefixAndChar promptType
            makeDisplayChars p c colorScheme colorScheme.promptModePrefix

    | PromptInsertMode (promptType, (PromptInsertPasteState isUpper))
     -> let p, _ = getPromptPrefixAndChar promptType
        let c = if isUpper then 'R' else 'r'
        makeDisplayChars p c colorScheme colorScheme.promptInsertPaste

    | PromptNormalMode (promptType, PromptNormalMainState)
        when keyPrefix.IsSome

     -> let p, _ = getPromptPrefixAndChar promptType
        let c = keyPrefix |> Option.get |> keyPrefixToChar |> Option.get
        makeDisplayChars p c colorScheme colorScheme.statusNormalMode

    | PromptNormalMode (promptType, PromptNormalMainState)
     -> if keyPrefix.IsSome then
            let p, _ = getPromptPrefixAndChar promptType
            let c = keyPrefix |> Option.get |> keyPrefixToChar |> Option.get
            makeDisplayChars p c colorScheme colorScheme.statusNormalMode
        else
            let p, c = getPromptPrefixAndChar promptType
            makeDisplayChars p c colorScheme colorScheme.statusNormalMode

    | PromptNormalMode (promptType, (PromptRegisterState (ToSelectRegisterState isUpper)))
     -> let p, _ = getPromptPrefixAndChar promptType
        let c = if isUpper then '"' else '''
        makeDisplayChars p c colorScheme colorScheme.statusNormalMode

    | PromptNormalMode (promptType, (PromptRegisterState (SelectedRegisterState (_isUpper, c))))
     -> let p, _ = getPromptPrefixAndChar promptType
        makeDisplayChars p c colorScheme colorScheme.promptRegister

    | PromptNormalMode (promptType, PromptGoToState isExtending)
     -> let p, _ = getPromptPrefixAndChar promptType
        let c = if isExtending then 'G' else 'g'
        makeDisplayChars p c colorScheme colorScheme.statusNormalMode

    | _
     -> invalidOp ""
