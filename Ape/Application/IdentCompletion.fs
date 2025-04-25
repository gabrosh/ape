module IdentCompletion

open System.Collections.Generic

open CharCategories
open CompletionUtils
open DataTypes
open Position

let private isCompletable (lineStr: string) (cursorChar: int) =
    if cursorChar = lineStr.Length then
        true
    else
        getCharCategory lineStr[cursorChar] <> Ident

let private getIdentInCompl (line: string) =
    let rec loop (index: int) =
        // Is there a previous char and is it's category Ident ?
        if index > 0 && getCharCategory line[index - 1] = Ident then
            loop (index - 1)
        else
            index

    let startIndex = loop line.Length

    line.Substring startIndex

let private lineEndsWithIdent (line: string) (ident: string) =
    let lineLength  = line.Length
    let identLength = ident.Length

    if line.EndsWith ident then
        if lineLength <> identLength then
            // index of the char before the found ident
            let index = lineLength - identLength - 1
            getCharCategory line[index] <> Ident
        else
            true
    else
        false

[<TailCall>]
let rec checkTailItems
    (identInCompl: string) (positionsSet: HashSet<Position>) (items: (Chars * Position) seq) =

    match items |> Seq.tryHead with

    | Some (chars, cursor) ->
        let lineStr = charsToString chars

        if isCompletable lineStr cursor.char then
            let lineLeft = lineStr.Substring (0, cursor.char)
            let identLength = identInCompl.Length

            if  lineEndsWithIdent lineLeft identInCompl &&
                positionsSet.Add (cursor.Sub 0 identLength)
            then
                checkTailItems identInCompl positionsSet (items |> Seq.tail)
            else
                false
        else
            false

    | None ->
        true

let private checkItems (items: (Chars * Position) seq) =
    let positionsSet = HashSet<Position> ()

    let chars, cursor = items |> Seq.head

    let lineStr = charsToString chars

    if isCompletable lineStr cursor.char then
        let lineLeft = lineStr.Substring (0, cursor.char)
        let identInCompl = getIdentInCompl lineLeft
        let identLength = identInCompl.Length

        positionsSet.Add (cursor.Sub 0 identLength) |> ignore

        if checkTailItems identInCompl positionsSet (items |> Seq.tail) then
            Some (identInCompl, positionsSet)
        else
            None
    else
        None

let private getIdentCompletions
    (lines: Lines) (identInCompl: string) (toSkip: HashSet<Position>) =

    let stringsSet = HashSet<string> ()

    let regex =
        if identInCompl.Length <> 0 then
            @"\b" + identInCompl + @"\w*"
        else
            @"\b\w+"

    let regexObject = RegexUtils.makeRegexObject regex

    let sr = SimpleRegex.AddMatchesAsStringsSet (regexObject, toSkip)
    sr.Init stringsSet

    for line, chars in lines |> Seq.indexed do
        sr.ProcessLine line chars |> ignore

    sr.FinishProcessing ()

    let result = ResizeArray<Completion> stringsSet.Count

    for s in stringsSet do
        result.Add (Complete s)
    result.Sort ()

    result

/// Returns prefix to complete and possible completions for given items to complete.
let getCompletions
    (isFromPrompt: bool) (getLinesFun: unit -> Lines)
    (itemsToComplete: (Chars * Position) seq) =

    match checkItems itemsToComplete with
    | Some (identInCompl, positionsSet) ->
        if isFromPrompt then
            positionsSet.Clear ()

        let toSkip = positionsSet

        let identCompletions =
            getIdentCompletions (getLinesFun ()) identInCompl toSkip

        Ok (identInCompl, identCompletions)
    | None ->
        Error "Cursor(s) at wrong position(s)."
