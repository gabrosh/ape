module MatchRanges

open System.Collections.Generic

open DataTypes
open IMatchRanges
open RegexUtils
open TextRanges
open UserMessages

type private Regex = System.Text.RegularExpressions.Regex

type SearchState = {
    mutable lastRegex:  string option
    mutable isCleared:  bool
    mutable textRanges: Dictionary<string, TextRanges>
}
with
    member this.Clone () =
        {
            lastRegex  = this.lastRegex
            isCleared  = this.isCleared
            textRanges = this.textRanges
        }

type SearchStates = {
    states: SearchState array
    mutable activeStateInd: int
}
with
    member this.Item
        with get index       = this.states[index]
        and  set index value = this.states[index] <- value

    member this.inactiveStateInd = 1 - this.activeStateInd
    
    member this.ActiveState = this.states[this.activeStateInd]
          
    member this.AreAllCleared =
        this.states[0].isCleared && this.states[1].isCleared

    member this.IsSndSearch = this.activeStateInd = 1
        
    member this.ToggleIsSndSearch () =
        this.activeStateInd <-
            if this.activeStateInd = 1 then 0 else 1
                
    member this.Clone () =
        {
            states = this.states |> Array.map (
                fun state -> state.Clone ()
            )
            activeStateInd = this.activeStateInd
        }            
            
let SearchStates_Default () = {
    states = [|
        {
            lastRegex  = None
            isCleared  = true
            textRanges = makeTextRangesGroups ()
        }
        {
            lastRegex  = None
            isCleared  = true
            textRanges = makeTextRangesGroups ()
        }
    |]
    activeStateInd = 0
}

/// MatchRanges holds results of searching for regex matches as a dictionary of
/// TextRanges for the main group and, optionally, several named groups. It provides
/// information about the groups and can perform clearing or re-searching with
/// the last used regex.

type MatchRanges (
    myUserMessages: UserMessages,
    myLines:        Lines,
    mySearchStates: SearchStates
) =
    new (inUserMessages: UserMessages, inLines: Lines) =
        MatchRanges (
            inUserMessages, inLines, SearchStates_Default ()
        )

    // public properties

    member _.AreAllClearedSearch = mySearchStates.AreAllCleared
    
    member _.IsSndSearch = mySearchStates.IsSndSearch
    member _.LastRegex   = mySearchStates.ActiveState.lastRegex
    
    // internal properties

    member internal _.SearchStates = mySearchStates
    
    member _.ToggleSndSearch () =
        mySearchStates.ToggleIsSndSearch ()

    /// Returns count of text ranges in the active main group.
    member this.GetMainGroupCount () =
        this.GetMainGroupCountAux mySearchStates.ActiveState

    /// Returns all text ranges from the active main group.
    member this.GetAllFromMainGroup () =
        this.GetAllFromMainGroupAux mySearchStates.ActiveState

    /// Returns count of text ranges in the main group.
    member internal _.GetMainGroupCountAux state =
        state.textRanges[mainGroupName].Count

    /// Returns all text ranges from the main group.
    member internal _.GetAllFromMainGroupAux state =
        state.textRanges[mainGroupName]

    /// Returns all text ranges from all groups.
    member _.GetAllFromAllGroups () =
        Seq.append
            mySearchStates[0].textRanges
            mySearchStates[1].textRanges
        |> Seq.map (
            fun item -> (getColorIndex item.Key, item.Value)
        )
        // Seq.sortBy is stable sort
        |> Seq.sortBy fst |> Seq.toArray

    /// Returns text ranges from all groups in interval <startLine, endLine>.
    member _.GetInIntervalFromAllGroups startLine endLine =
        Seq.append
            mySearchStates[0].textRanges
            mySearchStates[1].textRanges
        |> Seq.map (
            fun item -> (getColorIndex item.Key, item.Value)
        )
        |> Seq.map (
            fun (colorIndex, textRanges) ->
                (colorIndex, getFromInterval textRanges startLine endLine)
        )
        // Seq.sortBy is stable sort
        |> Seq.sortBy fst |> Seq.toArray

    /// Searches for regex/regexObject going through lines.
    member internal this.SearchNormal state lines regex regexObject =
        state.lastRegex <- Some regex
        state.isCleared <- false

        this.SearchAux state lines regex regexObject

    member internal this.SearchAux state lines regex regexObject =
        if isMultiLineRegex regex then
            this.SearchMultiLine state lines regexObject
        else
            this.SearchSingleLine state lines regexObject

        if this.GetMainGroupCountAux state = 0 then
            myUserMessages.RegisterMessage WARNING_NO_MATCH_FOUND

    member private _.SearchSingleLine (state: SearchState) (lines: Lines) (regexObject: Regex) =
        state.textRanges <- makeTextRangesGroups ()

        let slr = SingleLineRegex.AddMatchesAsTextRanges regexObject
        slr.Init state.textRanges

        for i, chars in lines |> Seq.indexed do
            slr.ProcessLine i 0 chars.Length true chars
                |> ignore
        slr.FinishProcessing ()

    member private _.SearchMultiLine (state: SearchState) (lines: Lines) (regexObject: Regex) =
        state.textRanges <- makeTextRangesGroups ()

        let mlr = MultiLineRegex.AddMatchesAsTextRanges (
            myUserMessages, regexObject, state.textRanges
        )

        for i, chars in lines |> Seq.indexed do
            mlr.ProcessLine i 0 chars.Length true chars
                |> ignore
        mlr.FinishProcessing ()

    /// Searches for the regex used in the last call to SearchNormal going through lines.
    member internal this.ReSearchNormal state lines =
        match state.lastRegex with
        | Some regex ->
            state.isCleared <- false

            this.SearchAux state lines regex (makeRegexObject regex)
        | None ->
            myUserMessages.RegisterMessage ERROR_NOTHING_TO_SEARCH_FOR

    /// Clears text ranges from the last call to Search.
    member internal this.ClearSearchNormal state =
        state.isCleared <- true

        this.ClearSearchAux state

    member internal _.ClearSearchAux state =
        state.textRanges <- makeTextRangesGroups ()

    /// Clears text ranges from the last calls to Search.
    member this.ClearSearchAll () =
        this.ClearSearchNormal mySearchStates[0]
        this.ClearSearchNormal mySearchStates[1]

    // auxiliary
    
    member internal this.SearchAuxIfNotCleared state lines =
        if not state.isCleared then
            // (not isCleared) => (lastRegex <> None)
            let regex = state.lastRegex |> Option.get
            let regexObject = makeRegexObject regex
            
            this.SearchAux state lines regex regexObject
    
    // virtual
            
    /// Searches for regex.
    abstract member Search:
        regex: string
     -> unit

    override this.Search (regex: string) =
        match tryMakeRegexObject regex with
        | Ok regexObject ->
            this.SearchNormal mySearchStates.ActiveState myLines regex regexObject
        | Error e ->
            myUserMessages.RegisterMessage (makeErrorMessage e)

    /// Searches for the regexes used in the last call to Search (or Extract).
    abstract member ReSearch:
        unit -> unit

    override this.ReSearch () =
        this.ReSearchNormal mySearchStates.ActiveState myLines

    /// Updates text ranges after change, according to the previous state.
    abstract member UpdateAfterChange:
        unit -> unit

    override this.UpdateAfterChange () =
        this.SearchAuxIfNotCleared mySearchStates[0] myLines
        this.SearchAuxIfNotCleared mySearchStates[1] myLines

    /// Clears text ranges from the last call to Search.
    abstract member ClearSearch:
        unit -> unit

    override this.ClearSearch () =
        this.ClearSearchNormal mySearchStates.ActiveState

    // IMatchRanges

    interface IMatchRanges with

        member this.GetInIntervalFromAllGroups startLine endLine =
            this.GetInIntervalFromAllGroups startLine endLine

        member this.GetMainGroupCount () =
            this.GetMainGroupCount ()

        member this.GetAllFromMainGroup () =
            this.GetAllFromMainGroup ()
