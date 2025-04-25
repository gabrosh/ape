module Common

open System

open Commands.InCommands
open Commands.OutCommands
open Context
open DataTypes
open Position
open PositionClassifier
open Selection

// editor mode

type FindCharState =
    | LeftToCharState
    | LeftUntilCharState
    | RightToCharState
    | RightUntilCharState

type RegisterState =
    | ToSelectRegisterState of upper: bool
    | SelectedRegisterState of upper: bool * c: char

// count = 0 for RegisterState and FindCharState indicates no count.
type NormalModeState =
    | NormalMainState
    | CountState        of count: int
    | RegisterState     of count: int * RegisterState
    | FindCharState     of count: int * isExtending: bool * FindCharState
    | FillWithCharState of count: int
    | GoToState         of isExtending: bool
    | ViewState

type InsertModeState =
    | InsertMainState
    | InsertPasteState of upper: bool

type PromptType =
    | CommandPrompt
    | SearchPrompt of isForward: bool * isExtending: bool
    | SelectPrompt
    | KeepPrompt
    | DiscardPrompt

type PromptNormalModeState =
    | PromptNormalMainState
    | PromptRegisterState of RegisterState
    | PromptGoToState     of isExtending: bool

type PromptInsertModeState =
    | PromptInsertMainState
    | PromptInsertPasteState of upper: bool

type Mode =
    | NormalMode       of NormalModeState
    | InsertMode       of InsertModeState
    | PromptNormalMode of PromptType * PromptNormalModeState
    | PromptInsertMode of PromptType * PromptInsertModeState

let isPromptMode mode =
    match mode with
    | PromptNormalMode _
    | PromptInsertMode _
        -> true
    | _
        -> false

let isInsertMode mode =
    match mode with
    | InsertMode _
        -> true
    | _
        -> false

let isPromptInsertMode mode =
    match mode with
    | PromptInsertMode _
        -> true
    | _
        -> false

// display renderer

type DisplayPos = {
    line:    int
    lineRow: int
    column:  int
}

let DisplayPos_Zero = {
    line    = 0
    lineRow = 0
    column  = 0
}

let DisplayPos_Invalid = {
    line    = IntType.MaxValue
    lineRow = IntType.MaxValue
    column  = IntType.MaxValue
}

type IDisplayPosToken =
    abstract member Modify:
           ModifyingOutCommand
        -> unit

type IDisplayRenderer =
    /// Returns first and last columns corresponding to line and char_.
    abstract member GetCharColumns:
           line: int -> char_: int
        -> int * int

    /// Returns hard and soft wanted columns corresponding to line and char_.
    abstract member GetCharWantedColumns:
           line: int -> char_: int
        -> int * int

    /// Returns index of the last line to be displayed starting at display line and row.
    abstract member GetLastDisplayedLine:
           DisplayPos
        -> int

    /// Returns display rows starting at display line, row and column.
    abstract member GetDisplayRows:
           DisplayPos -> PositionClassifier
        -> ResizeArray<ResizeArray<DisplayChar>>

    /// Returns display position token prepared for later modifications.
    abstract member GetDisplayPosToken:
           InCommand -> DisplayPos
        -> IDisplayPosToken

    /// Returns display position deriving it from given display position token.
    abstract member GetModifiedDisplayPos:
           IDisplayPosToken
        -> DisplayPos

    /// Returns selection made by copying given selection to the preceding line
    /// according to its hard wanted columns.
    abstract member GetCopiedSelectionUp:
           Selection
        -> Selection option

    /// Returns selection made by copying given selection to the succeeding line
    /// according to its hard wanted columns.
    abstract member GetCopiedSelectionDown:
           Selection
        -> Selection option

    /// Disallows lines cache used for virtual and real rendering.
    /// Subsequent use of it will throw System.NullReferenceException.
    abstract member DisallowLinesCache:
           unit
        -> unit

    /// Initializes lines cache used for virtual and real rendering.
    abstract member ResetLinesCache:
           unit
        -> unit

    /// Trims lines cache to the neighborhood of firstLine to lastLine range.
    abstract member TrimLinesCacheIfNeeded:
           firstLine: int -> lastLine: int
        -> unit

/// Returns string for line number column in the text area.
let getLineNumberString (context: AreaContext) line =
    if context.showLineNumbers then
        let lineNumber = posToUserPos line
        let s = lineNumber.ToString () + " "
        s.PadLeft context.lineNumbersWidth
    else
        ""

/// Returns filling string for line number column in the text area.
let getLineNumberFillString (context: AreaContext) =
    if context.showLineNumbers then
        "".PadLeft context.lineNumbersWidth
    else
        ""

// commands performers

type CommandInState = {
    selection:         Selection
    displayLine:       int
    displayLineRow:    int
    displayColumn:     int
    isSingleSelection: bool
    prevCommand:       InCommand option
}

type CommandOutState = {
    line:              int
    char:              int
    toUpdateSelection: bool
    displayLine:       int
    displayLineRow:    int
    displayColumn:     int
}

type WantedColumnsAction =
    | SetHardWantedColumn
    | SetSoftWantedColumn

type ICommandsPerformer<'Command> =
    /// Performs command on given state of the performer. Returns new state
    /// of the performer and OutCommand to be performed on the caller side.
    abstract member PerformCommand:
           CommandInState -> 'Command
        -> CommandOutState

    /// Returns wanted columns actions to be performed before and after given command.
    abstract member GetWantedColumnsActions:
           'Command -> isSingleSelection: bool
        -> WantedColumnsAction list *
           WantedColumnsAction list

// completions

type CompletionAction = {
    toDelete: int
    toInsert: Chars
}

type ICompletionItems =
    inherit IDisposable

    /// Tries to set the completion sequence according to itemsToComplete.
    abstract member TrySet:
        itemsToComplete: (Chars * Position) seq -> unit

    /// Clears the completion sequence.
    abstract member Clear:
        unit -> unit

    /// Returns true if the completion is activated.
    abstract member IsInCompletion:
        unit -> bool

    /// Returns the next line from the completion sequence.
    abstract member GetNext:
        unit -> CompletionAction option

    /// Returns the previous line from the completion sequence.
    abstract member GetPrevious:
        unit -> CompletionAction option

    /// Returns string with all possible completions and offset
    /// and length of the current completion in this string.
    abstract member GetCompletionsRow:
        unit -> (string * int * int)
