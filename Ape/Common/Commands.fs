namespace Commands

open DataTypes
open Position

module InCommands =

    type CommonCommand =
        | CursorLeft
        | CursorRight
        | CursorAfterSelection of afterEof: bool
        | CursorLeftAtWordStart
        | CursorLeftAtWordEnd
        | CursorLeftAfterWordEnd
        | CursorLeftToChar     of char
        | CursorLeftUntilChar  of char
        | CursorRightAtWordStart
        | CursorRightAtWordEnd
        | CursorRightBeforeWordStart
        | CursorRightToChar    of char
        | CursorRightUntilChar of char
        | CursorHardLineStart
        | CursorHardLineEnd
        | CursorAtEol
        | CursorBeforeEol
        | CursorAt             of int
        | CursorToNextMatch    of isInitial: bool  // added CenterAfterMatch
        | CursorToPrevMatch    of isInitial: bool  // added CenterAfterMatch
        | CursorToPairChar
        | ClearInfoMessage
        | AssertNonWhiteSpace

    type WrapLinesDepCommand =
        | CursorHardUp
        | CursorHardDown
        | CursorSoftUp
        | CursorSoftDown
        | CursorSoftLineStart
        | CursorSoftLineEnd
        | CursorSoftFileStart
        | CursorSoftFileEnd
        | CursorHardToLine of int
        | ScrollPageUp
        | ScrollPageDown
        | CenterVertically
        | CenterHorizontally
        | CenterAfterMatch of isForward: bool * hitFileBoundary: bool * hitLineBoundary: bool
        | ScrollCursorTop
        | ScrollCursorBottom
        | ScrollCursorLeft
        | ScrollCursorRight
        | AdaptDisplayPos

    type ModifyingCommand =
        | EnterInserting
        | EnterAppending
        | EnterInsertingAtSol
        | EnterAppendingAtEol
        | InsertChar        of char
        | InsertChars       of Chars
        | InsertNewLine
        | InsertNewLineIndent
        | DeleteChar
        | DeletePrevChar
        | DeletePrevChars   of int
        | Yank              of RegisterSelection
        | Delete
        | YankAndDelete     of RegisterSelection  // split
        | PasteBefore       of RegisterSelection  // split
        | PasteAfter        of RegisterSelection  // split
        | PasteBeforeAux    of RegisterSelection * toPrompt: bool
        | Replace           of RegisterSelection  // split
        | AlignSelections                         // replaced
        | AlignSelectionAux of delta: int

    type TextRangesCommand =
        | FillWithChar of char
        | ToUppercase
        | InvertCase
        | TabsToSpaces
        | SpacesToTabs
        | RegexEscape
        | Indent
        | Unindent

    type SelectionsCommand =
        | CopyFirstUp
        | CopyLastDown
        | Multiply        of count: int
        | SelectWholeBuffer
        | InvertSelections
        | SplitOnLineStarts
        | MergeContiguous
        | ExpandToFullLines
        | TrimToFullLines
        | ReduceToCursor
        | ForwardDirection
        | FlipDirection
        | KeepOnlyMain
        | RemoveMain
        | RemoveLessIndented
        | RemoveMoreIndented
        | RotateUp
        | RotateDown
        | SelectMatching  of regex: string
        | KeepMatching    of regex: string
        | DiscardMatching of regex: string
        | Store           of RegisterSelection
        | Load            of RegisterSelection
        | RemoveStored    of RegisterSelection

    type UndoRedoCommand =
        | Undo
        | Redo
        | UndoFast
        | RedoFast
        | AddName   of RegisterSelection
        | UndoNamed of RegisterSelection
        | RedoNamed of RegisterSelection

    type InCommand =
        | CommonCommand       of CommonCommand
        | WrapLinesDepCommand of WrapLinesDepCommand
        | ModifyingCommand    of ModifyingCommand
        | TextRangesCommand   of TextRangesCommand
        | SelectionsCommand   of SelectionsCommand
        | UndoRedoCommand     of UndoRedoCommand

    // Returns true if given command can modify text in the buffer.
    let isWriteCommand command =
        match command with
        | ModifyingCommand (Yank _) -> false
        | ModifyingCommand _        -> true
        | TextRangesCommand _       -> true
        | _                         -> false

module OutCommands =

    type ModifyingOutCommand =
        | ApplyInsert        of InsertSpec
        | ApplyDelete        of DeleteSpec
        | ApplyOneLineInsert of OneLineInsertSpec
        | ApplyOneLineDelete of OneLineDeleteSpec

    type ModifyingOutCommands = ResizeArray<ModifyingOutCommand>

    let ModifyingOutCommands_EmptyReadOnly =
        (ModifyingOutCommands ()).AsReadOnly ()
