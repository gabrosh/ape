module UndoRedo

open Common
open Prompt
open TextArea

let private staysInInsertMode mode nextMode =
    isInsertMode mode && isInsertMode nextMode

let private leavesInsertMode mode nextMode =
    isInsertMode mode && not (isInsertMode nextMode)

let private staysInPromptInsertMode mode nextMode =
    isPromptInsertMode mode && isPromptInsertMode nextMode

let private leavesPromptInsertMode mode nextMode =
    isPromptInsertMode mode && not (isPromptInsertMode nextMode)

/// Registers textArea undo state if needed.
let handleTextAreaUndo (textArea: TextArea) mode nextMode =
    if textArea.HasUndoToRegister then
        let isStateVolatile =
               staysInInsertMode mode nextMode
            && textArea.HasUndoLinesToRegister
        textArea.RegisterUndo isStateVolatile

    if leavesInsertMode mode nextMode then
        textArea.ClearIsLastUndoVolatile ()

/// Registers prompt undo state if needed.
let handlePromptUndo (prompt: Prompt) mode nextMode =
    if prompt.HasUndoToRegister then
        let isStateVolatile =
               staysInPromptInsertMode mode nextMode
            && prompt.HasUndoLinesToRegister
        prompt.RegisterUndo isStateVolatile

    if leavesPromptInsertMode mode nextMode then
        prompt.ClearIsLastUndoVolatile ()
