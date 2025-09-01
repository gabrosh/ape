module ITextAreaBuffer

open System

open Commands.InCommands
open DataTypes
open Selections

type ITextAreaBuffer =
    inherit IDisposable

    // properties

    abstract member FilePath: string
        with get, set

    abstract member BufferName: string
        with get, set

    abstract member Lines:                  Lines
    abstract member LinesForCompletion:     Lines
    abstract member Selections:             Selections
    abstract member IsReadOnly:             bool
    abstract member IsBufferChanged:        bool
    abstract member HasUndoToRegister:      bool
    abstract member HasUndoLinesToRegister: bool

    // commands

    abstract member PerformCommand:
        isNormalMode: bool -> isExtending: bool -> command: InCommand -> count: int
     -> unit

    // rendering

    abstract member GetDisplayRows:
        unit
     -> ResizeArray<ResizeArray<DisplayChar>>

    abstract member GetCursorPosForStatusArea:
        unit
     -> CursorPosForStatusArea

    // search matching

    abstract member SearchMatching:
        regex: string -> isForward: bool -> isExtending: bool
     -> unit

    abstract member ReSearchMatching:
        unit -> unit

    abstract member ClearSearchMatching:
        unit -> unit

    // Undo/Redo

    abstract member RegisterUndo:
        isStateVolatile: bool
     -> unit

    abstract member ClearIsLastUndoVolatile:
        unit -> unit

    abstract member UndoCorruptedState:
        unit -> unit

    // others

    abstract member Reload:
        encoding: string -> strictEncoding: bool -> warnIfNoMatchFound: bool
     -> Result<FileUtils.FileFormat * bool, string>

    abstract member WriteFile:
        encoding: string -> fileFormat: FileUtils.FileFormat -> endWithNewLine: bool
     -> unit

    abstract member GetFirstChild:
        unit
     -> ITextAreaBuffer option
