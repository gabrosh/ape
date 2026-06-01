module ConsoleInterop_Common

type WindowSize = {
    width:  int
    height: int
}

type ClipboardType =
    | windows = 0
    | x11     = 1
    | wayland = 2
    | wsl     = 3

let clipboardTypesArray =
    System.Enum.GetNames typeof<ClipboardType>

type IConsoleInterop =
    /// Disables process termination on pressing Ctrl-C.
    abstract member DisableExitOnCtrlC:
        unit -> unit

    /// If available, sets console output mode as needed.
    abstract member SetConsoleOutputMode:
        unit -> unit

    /// Returns the current size of the console window or zero size in the case of failure.
    abstract member GetConsoleWindowSize:
        unit -> WindowSize

    /// If available, returns true if CAPS LOCK is turned on.
    abstract member GetCapsLock:
        unit -> bool

    /// Returns default clipboard type.
    abstract member GetDefaultClipboardType:
        unit -> ClipboardType

    /// Returns true if given clipboard type is supported.
    abstract member IsClipboardTypeSupported:
        ClipboardType -> bool
    
    /// Sets the clipboard type to use.
    abstract member SetClipboardType:
        ClipboardType -> unit
    
    /// Returns the text from the clipboard.
    abstract member GetClipboardText:
        unit -> string

    /// Sets the clipboard text.
    abstract member SetClipboardText:
        string -> unit
