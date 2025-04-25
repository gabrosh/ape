module ConsoleInterop_Common

type WindowSize = {
    width:  int
    height: int
}

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
