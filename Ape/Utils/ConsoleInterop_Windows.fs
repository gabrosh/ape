module ConsoleInterop_Windows

#nowarn "9"  // FS0009: Possible unverifiable code

open Microsoft.FSharp.NativeInterop
open System
open System.Runtime.InteropServices

open ConsoleInterop_Common

let private INVALID_HANDLE_VALUE = System.IntPtr -1
let private STD_OUTPUT_HANDLE    = uint -11

let private ENABLE_PROCESSED_OUTPUT            = uint 0x0001
let private ENABLE_WRAP_AT_EOL_OUTPUT          = uint 0x0002
let private ENABLE_VIRTUAL_TERMINAL_PROCESSING = uint 0x0004

// common definitions

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type COORD =
    val mutable X: int16
    val mutable Y: int16

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type SMALL_RECT =
    val mutable Left:  int16
    val mutable Top:   int16
    val mutable Right: int16
    val mutable Bottom:int16

// common functions

[<DllImport("kernel32")>]
extern uint private GetLastError()

[<DllImport("kernel32")>]
extern System.IntPtr private GetStdHandle(uint nStdHandle)

[<DllImport("kernel32")>]
extern bool private GetConsoleMode(System.IntPtr hConsoleHandle, uint* lpMode)

[<DllImport("kernel32")>]
extern bool private SetConsoleMode(System.IntPtr hConsoleHandle, uint dwMode)

// GetConsoleScreenBufferInfo

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type CONSOLE_SCREEN_BUFFER_INFO =
    val mutable dwSize:              COORD
    val mutable dwCursorPosition:    COORD
    val mutable wAttributes:         uint16
    val mutable srWindow:            SMALL_RECT
    val mutable dwMaximumWindowSize: COORD

[<DllImport("kernel32")>]
extern bool private GetConsoleScreenBufferInfo(
    System.IntPtr hConsoleOutput, CONSOLE_SCREEN_BUFFER_INFO* lpConsoleScreenBufferInfo
)

type ConsoleInterop () =

    interface IConsoleInterop with

        member _.DisableExitOnCtrlC () =
            Console.CancelKeyPress.Add (
                fun arg ->
                    arg.Cancel <- true
            )

        /// Disables wrapping at the end of line mode of the console.
        /// Enables processed output and virtual terminal processing.
        member _.SetConsoleOutputMode () =
            let handle = GetStdHandle STD_OUTPUT_HANDLE
            if handle = INVALID_HANDLE_VALUE then
                invalidOp $"GetStdHandle failed, error code {GetLastError ()}"

            let mode = NativePtr.stackalloc<uint> 1
            let result = GetConsoleMode (handle, mode)
            if not result then
                invalidOp $"GetConsoleMode failed, error code {GetLastError ()}"

            let mode = NativePtr.read mode
            let mode = mode &&& ~~~ (
                ENABLE_WRAP_AT_EOL_OUTPUT
            )
            let mode = mode ||| (
                    ENABLE_PROCESSED_OUTPUT
                ||| ENABLE_VIRTUAL_TERMINAL_PROCESSING
            )
            let result = SetConsoleMode (handle, mode)
            if not result then
                invalidOp $"SetConsoleMode failed, error code {GetLastError ()}"

        member this.GetConsoleWindowSize () =
            let info = this.GetConsoleScreenBufferInfo ()
            let width  = info.srWindow.Right - info.srWindow.Left + (int16 1)
            let height = info.srWindow.Bottom - info.srWindow.Top + (int16 1)

            // When the application is running in cmd.exe console,
            // Console.WindowWidth returns -4 in certain conditions.
            if width > int16 0 then
                { width = int width; height = int height }
            else
                { width = 0; height = 0 }

        member _.GetCapsLock () =
            Console.CapsLock

    /// Returns information about the specified console screen buffer.
    member private _.GetConsoleScreenBufferInfo () =
        let handle = GetStdHandle STD_OUTPUT_HANDLE
        if handle = INVALID_HANDLE_VALUE then
            invalidOp $"GetStdHandle failed, error code {GetLastError ()}"

        let info = NativePtr.stackalloc<CONSOLE_SCREEN_BUFFER_INFO> 1
        let result = GetConsoleScreenBufferInfo (handle, info)
        if not result then
            invalidOp $"GetConsoleScreenBufferInfo failed, error code {GetLastError ()}"

        NativePtr.read info
