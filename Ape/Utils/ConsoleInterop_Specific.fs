module ConsoleInterop_Specific

open System.Runtime.InteropServices;

open ConsoleInterop_Common

type RuntimeOSPlatform =
    | Undefined
    | Windows
    | Linux
    | FreeBSD
    | OSX

let private getOSPlatform () =
    if   RuntimeInformation.IsOSPlatform OSPlatform.Windows then
        Windows
    elif RuntimeInformation.IsOSPlatform OSPlatform.Linux   then
        Linux
    elif RuntimeInformation.IsOSPlatform OSPlatform.FreeBSD then
        FreeBSD
    elif RuntimeInformation.IsOSPlatform OSPlatform.OSX     then
        OSX
    else
        Undefined

/// OS platform we are running on.
let osPlatform = getOSPlatform ()

/// This instance provides OS platform dependent methods.
let consoleInterop =
    match osPlatform with
    | Windows -> ConsoleInterop_Windows.ConsoleInterop () :> IConsoleInterop
    | Linux   -> ConsoleInterop_Linux  .ConsoleInterop () :> IConsoleInterop
    | _       -> ConsoleInterop_Linux  .ConsoleInterop () :> IConsoleInterop
