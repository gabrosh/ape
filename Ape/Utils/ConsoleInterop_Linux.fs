module ConsoleInterop_Linux

open System

open ConsoleInterop_Common

type ConsoleInterop () =

    interface IConsoleInterop with

        member _.DisableExitOnCtrlC () =
            Console.CancelKeyPress.Add (
                fun arg ->
                    arg.Cancel <- true
            )

        /// Not available this OS platform.
        member _.SetConsoleOutputMode () =
            ()

        member _.GetConsoleWindowSize () =
            {
                width  = Console.WindowWidth
                height = Console.WindowHeight
            }

        /// Not available on this OS platform.
        member _.GetCapsLock () =
            false
