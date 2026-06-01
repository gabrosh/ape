module ConsoleInterop_Linux

open System
open System.IO

open ConsoleInterop_Common

type ConsoleInterop () =

    let mutable myClipboardType = None
    
    let invalidClipboardType = "Invalid clipboard type"

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

        member _.GetDefaultClipboardType () =
            ClipboardType.x11

        member _.IsClipboardTypeSupported (clipboardType: ClipboardType) = 
            match clipboardType with
            | ClipboardType.x11     
            | ClipboardType.wayland 
            | ClipboardType.wsl     -> true
            | _                     -> false

        member _.SetClipboardType (clipboardType: ClipboardType) =
            myClipboardType <- Some clipboardType

        member this.GetClipboardText () =
            let tempFileName = Path.GetTempFileName ()
            
            try
                let command =
                    match myClipboardType with
                    | Some ClipboardType.x11     ->
                        $"xsel -o --clipboard > {tempFileName}"
                    | Some ClipboardType.wayland ->
                        $"wl-paste -p > {tempFileName}"                 
                    | Some ClipboardType.wsl     ->
                        $"powershell.exe -NoProfile Get-Clipboard > {tempFileName}"
                    | _ ->
                        invalidOp invalidClipboardType

                BashRunner.Run command |> ignore
                
                File.ReadAllText tempFileName
            finally
                File.Delete tempFileName
                
        member _.SetClipboardText text =
            let tempFileName = Path.GetTempFileName ()
            
            File.WriteAllText(tempFileName, text)
            
            try
                let command =
                    match myClipboardType with
                    | Some ClipboardType.x11     ->
                        $"cat {tempFileName} | xsel -i --clipboard"
                    | Some ClipboardType.wayland ->
                        $"wl-copy < {tempFileName}"                
                    | Some ClipboardType.wsl     ->
                        $"cat {tempFileName} | clip.exe"
                    | _ ->
                        invalidOp invalidClipboardType

                BashRunner.Run command |> ignore                
            finally
                File.Delete tempFileName
            