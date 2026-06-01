// Based on the implementation of BashRunner class in the TextCopy library:
// https://github.com/CopyText/TextCopy/blob/main/src/TextCopy/BashRunner.cs

module BashRunner

open System
open System.Diagnostics
open System.Text

let private WaitForExit (process_: Process) =
    process_.WaitForExit 500

let Run (commandLine: string) : string =
    let errorBuilder = StringBuilder ()
    let outputBuilder = StringBuilder ()
    let arguments = $"-c \"{commandLine}\""
    
    use process_ = new Process()
    
    process_.StartInfo <- ProcessStartInfo (
        FileName = "bash",
        Arguments = arguments,
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        UseShellExecute = false,
        CreateNoWindow = false
    )
    
    process_.Start () |> ignore
    
    process_.OutputDataReceived.Add (
        fun args ->
            if not (String.IsNullOrEmpty args.Data) then
                outputBuilder.AppendLine args.Data |> ignore
    )
    process_.BeginOutputReadLine ()
    
    process_.ErrorDataReceived.Add (
        fun args ->
            if not (String.IsNullOrEmpty args.Data) then
                errorBuilder.AppendLine args.Data |> ignore
    )
    process_.BeginErrorReadLine ()
    
    if not (WaitForExit process_) then
        let message = 
            $"Process timed out. Command line: bash {arguments}.\n" +
            $"Output: {outputBuilder}\n" +
            $"Error: {errorBuilder}"
        raise (Exception message)
    
    if process_.ExitCode = 0 then
        outputBuilder.ToString ()
    else
        let message = 
            $"Could not execute process. Command line: bash {arguments}.\n" +
            $"Output: {outputBuilder}\n" +
            $"Error: {errorBuilder}"
        raise (Exception message)
