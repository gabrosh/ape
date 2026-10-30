module PromptHistoryVirtualFile

open Common
open IVirtualFile
open Prompt
open FileUtils

type PromptHistoryVirtualFile(prompt: Prompt) =

    interface IVirtualFile with

        member _.Read filePath =
            match filePath with
            | RegexHistoryFileName   ->
                Some (prompt.RegexHistory.GetAll ())
            | CommandHistoryFileName ->
                Some (prompt.CommandHistory.GetAll ())        
            | _ ->
                None
        
        member _.Write filePath lines =
            match filePath with
            | RegexHistoryFileName   ->
                prompt.RegexHistory.SetAll lines
                Some ()
            | CommandHistoryFileName ->
                prompt.CommandHistory.SetAll lines
                Some ()
            | _ ->
                None
    
        member _.GetDiskFileState filePath =
            match filePath with
            | RegexHistoryFileName   ->
                // The mechanism is usable for regex history.
                Some {
                    name          = filePath
                    size          = 0
                    lastWriteTime = prompt.RegexHistory.LastWriteTime
                }
            | CommandHistoryFileName ->
                // The mechanism is unusable for command history as "write" command
                // is added to command history before getting its LastWriteTime.
                Some (
                    DiskFileState_Default filePath
                )
            | _ ->
                None
