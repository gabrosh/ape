module ExecutionCommon

open System

open CommandArgs
open Registers
open TextArea
open UserMessages

let helpFileName   = "help.txt"
let defaultCfgName = "default"
let cfgFileExt     = ".cfg"

let cfgFilesDir = AppContext.BaseDirectory

let getCfgFilePath cfgName =
    IO.Path.Combine (
        cfgFilesDir, cfgName + cfgFileExt
    )

type ExecuteCfgFileFun =
    UserMessages -> TextArea -> Registers -> string -> unit

type ExecutionContext = {
    userMessages:      UserMessages
    textArea:          TextArea
    registers:         Registers
    executeCfgFileFun: ExecuteCfgFileFun
}

type ExecuteFun =
    ExecutionContext -> ArgsMap -> bool
