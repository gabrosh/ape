module CompletionRegisters

open CommandArgs
open CompletionCommon
open CompletionUtils

// reg, unreg

let check_regUnreg (argsMap: ArgsMap) =
    let regName = argsMap["regName"]

    let has0 = regName |> Option.map Registers.parseRegisterString
    
    match has0 with
    | Some (Error _)
        -> false
    | _
        -> true

let complete_regUnreg_regName _context (argsMap: ArgsMap) (_argInCompl: string) =
    if check_regUnreg argsMap then
        seq { ListOnly "#regName"}
    else
        noCompletions

let complete_reg_line _context (argsMap: ArgsMap) (_argInCompl: string) =
    if check_regUnreg argsMap then
        seq { ListOnly "#line"}
    else
        noCompletions

let complete_reg: CompleteFun list = [
    complete_regUnreg_regName
    complete_reg_line
]

let complete_unreg: CompleteFun list = [
    complete_regUnreg_regName
]
