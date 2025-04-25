module ExecutionRegisters

open CommandArgs
open ExecutionCommon
open DataTypes
open Registers
open UserMessages

// reg

let argsMapSpec_reg = (2, [| "regName"; "line" |])

let execute_reg context (argsMap: ArgsMap) =
    let regName = argsMap["regName"] |> Option.get
    let line    = argsMap["line"   ] |> Option.get

    if not (isRegisterString regName) then
        context.userMessages.RegisterMessage (
            formatMessage ERROR_INVALID_REGISTER_NAME regName
        )
    elif line.Length = 0 then
        context.userMessages.RegisterMessage (
            formatMessage ERROR_EMPTY_REGISTER_VALUE line
        )
    else
        let lines = ResizeArray [stringToChars line]

        let register = SelectedRegister (false, registerStringToChar regName)

        context.registers.ApplyToSlot register 0 lines
        if isClipboardRegister register then
            context.registers.CopyFromRegisterToClipboard register

    false

// unreg

let argsMapSpec_unreg = (1, [| "regName" |])

let execute_unreg context (argsMap: ArgsMap) =
    let regName = argsMap["regName"] |> Option.get

    if not (isRegisterString regName) then
        context.userMessages.RegisterMessage (
            formatMessage ERROR_INVALID_REGISTER_NAME regName
        )
    else
        let register = SelectedRegister (false, registerStringToChar regName)

        context.registers.Remove register
        if isClipboardRegister register then
            context.registers.CopyFromRegisterToClipboard register

    false
