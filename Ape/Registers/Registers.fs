module Registers

open System.Collections.Generic
open TextCopy

open DataTypes
open Register

/// Returns true if c can represent a register.
let isRegisterChar (c: char) =
    Utils.canBeDisplayed c

/// Returns true if s can represent a register.
let isRegisterString (s: string) =
    s.Length = 1 && isRegisterChar s[0]

/// Converts register string to register char.
let registerStringToChar (s: string) =
    s[0]

let parseRegisterString (s: string) =
    if isRegisterString s then
        Ok (registerStringToChar s)
    else
        Error $"Invalid register name: '{s}'"

let defaultRegisterName   = '"'
let clipboardRegisterName = '''
let recordingRegisterName = '.'

/// Returns true if it is an uppercase register, and register name.
let private getRegisterKindAndName register =
    let isUpper, c =
        match register with
        | DefaultRegister -> (false, defaultRegisterName)
        | SelectedRegister (isUpper, c) -> (isUpper, c)

    if System.Char.IsUpper c then
        (true, System.Char.ToLower c)
    else
        (isUpper, c)

/// Returns register name.
let getRegisterName register =
    let _isUpper, c = getRegisterKindAndName register
    c

/// Returns true if register is an uppercase register.
let isUpperRegister register =
    let isUpper, _c = getRegisterKindAndName register
    isUpper

/// Returns true if register is the clipboard register.
let isClipboardRegister register =
    let _isUpper, c = getRegisterKindAndName register
    c = clipboardRegisterName

let private isPastableToPromptAux (lines: Lines) =
    (
           lines.Count = 1
        && not lines[0].IsEmpty
    ) || (
           lines.Count = 2
        && not lines[0].IsEmpty
        && lines[1].IsEmpty
    )

/// Returns true if lines is pastable to prompt.
let isPastableToPrompt (lines: Lines option) =
    match lines with
    | Some lines
        -> isPastableToPromptAux lines
    | None
        -> true

/// Returns the first item of lines. Assumes that lines is pastable to prompt.
let getPastableToPrompt (lines: Lines option) =
    match lines with
    | Some lines
        -> Some (Lines [lines[0]])
    | _
        -> None

/// Registers holds a dictionary of named registers and performs operations on them.

type Registers () =
    let myRegisters = Dictionary<char, Register> ()

    /// Removes register.
    member _.Remove register =
        let name = getRegisterName register

        myRegisters.Remove name |> ignore

    /// Creates or clears register.
    member this.CreateOrClear register =
        let name = getRegisterName register

        this.CreateOrClearRegister name

    /// Sets or appends lines to register slot "index".
    member this.ApplyToSlot register index lines =
        let isUpper, name = getRegisterKindAndName register

        if isUpper then
            this.AppendToSlot name index lines
        else
            this.SetSlot name index lines

    /// Returns count of register slots.
    /// If there is no such register, returns None.
    member _.GetSlotsCount register =
        let name = getRegisterName register

        if myRegisters.ContainsKey name then
            Some (myRegisters[name].GetSlotsCount ())
        else
            None

    /// Returns register slot "index" or all register slots joined together.
    /// If there is no such register and slot, returns None.
    member _.GetSlot register index =
        let isUpper, name = getRegisterKindAndName register

        if myRegisters.ContainsKey name then
            if isUpper then
                myRegisters[name].GetJoinedSlots ()
            else
                myRegisters[name].GetSlot index
        else
            None

    /// Copies the clipboard to the first register slot.
    member this.CopyFromClipboardToRegister register =
        let name = getRegisterName register

        this.CreateOrClearRegister name

        let text = ClipboardService.GetText ()
        if text <> null then
            myRegisters[name].SetFromString text

    /// Copies all register slots joined together using new line character to the clipboard.
    member _.CopyFromRegisterToClipboard register =
        let name = getRegisterName register

        if myRegisters.ContainsKey name then
            let text = myRegisters[name].GetAsString ()
            ClipboardService.SetText text
        else
            ClipboardService.SetText ""

    member private _.CreateOrClearRegister name =
        if not (myRegisters.ContainsKey name) then
            myRegisters.Add (name, Register ())
        else
            myRegisters[name].Clear ()

    member private this.SetSlot name index lines =
        this.AssureRegisterExists name
        myRegisters[name].SetSlot index lines

    member private this.AppendToSlot name index lines =
        this.AssureRegisterExists name
        myRegisters[name].AppendToSlot index lines

    member private _.AssureRegisterExists name =
        if not (myRegisters.ContainsKey name) then
            myRegisters.Add (name, Register ())
