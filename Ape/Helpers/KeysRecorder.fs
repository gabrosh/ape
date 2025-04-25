module KeysRecorder

open System.Text

open DataTypes

/// KeyRecorder registers keys and moves registered keys into given register.

type KeysRecorder (registers: Registers.Registers) =
    let myKeysString = StringBuilder ()

    /// Append given key to internal keys string builder.
    member _.AppendKey (key: ConsoleKeys.Key) =
        myKeysString.Append (KeysStrings.keyToKeyString key)
            |> ignore

    /// Stores content of internal string builder into given register and
    /// clears internal string builder. If the string builder is empty, it
    /// removes given register.
    member _.MoveKeysToRegister register =
        if myKeysString.Length <> 0 then
            let keysString = myKeysString.ToString ()

            myKeysString.Clear ()
                |> ignore

            registers.CreateOrClear register
            let lines = ResizeArray [stringToChars keysString]
            registers.ApplyToSlot register 0 lines
        else
            registers.Remove register
