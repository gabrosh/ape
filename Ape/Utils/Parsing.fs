module Parsing

type FUFF = FileUtils.FileFormat

// parsing user input

let parseBool (value: string) =
    match value with
    | "false" | "f" -> Ok false
    | "true"  | "t" -> Ok true
    | _             -> Error $"Must be Bool: '{value}'"

let parseInt (value: string) =
    match System.Int32.TryParse value with
    | true, x -> Ok x
    | _       -> Error $"Must be Int: '{value}'"

let parseString (value: string) =
    Ok value

let parseEncoding (value: string) =
    if FileUtils.encodingsSet |> Seq.contains value then
        Ok value
    else
        Error $"Invalid encoding: '{value}'"

let parseFileFormat (value: string) =
    match System.Enum.TryParse<FUFF> value with
    | true, x -> Ok x
    | _       -> Error $"Must be FileFormat: '{value}'"
