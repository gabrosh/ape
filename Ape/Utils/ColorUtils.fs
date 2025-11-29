module ColorUtils

open System
open System.Collections.Generic
open System.IO
open System.Text.Json

open ColorSchemes
open DefaultColors

let private colorsFileDir = AppContext.BaseDirectory

let private colorsFileName = "colors.json"

let private getColorsFilePath () =
    IO.Path.Combine (
        colorsFileDir, colorsFileName
    )

let mutable schemesArray = [|
    "dark"
    "light"
    "rgbDark"
    "rgbLight"
|]

let mutable defaultScheme = "dark"

let mutable schemesMap = Map [
    "dark"     , darkScheme
    "light"    , lightScheme
    "rgbDark"  , rgbDarkScheme
    "rgbLight" , rgbLightScheme
]

type private ColorException (message) =
    inherit Exception (message)

let private parseColorString (colorString: string) =
    if colorString.StartsWith "#" then
        // Parse RGB hex color (e.g., "#0A0B0C")
        let colorRGB = colorString.Substring 1

        if colorRGB.Length = 6 then
            try
                let r = Convert.ToByte (colorRGB.Substring (0, 2), 16)
                let g = Convert.ToByte (colorRGB.Substring (2, 2), 16)
                let b = Convert.ToByte (colorRGB.Substring (4, 2), 16)
                RGBColor (r, g, b)
            with
                | :? FormatException ->
                    raise (ColorException $"Invalid RGB color format: {colorString}")
        else
            raise (ColorException $"Invalid RGB color format: {colorString}")
    else
        // Parse color name (e.g., "black", "darkGray")
        let colorName = colorString.ToLowerInvariant ()

        match indColorNamesLowerMap.TryFind colorName with
        | Some value ->
            IndColor value
        | _ ->
            raise (ColorException $"Unknown color name: {colorString}")

let private parseCharColors (element: JsonElement) =
    {
        fg = parseColorString (element.GetProperty("fg").GetString())
        bg = parseColorString (element.GetProperty("bg").GetString())
    }

let private parseMatches (matchesElement: JsonElement) (schemeName: string) =
    let matches = 
        matchesElement.EnumerateArray ()
        |> Seq.map parseCharColors
        |> Array.ofSeq

    if isMatchesOk matches then
        matches
    else
        raise (ColorException $"Wrong match colors count in color scheme '{schemeName}'")

let private parseScheme (schemeElement: JsonElement) (schemeName: string) =
    try
        {
            normal             = parseCharColors (schemeElement.GetProperty "normal"            )
            lineNumber         = parseCharColors (schemeElement.GetProperty "lineNumber"        )
            lineAfterEof       = parseCharColors (schemeElement.GetProperty "lineAfterEof"      )
            selection          = parseCharColors (schemeElement.GetProperty "selection"         )
            matches            = parseMatches    (schemeElement.GetProperty "matches"           ) schemeName
            mainCursor         = parseCharColors (schemeElement.GetProperty "mainCursor"        )
            nonMainCursor      = parseCharColors (schemeElement.GetProperty "nonMainCursor"     )
            mainCursorAtEol    = parseCharColors (schemeElement.GetProperty "mainCursorAtEol"   )
            nonMainCursorAtEol = parseCharColors (schemeElement.GetProperty "nonMainCursorAtEol")
            status             = parseCharColors (schemeElement.GetProperty "status"            )
            statusNormalMode   = parseCharColors (schemeElement.GetProperty "statusNormalMode"  )
            statusIsRecording  = parseCharColors (schemeElement.GetProperty "statusIsRecording" )
            promptModePrefix   = parseCharColors (schemeElement.GetProperty "promptModePrefix"  )
            promptInsertPaste  = parseCharColors (schemeElement.GetProperty "promptInsertPaste" )
            promptRegister     = parseCharColors (schemeElement.GetProperty "promptRegister"    )
            completion         = parseCharColors (schemeElement.GetProperty "completion"        )
            activeCompletion   = parseCharColors (schemeElement.GetProperty "activeCompletion"  )
            statusInfo         = parseCharColors (schemeElement.GetProperty "statusInfo"        )
            statusWarning      = parseCharColors (schemeElement.GetProperty "statusWarning"     )
            statusError        = parseCharColors (schemeElement.GetProperty "statusError"       )
        }
    with
        | :? KeyNotFoundException as ex ->
            raise (ColorException $"Wrong format in color scheme '{schemeName}'")

let private readColorsFile () =
    let filePath = getColorsFilePath ()

    try
        let jsonContent = File.ReadAllText filePath
        let jsonDocument = JsonDocument.Parse jsonContent
        jsonDocument.RootElement
    with
        | :? FileNotFoundException as ex ->
            raise (ColorException $"Can't read color definitions file: '{colorsFileName}'")
        | :? JsonException ->
            raise (ColorException $"Wrong format of color definitions file: '{colorsFileName}'")
            
/// Loads colors definition file and sets module's variables
/// schemesArray, defaultScheme and schemesMap if succeeded.
let reloadColors () =
    try
        let root = readColorsFile ()
        
        // Parse defaultScheme.
        let newDefaultScheme =
            try
                root.GetProperty("defaultScheme").GetString();
            with
                | :? KeyNotFoundException as ex ->
                    raise (ColorException $"Missing 'defaultScheme' attribute in color definitions file")
        
        // Parse schemes.
        let schemes =
            try
                root.GetProperty("schemes").EnumerateObject()
            with
                | :? KeyNotFoundException as ex ->
                    raise (ColorException $"Missing 'schemes' attribute in color definitions file")

        let newSchemesMap =
            schemes
            |> Seq.map (
                fun scheme -> scheme.Name, parseScheme scheme.Value scheme.Name
            )
            |> Map.ofSeq
        
        let newSchemesArray =
            newSchemesMap
            |> Map.keys
            |> Array.ofSeq
        
        schemesArray  <- newSchemesArray
        defaultScheme <- newDefaultScheme
        schemesMap    <- newSchemesMap

        Ok ()
    with
        | :? ColorException as ex ->
            Error ex.Message

// initialization

let initResult = reloadColors ()
