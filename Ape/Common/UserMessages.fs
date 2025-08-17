module UserMessages

open System

open DataTypes

[<Struct>]
type UserMessageType =
    | InfoMessage
    | WarningMessage
    | ErrorMessage

let private priorities = Map [
    InfoMessage    , 1
    WarningMessage , 2
    ErrorMessage   , 3
]

// standard UserMessages

let ERROR_UNEXPECTED_ERROR =
    ErrorMessage   , "Unexpected error: '{0}'"

let ERROR_UNKNOWN_COMMAND =
    ErrorMessage   , "Unknown command: '{0}'"

let ERROR_RECURSION_LIMIT_WAS_REACHED =
    ErrorMessage   , "Recursion limit was reached: {0}"

let WARNING_NON_TRANSLATABLE_BYTES =
    WarningMessage , "File contains non-translatable bytes: '{0}'"

let WARNING_BUFFER_ALREADY_OPENED =
    WarningMessage , "Buffer already opened: '{0}'"
let ERROR_BUFFER_OPENED_AS_READ_ONLY =
    ErrorMessage   , "Buffer opened as read-only."
let WARNING_NO_WRITE_SINCE_LAST_CHANGE =
    WarningMessage , "No write since last change. Add ! to override."
let WARNING_NO_CHANGE_SINCE_LAST_WRITE =
    WarningMessage , "No change since last write. Add ! to override."
let WARNING_FILE_ALREADY_EXISTS =
    WarningMessage , "File already exists: '{0}'. Add ! to override."

let INFO_ONLY_ONE_BUFFER =
    InfoMessage    , "Only one buffer."
let INFO_BACK_AT_FIRST_BUFFER =
    InfoMessage    , "Back at the first buffer."
let INFO_BACK_AT_LAST_BUFFER =
    InfoMessage    , "Back at the last buffer."
let INFO_ALREADY_AT_FIRST_BUFFER =
    InfoMessage    , "Already at the first buffer."
let INFO_ALREADY_AT_LAST_BUFFER =
    InfoMessage    , "Already at the last buffer."

let INFO_ROTATION_HIT_BOTTOM =
    InfoMessage    , "Rotation hit bottom, continuing at top."
let INFO_ROTATION_HIT_TOP =
    InfoMessage    , "Rotation hit top, continuing at bottom."
let ERROR_SELECTIONS_REGISTER_IS_EMPTY =
    ErrorMessage   , "Selections register is empty: '{0}'"

let ERROR_INVALID_REGISTER_NAME =
    ErrorMessage   , "Invalid register name: '{0}'"
let ERROR_EMPTY_REGISTER_VALUE =
    ErrorMessage   , "Empty register value: '{0}'"

let ERROR_NOTHING_TO_SEARCH_FOR =
    ErrorMessage   , "Nothing to search for."
let WARNING_NO_MATCH_FOUND =
    WarningMessage , "No match found."
let INFO_NO_SELECTIONS_DISCARDED =
    InfoMessage    , "No selections discarded."
let WARNING_ALL_SELECTIONS_DISCARDED =
    WarningMessage , "All selections discarded."

let WARNING_MATCH_LINES_COUNT_LIMIT_EXCEEDED =
    WarningMessage , "Match lines count limit exceeded."

let WARNING_SEARCH_STOPPED_AT_BOTTOM =
    WarningMessage , "Search stopped at bottom."
let WARNING_SEARCH_STOPPED_AT_TOP =
    WarningMessage , "Search stopped at top."
let WARNING_SEARCH_HIT_BOTTOM_CONT_AT_TOP =
    WarningMessage , "Search hit bottom, continuing at top."
let WARNING_SEARCH_HIT_TOP_CONT_AT_BOTTOM =
    WarningMessage , "Search hit top, continuing at bottom."

let ERROR_OP_INVALID_ON_EXTRACT_BUFFER =
    ErrorMessage   , "Operation invalid on an extract buffer."
let WARNING_CHILD_BUFFER_STILL_OPENED =
    WarningMessage , "Child buffer still opened."

let WARNING_CHAR_NOT_FOUND =
    WarningMessage , "Character not found."
let WARNING_PAIRING_CHAR_NOT_FOUND =
    WarningMessage , "Pairing character not found."
let ERROR_CURSOR_AT_NON_PAIRED_CHAR =
    ErrorMessage   , "Cursor at non-paired character."

let ERROR_CURSOR_AT_WHITE_SPACE =
    ErrorMessage   , "Cursor at white-space character."

// main functionality

type UserMessage = UserMessageType * string

/// Returns message with text made by formatting given arg.
let formatMessage (message: UserMessage) arg =
    (fst message, String.Format (snd message, [| arg |]))

/// Returns info message with given text.
let makeInfoMessage text =
    (InfoMessage, text)

/// Returns warning message with given text.
let makeWarningMessage text =
    (WarningMessage, text)

/// Returns error message with given text.
let makeErrorMessage text =
    (ErrorMessage, text)

/// Returns true is message is an Error message.
let isErrorMessage message =
    match message with
    | Some (ErrorMessage, _) -> true
    | _                      -> false

let private hasHigherPriority a (b: _ option) =
    b.IsNone || priorities[fst a] >  priorities[fst b.Value]

let private hasEqualOrHigherPriority a (b: _ option) =
    b.IsNone || priorities[fst a] >= priorities[fst b.Value]

let private logFilePath = IO.Path.Combine (
    AppContext.BaseDirectory, "Ape.log"
)

/// Logs exception ex, returns true if logging succeeded.
let private logException (ex: Exception) =
    let s =
        Utils.toTimestampString DateTime.Now +
        "Exception caught\n   " +
        (ex.GetType ()).ToString () + ": " + ex.Message + "\n" +
        "Stack trace:\n" + ex.StackTrace + "\n\n";

    try
        IO.File.AppendAllText (
            logFilePath, s, Text.UTF8Encoding false
        )
        true
    with _ ->
        false

let private exceptionToMessage (ex: Exception) =
    match ex with
    | :? System.ArgumentException when
        ex.Message.StartsWith ("The path is empty.", StringComparison.Ordinal) ->
            makeErrorMessage "Empty file path."
    | :? RegexMatchTimeoutException ->
        makeErrorMessage ex.Message
    | _ ->
        formatMessage ERROR_UNEXPECTED_ERROR ex.Message

// rendering

let private getMessageColors (colorScheme: Colors.Scheme) type_ =
    match type_ with
    | InfoMessage    ->
        colorScheme.statusInfo
    | WarningMessage ->
        colorScheme.statusWarning
    | ErrorMessage
    | _              ->
        colorScheme.statusError

/// Returns user message string and colors.
let getMessageStringAndColors
    (colorScheme: Colors.Scheme) (message: UserMessage option) fillLength =

    match message with
    | Some message ->
        let type_, text = message

        let colors = getMessageColors colorScheme type_

        if text.Length <= fillLength then
            (text.PadRight fillLength, colors)
        elif fillLength > 2 then
            let s = text.Substring (0, fillLength - 2)
            (s + "..", colors)
        else
            ("..", colors)
    | None ->
        let colors = colorScheme.normal

        ("".PadRight fillLength, colors)

/// UserMessages registers or updates a single message and provides it to be displayed
/// to the user. It also provides information about the type of the registered message.

type UserMessages () =
    let mutable myMessage: UserMessage option = None

    /// Returns true if any message is registered.
    member _.HasAnyMessage =
        myMessage.IsSome

    /// Returns true if an InfoMessage is registered.
    member _.HasInfoMessage =
        match myMessage with
        | Some (InfoMessage   , _) -> true
        | _                        -> false

    /// Returns true if an ErrorMessage is registered.
    member _.HasErrorMessage =
        match myMessage with
        | Some (ErrorMessage  , _) -> true
        | _                        -> false

    /// Returns true if an ErrorMessage or WarningMessage is registered.
    member _.HasErrorOrWarningMessage =
        match myMessage with
        | Some (WarningMessage, _)
        | Some (ErrorMessage  , _) -> true
        | _                        -> false

    /// Registers given message, replaces a message with lower priority.
    member _.RegisterMessage message =
        if hasHigherPriority message myMessage then
            myMessage <- Some message

    /// Registers given message, replaces a message with the same or lower priority.
    member _.RegisterUpdatedMessage message =
        if hasEqualOrHigherPriority message myMessage then
            myMessage <- Some message

    /// Registers and eventually logs given exception.
    member this.RegisterException ex =
        let _logSucceeded = logException ex
        let message = exceptionToMessage ex
        this.RegisterMessage message

    /// Retrieves and forgets previously registered message.
    /// Returns None if there is no registered message.
    member _.RetrieveMessage () =
        let message = myMessage
        myMessage <- None
        message
