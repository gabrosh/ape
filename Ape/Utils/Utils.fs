module Utils

open System

// frequently used characters' definitions

let charSpace = ' '
let charTab   = '\t'

// Returns true if char c can be displayed directly.
let canBeDisplayed c =
    not (System.Char.IsControl c)

/// If bool_ is true, returns Some value, otherwise returns None.
let optionOfPair (bool_, value) =
    if bool_ then Some value else None

/// If result is (Ok value), returns value, otherwise throws an exception.
let resultGet result =
    match result with
    | Ok value -> value
    | Error _  ->
        invalidOp "Result must be (Ok value)"

/// Returns n ending characters of string s.
let endSubstring (s: string) n =
    s.Substring (s.Length - n, n)

/// Converts dateTime to string in timestamp format.
let toTimestampString (dateTime: DateTime) =
    String.Format (
        "{0:yyyy-MM-dd HH:mm:ss.fff} ", dateTime
    )

/// Returns decimal width of given number.
let getDecimalWidth (a: uint64) =
    if a = 0UL then
        1
    else
        let mutable m = a
        let mutable n = 0

        while m <> 0UL do
            n <- n + 1
            m <- m / 10UL

        n
