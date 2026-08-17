module FilePathUtils

open System

open ConsoleInterop_Specific

let private isCaseSensitive =
    match osPlatform with
    | RuntimeOSPlatform.Undefined
    | RuntimeOSPlatform.Windows   -> false
    | RuntimeOSPlatform.Linux
    | RuntimeOSPlatform.FreeBSD
    | RuntimeOSPlatform.OSX       -> true

let private osCaseSensitivenessFun =
    if isCaseSensitive then
        id
    else
        Char.ToLower

/// Returns true if all strings in strs has the same character on position charIndex.
/// Assumes that strs is not empty.
let private allTheSame (strs: string array) charIndex =
    let mutable result = true

    let s = strs[0]

    if charIndex >= s.Length then
        false
    else
        let c = osCaseSensitivenessFun s[charIndex]

        for i = 1 to strs.Length - 1 do
            let s = strs[i]

            if charIndex >= s.Length || osCaseSensitivenessFun s[charIndex] <> c then
                result <- false

        result

/// Returns the common prefix of given strings. Assumes that strs is not empty.
let getCommonPrefix (strs: string array) =
    let rec loop charIndex =
        if allTheSame strs charIndex then
            loop (charIndex + 1)
        else
            charIndex

    let suffixLength = loop 0

    strs[0].Substring (0, suffixLength)

/// Returns true if s1 equals s2, case sensitively or not depending on the OS platform.
let equalsWithOsCaseSensitiveness (s: string) (prefix: string) =
    if isCaseSensitive then
        String.Equals (s, prefix, StringComparison.Ordinal)
    else
        String.Equals (s, prefix, StringComparison.OrdinalIgnoreCase)

/// Returns true if s1 equals s2, case sensitively.
let equalsCaseSensitive (s: string) (prefix: string) =
    String.Equals (s, prefix, StringComparison.Ordinal)
