module KeysStringsTest

open NUnit.Framework

open ConsoleKeys
open KeysStrings

[<TestFixture>]
type KeysStringsTest () =
    let invalidKey = CharNoModif '!'  // unused value

    let keysStringToKey keyString =
        let result = parseKeysString keyString

        match result with
        | Ok [| key |]
            -> key
        | _
            -> invalidKey

    // parseKeysString ---------------------------------------------------------

    [<Test>]
    member _.parseKeysString_Ok () =
        let input = "a<c-a><del><c-del>"

        let expected = [|
            NoModif InputKey.A
            Ctrl    InputKey.A
            NoModif InputKey.Delete
            Ctrl    InputKey.Delete
        |]

        let result = parseKeysString input

        Assert.IsTrue (result |> Result.contains expected, input)

    [<Test>]
    member _.parseKeysString_INVALID_KEY_SPEC () =
        let input    = "a<>b"
        let expected =
            Result<Key array, string>.Error $"{INVALID_KEY_SPEC} at offset 1: 'a<>b'"
        let result   = parseKeysString input

        Assert.AreEqual (expected, result, input)

    [<Test>]
    member _.parseKeysString_INVALID_KEY_MODIFIERS () =
        let input    = "a<cx-a>b"
        let expected =
            Result<Key array, string>.Error $"{INVALID_KEY_MODIFIERS}: '<cx-a>'"
        let result   = parseKeysString input

        Assert.AreEqual (expected, result, input)

    [<Test>]
    member _.parseKeysString_INVALID_KEY_NAME () =
        let input    = "a<c-ax>b"
        let expected =
            Result<Key array, string>.Error $"{INVALID_KEY_NAME}: '<c-ax>'"
        let result   = parseKeysString input

        Assert.AreEqual (expected, result, input)

    [<Test>]
    member _.parseKeysString_Lowercase () =
        let inputsAndExpected = [
            "a"       , NoModif    InputKey.A
            "<a>"     , NoModif    InputKey.A
            "<c-a>"   , Ctrl       InputKey.A
            "<a-a>"   , Alt        InputKey.A
            "<ca-a>"  , invalidKey
            "<s-a>"   , Shift      InputKey.A
            "<sc-a>"  , ShiftCtrl  InputKey.A
            "<sa-a>"  , ShiftAlt   InputKey.A
            "<sca-a>" , invalidKey
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keysStringToKey input, input)

    [<Test>]
    member _.parseKeysString_Uppercase () =
        let inputsAndExpected = [
            "A"       , Shift      InputKey.A
            "<A>"     , Shift      InputKey.A
            "<c-A>"   , ShiftCtrl  InputKey.A
            "<a-A>"   , ShiftAlt   InputKey.A
            "<ca-A>"  , invalidKey
            "<s-A>"   , invalidKey
            "<sc-A>"  , invalidKey
            "<sa-A>"  , invalidKey
            "<sca-A>" , invalidKey
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keysStringToKey input, input)

    [<Test>]
    member _.parseKeysString_Digit () =
        let inputsAndExpected = [
            "0"       , NoModif    InputKey.D0
            "<0>"     , NoModif    InputKey.D0
            "<c-0>"   , Ctrl       InputKey.D0
            "<a-0>"   , Alt        InputKey.D0
            "<ca-0>"  , invalidKey
            "<s-0>"   , invalidKey
            "<sc-0>"  , ShiftCtrl  InputKey.D0
            "<sa-0>"  , ShiftAlt   InputKey.D0
            "<sca-0>" , invalidKey
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keysStringToKey input, input)

    [<Test>]
    member _.parseKeysString_Special () =
        let inputsAndExpected = [
            "<bsp>"     , NoModif      InputKey.Backspace
            "<c-bsp>"   , Ctrl         InputKey.Backspace
            "<a-bsp>"   , Alt          InputKey.Backspace
            "<ca-bsp>"  , CtrlAlt      InputKey.Backspace
            "<s-bsp>"   , Shift        InputKey.Backspace
            "<sc-bsp>"  , ShiftCtrl    InputKey.Backspace
            "<sa-bsp>"  , ShiftAlt     InputKey.Backspace
            "<sca-bsp>" , ShiftCtrlAlt InputKey.Backspace
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keysStringToKey input, input)

    [<Test>]
    member _.parseKeysString_Symbol_KeyChar () =
        let inputsAndExpected = [
            "!"       , NoModif    InputKey.Exclamation
            "<!>"     , NoModif    InputKey.Exclamation
            "<c-!>"   , Ctrl       InputKey.Exclamation
            "<a-!>"   , Alt        InputKey.Exclamation
            "<ca-!>"  , invalidKey
            "<s-!>"   , invalidKey
            "<sc-!>"  , invalidKey
            "<sa-!>"  , invalidKey
            "<sca-!>" , invalidKey
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keysStringToKey input, input)

    [<Test>]
    member _.parseKeysString_Symbol_KeyName () =
        let inputsAndExpected = [
            "<lt>"     , NoModif    InputKey.LessThan
            "<gt>"     , NoModif    InputKey.GreaterThan
            "<dq>"     , NoModif    InputKey.Quotation

            "<c-lt>"   , Ctrl       InputKey.LessThan
            "<a-lt>"   , Alt        InputKey.LessThan
            "<ca-lt>"  , invalidKey
            "<s-lt>"   , invalidKey
            "<sc-lt>"  , invalidKey
            "<sa-lt>"  , invalidKey
            "<sca-lt>" , invalidKey
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keysStringToKey input, input)

    [<Test>]
    member _.parseKeysString_Char () =
        let inputsAndExpected = [
            "á"       , CharNoModif 'á'
            "<á>"     , CharNoModif 'á'
            "<c-á>"   , invalidKey
            "<a-á>"   , invalidKey
            "<ca-á>"  , invalidKey
            "<s-á>"   , invalidKey
            "<sc-á>"  , invalidKey
            "<sa-á>"  , invalidKey
            "<sca-á>" , invalidKey
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keysStringToKey input, input)

    // parseKeyPrefixAndKeyString ----------------------------------------------

    [<Test>]
    member _.parseKeyPrefixAndKeyString_OkOneKey () =
        let input    = "a"
        let expected =
            Result<Key option * Key, string>.Ok (None, NoModif InputKey.A)
        let result   = parseKeyPrefixAndKeyString input

        Assert.AreEqual (expected, result, input)

    [<Test>]
    member _.parseKeyPrefixAndKeyString_OkTwoKeys () =
        let input    = "<c-b>a"
        let expected =
            Result<Key option * Key, string>.Ok (Some (Ctrl InputKey.B), NoModif InputKey.A)
        let result   = parseKeyPrefixAndKeyString input

        Assert.AreEqual (expected, result, input)

    [<Test>]
    member _.parseKeyPrefixAndKeyString_MUST_BE_ONE_OR_TWO_KEY_SPECS () =
        let input    = "abc"
        let expected =
            Result<Key option * Key, string>.Error $"{MUST_BE_ONE_OR_TWO_KEY_SPECS}: 'abc'"
        let result   = parseKeyPrefixAndKeyString input

        Assert.AreEqual (expected, result, input)

    [<Test>]
    member _.parseKeyPrefixAndKeyString_NO_KEY_AFTER_KEY_PREFIX () =
        let input    = "<c-b>"
        let expected =
            Result<Key option * Key, string>.Error $"{NO_KEY_AFTER_KEY_PREFIX}: '<c-b>'"
        let result   = parseKeyPrefixAndKeyString input

        Assert.AreEqual (expected, result, input)

    [<Test>]
    member _.parseKeyPrefixAndKeyString_INVALID_KEY_PREFIX () =
        let input    = "ab"
        let expected =
            Result<Key option * Key, string>.Error $"{INVALID_KEY_PREFIX}: 'ab'"
        let result   = parseKeyPrefixAndKeyString input

        Assert.AreEqual (expected, result, input)

    // keyToString -------------------------------------------------------------

    [<Test>]
    member _.keyToKeyString_Lowercase () =
        let inputsAndExpected = [
            NoModif   InputKey.A , "a"
            Ctrl      InputKey.A , "<c-a>"
            Alt       InputKey.A , "<a-a>"
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyToKeyString input, expected)

    [<Test>]
    member _.keyToString_Uppercase () =
        let inputsAndExpected = [
            Shift     InputKey.A , "A"
            ShiftCtrl InputKey.A , "<c-A>"
            ShiftAlt  InputKey.A , "<a-A>"
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyToKeyString input, expected)

    [<Test>]
    member _.keyToString_Digit () =
        let inputsAndExpected = [
            NoModif   InputKey.D0 , "0"
            Ctrl      InputKey.D0 , "<c-0>"
            Alt       InputKey.D0 , "<a-0>"
            ShiftCtrl InputKey.D0 , "<sc-0>"
            ShiftAlt  InputKey.D0 , "<sa-0>"
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyToKeyString input, expected)

    [<Test>]
    member _.keyToString_Special () =
        let inputsAndExpected = [
            NoModif      InputKey.Backspace , "<bsp>"
            Ctrl         InputKey.Backspace , "<c-bsp>"
            Alt          InputKey.Backspace , "<a-bsp>"
            CtrlAlt      InputKey.Backspace , "<ca-bsp>"
            Shift        InputKey.Backspace , "<s-bsp>"
            ShiftCtrl    InputKey.Backspace , "<sc-bsp>"
            ShiftAlt     InputKey.Backspace , "<sa-bsp>"
            ShiftCtrlAlt InputKey.Backspace , "<sca-bsp>"
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyToKeyString input, expected)

    [<Test>]
    member _.keyToString_Symbol_KeyChar () =
        let inputsAndExpected = [
            NoModif InputKey.Exclamation , "!"
            Ctrl    InputKey.Exclamation , "<c-!>"
            Alt     InputKey.Exclamation , "<a-!>"
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyToKeyString input, expected)

    [<Test>]
    member _.keyToString_Symbol_KeyName () =
        let inputsAndExpected = [
            NoModif InputKey.LessThan    , "<lt>"
            NoModif InputKey.GreaterThan , "<gt>"
            NoModif InputKey.Quotation   , "<dq>"

            Ctrl    InputKey.LessThan    , "<c-lt>"
            Alt     InputKey.LessThan    , "<a-lt>"
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyToKeyString input, expected)

    [<Test>]
    member _.keyToString_Char () =
        let inputsAndExpected = [
            CharNoModif 'á' , "á"
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyToKeyString input, expected)
