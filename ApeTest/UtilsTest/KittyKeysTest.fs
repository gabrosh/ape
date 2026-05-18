module KittyKeysTest

open NUnit.Framework
open System

open ConsoleKeys
open KittyKeysAux
open KittyKeys

let private modNoModif           = ConsoleModifiers.None
let private modCtrl              = ConsoleModifiers.Control
let private modAlt               = ConsoleModifiers.Alt
let private modCtrlAlt           = ConsoleModifiers.Control ||| ConsoleModifiers.Alt
let private modShift             = ConsoleModifiers.Shift
let private modShiftCtrl         = ConsoleModifiers.Shift   ||| modCtrl
let private modShiftAlt          = ConsoleModifiers.Shift   ||| modAlt
let private modShiftCtrlAlt      = ConsoleModifiers.Shift   ||| modCtrlAlt

let private kittyModNoModif      = KittyModifiers.None
let private kittyModCtrl         = KittyModifiers.Ctrl
let private kittyModAlt          = KittyModifiers.Alt
let private kittyModCtrlAlt      = KittyModifiers.Ctrl  ||| KittyModifiers.Alt
let private kittyModShift        = KittyModifiers.Shift
let private kittyModShiftCtrl    = KittyModifiers.Shift ||| kittyModCtrl
let private kittyModShiftAlt     = KittyModifiers.Shift ||| kittyModAlt
let private kittyModShiftCtrlAlt = KittyModifiers.Shift ||| kittyModCtrlAlt
let private kittyModCapsLock     = KittyModifiers.Caps_lock
let private kittyModNumLock      = KittyModifiers.Num_lock

[<TestFixture>]
type KittyKeysTest () =
    let escaped unicode modifs endChar =
        KittyEscaped { 
            Escaped_zero with
                unicode = Some unicode
                modifiers = modifs; endChar = endChar
        }

    let escapedS unicode shifted modifs endChar =
        KittyEscaped {
            Escaped_zero with
                unicode = Some unicode; shifted = Some shifted
                modifiers = modifs; endChar = endChar
        }

    let escapedT unicode text modifs endChar =
        KittyEscaped {
            Escaped_zero with
                unicode = Some unicode; text = Some text
                modifiers = modifs; endChar = endChar
        }

    // ConsoleKey --------------------------------------------------------------

    [<Test>]
    member _.kittyKeyToKey_SpecialConsoleKey () =
        let inputsAndExpected = [
            ConsoleKey (ConsoleKey.Enter, modNoModif     ), NoModif      InputKey.Enter
            ConsoleKey (ConsoleKey.Enter, modCtrl        ), Ctrl         InputKey.Enter
            ConsoleKey (ConsoleKey.Enter, modCtrlAlt     ), CtrlAlt      InputKey.Enter
            ConsoleKey (ConsoleKey.Enter, modShift       ), Shift        InputKey.Enter
            ConsoleKey (ConsoleKey.Enter, modShiftCtrl   ), ShiftCtrl    InputKey.Enter
            ConsoleKey (ConsoleKey.Enter, modShiftCtrlAlt), ShiftCtrlAlt InputKey.Enter
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    // KittyChar ---------------------------------------------------------------

    [<Test>]
    member _.kittyKeyToKey_SpecialChar () =
        let inputsAndExpected = [
            KittyChar '\x0D', NoModif InputKey.Enter
            KittyChar '\x09', NoModif InputKey.Tab
            KittyChar '\x08', NoModif InputKey.Backspace
            KittyChar '\x7F', NoModif InputKey.Backspace
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    [<Test>]
    member _.kittyKeyToKey_LetterChar () =
        let inputsAndExpected = [
            KittyChar 'a', NoModif InputKey.A
            KittyChar 'z', NoModif InputKey.Z
            KittyChar 'A', Shift   InputKey.A
            KittyChar 'Z', Shift   InputKey.Z
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    [<Test>]
    member _.kittyKeyToKey_SymbolChar () =
        let inputsAndExpected = [
            KittyChar '&', NoModif InputKey.Ampersand
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    [<Test>]
    member _.kittyKeyToKey_DigitChar () =
        let inputsAndExpected = [
            KittyChar '0', NoModif InputKey.D0
            KittyChar '9', NoModif InputKey.D9
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    [<Test>]
    member _.kittyKeyToKey_OtherChar () =
        let inputsAndExpected = [
            KittyChar 'á', CharNoModif 'á'
            KittyChar '€', CharNoModif '€'
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    // KittyEscaped - Text -----------------------------------------------------

    [<Test>]
    member _.kittyKeyToKey_TextEscaped () =
        let inputsAndExpected = [
            escapedT 'e' '€' kittyModNoModif 'u', CharNoModif '€'
            escapedT 'd' 'Đ' kittyModNoModif 'u', CharNoModif 'Đ'
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    // KittyEscaped - Special --------------------------------------------------

    [<Test>]
    member _.kittyKeyToKey_SpecialEscaped () =
        let inputsAndExpected = [
            escaped '\x1B' kittyModNoModif      'u', NoModif      InputKey.Escape
            escaped '\x1B' kittyModCtrl         'u', Ctrl         InputKey.Escape
            escaped '\x1B' kittyModAlt          'u', Alt          InputKey.Escape
            escaped '\x1B' kittyModCtrlAlt      'u', CtrlAlt      InputKey.Escape
            escaped '\x1B' kittyModShift        'u', Shift        InputKey.Escape
            escaped '\x1B' kittyModShiftCtrl    'u', ShiftCtrl    InputKey.Escape
            escaped '\x1B' kittyModShiftAlt     'u', ShiftAlt     InputKey.Escape
            escaped '\x1B' kittyModShiftCtrlAlt 'u', ShiftCtrlAlt InputKey.Escape

            escaped '\x1B' (kittyModCapsLock                     ||| kittyModCtrl     ) 'u', Ctrl      InputKey.Escape
            escaped '\x1B' (kittyModNumLock                      ||| kittyModCtrl     ) 'u', Ctrl      InputKey.Escape
            escaped '\x1B' (kittyModCapsLock ||| kittyModNumLock ||| kittyModCtrl     ) 'u', Ctrl      InputKey.Escape

            escaped '\x1B' (kittyModCapsLock                     ||| kittyModShiftCtrl) 'u', ShiftCtrl InputKey.Escape
            escaped '\x1B' (kittyModNumLock                      ||| kittyModShiftCtrl) 'u', ShiftCtrl InputKey.Escape
            escaped '\x1B' (kittyModCapsLock ||| kittyModNumLock ||| kittyModShiftCtrl) 'u', ShiftCtrl InputKey.Escape
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    [<Test>]
    member _.kittyKeyToKey_SpecialEscaped_KP () =
        let inputsAndExpected = [
            escaped (char 57417) kittyModNoModif      'u', NoModif      InputKey.LeftArrow
            escaped (char 57417) kittyModCtrl         'u', Ctrl         InputKey.LeftArrow
            escaped (char 57417) kittyModAlt          'u', Alt          InputKey.LeftArrow
            escaped (char 57417) kittyModCtrlAlt      'u', CtrlAlt      InputKey.LeftArrow
            escaped (char 57417) kittyModShift        'u', Shift        InputKey.LeftArrow
            escaped (char 57417) kittyModShiftCtrl    'u', ShiftCtrl    InputKey.LeftArrow
            escaped (char 57417) kittyModShiftAlt     'u', ShiftAlt     InputKey.LeftArrow
            escaped (char 57417) kittyModShiftCtrlAlt 'u', ShiftCtrlAlt InputKey.LeftArrow

            escaped (char 57417) (kittyModCapsLock                     ||| kittyModCtrl     ) 'u', Ctrl      InputKey.LeftArrow
            escaped (char 57417) (kittyModNumLock                      ||| kittyModCtrl     ) 'u', Ctrl      InputKey.LeftArrow
            escaped (char 57417) (kittyModCapsLock ||| kittyModNumLock ||| kittyModCtrl     ) 'u', Ctrl      InputKey.LeftArrow

            escaped (char 57417) (kittyModCapsLock                     ||| kittyModShiftCtrl) 'u', ShiftCtrl InputKey.LeftArrow
            escaped (char 57417) (kittyModNumLock                      ||| kittyModShiftCtrl) 'u', Ctrl      InputKey.LeftArrow
            escaped (char 57417) (kittyModCapsLock ||| kittyModNumLock ||| kittyModShiftCtrl) 'u', Ctrl      InputKey.LeftArrow
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    // KittyEscaped - Letter ---------------------------------------------------

    [<Test>]
    member _.kittyKeyToKey_LetterEscaped_WithoutCapsLock () =
        let inputsAndExpected = [
            escaped 'a' kittyModNoModif      'u', NoModif      InputKey.A
            escaped 'a' kittyModCtrl         'u', Ctrl         InputKey.A
            escaped 'a' kittyModAlt          'u', Alt          InputKey.A
            escaped 'a' kittyModCtrlAlt      'u', CtrlAlt      InputKey.A
            escaped 'a' kittyModShift        'u', Shift        InputKey.A
            escaped 'a' kittyModShiftCtrl    'u', ShiftCtrl    InputKey.A
            escaped 'a' kittyModShiftAlt     'u', ShiftAlt     InputKey.A
            escaped 'a' kittyModShiftCtrlAlt 'u', ShiftCtrlAlt InputKey.A

            escaped 'a' (kittyModNumLock ||| kittyModCtrl     ) 'u', Ctrl      InputKey.A

            escaped 'a' (kittyModNumLock ||| kittyModShiftCtrl) 'u', ShiftCtrl InputKey.A
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    [<Test>]
    member _.kittyKeyToKey_LetterEscaped_WithCapsLock () =
        let inputsAndExpected = [
            escaped 'a' (kittyModNoModif      ||| kittyModCapsLock) 'u', Shift        InputKey.A
            escaped 'a' (kittyModCtrl         ||| kittyModCapsLock) 'u', ShiftCtrl    InputKey.A
            escaped 'a' (kittyModAlt          ||| kittyModCapsLock) 'u', ShiftAlt     InputKey.A
            escaped 'a' (kittyModCtrlAlt      ||| kittyModCapsLock) 'u', ShiftCtrlAlt InputKey.A
            escaped 'a' (kittyModShift        ||| kittyModCapsLock) 'u', NoModif      InputKey.A
            escaped 'a' (kittyModShiftCtrl    ||| kittyModCapsLock) 'u', Ctrl         InputKey.A
            escaped 'a' (kittyModShiftAlt     ||| kittyModCapsLock) 'u', Alt          InputKey.A
            escaped 'a' (kittyModShiftCtrlAlt ||| kittyModCapsLock) 'u', CtrlAlt      InputKey.A

            escaped 'a' (kittyModNumLock ||| kittyModCtrl      ||| kittyModCapsLock) 'u', ShiftCtrl InputKey.A

            escaped 'a' (kittyModNumLock ||| kittyModShiftCtrl ||| kittyModCapsLock) 'u', Ctrl      InputKey.A
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    // KittyEscaped - Symbol ---------------------------------------------------

    [<Test>]
    member _.kittyKeyToKey_SymbolEscaped () =
        let inputsAndExpected = [
            escaped  '&'     kittyModNoModif      'u', NoModif InputKey.Ampersand
            escaped  '&'     kittyModCtrl         'u', Ctrl    InputKey.Ampersand
            escaped  '&'     kittyModAlt          'u', Alt     InputKey.Ampersand
            escaped  '&'     kittyModCtrlAlt      'u', CtrlAlt InputKey.Ampersand
            escapedS '_' '&' kittyModShift        'u', NoModif InputKey.Ampersand
            escapedS '_' '&' kittyModShiftCtrl    'u', Ctrl    InputKey.Ampersand
            escapedS '_' '&' kittyModShiftAlt     'u', Alt     InputKey.Ampersand
            escapedS '_' '&' kittyModShiftCtrlAlt 'u', CtrlAlt InputKey.Ampersand

            escaped  '&'     (kittyModCapsLock ||| kittyModNumLock ||| kittyModCtrl     ) 'u', Ctrl InputKey.Ampersand

            escapedS '_' '&' (kittyModCapsLock ||| kittyModNumLock ||| kittyModShiftCtrl) 'u', Ctrl InputKey.Ampersand
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")

    // KittyEscaped - Digit ----------------------------------------------------

    [<Test>]
    member _.kittyKeyToKey_DigitEscaped () =
        let inputsAndExpected = [
            escaped  '0'     kittyModNoModif      'u', NoModif InputKey.D0
            escaped  '0'     kittyModCtrl         'u', Ctrl    InputKey.D0
            escaped  '0'     kittyModAlt          'u', Alt     InputKey.D0
            escaped  '0'     kittyModCtrlAlt      'u', CtrlAlt InputKey.D0
            escapedS '_' '0' kittyModShift        'u', NoModif InputKey.D0
            escapedS '_' '0' kittyModShiftCtrl    'u', Ctrl    InputKey.D0
            escapedS '_' '0' kittyModShiftAlt     'u', Alt     InputKey.D0
            escapedS '_' '0' kittyModShiftCtrlAlt 'u', CtrlAlt InputKey.D0

            escaped  '0'     (kittyModCapsLock ||| kittyModNumLock ||| kittyModCtrl      ) 'u', Ctrl InputKey.D0

            escapedS '_' '0' (kittyModCapsLock ||| kittyModNumLock ||| kittyModShiftCtrl ) 'u', Ctrl InputKey.D0
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, kittyKeyToKey input, $"{input}")
