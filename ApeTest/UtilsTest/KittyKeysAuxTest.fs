module KittyKeysAuxTest

open NUnit.Framework

open KittyKeysAux

[<TestFixture>]
type ParseEscapedTest () =

    [<Test>]
    member _.parseEscaped_WithKeyCodesOnly () =
        let inputsAndExpected = [
            "u"        , { Escaped_zero with unicode = Some '\x01';                                        endChar = 'u' }

            "97u"      , { Escaped_zero with unicode = Some 'a'   ;                                        endChar = 'u' }
            "97:98u"   , { Escaped_zero with unicode = Some 'a'   ; shifted = Some 'b';                    endChar = 'u' }
            "97:98:99u", { Escaped_zero with unicode = Some 'a'   ; shifted = Some 'b'; layout = Some 'c'; endChar = 'u' }
                                                                  
            ":98:99u"  , { Escaped_zero with unicode = None       ; shifted = Some 'b'; layout = Some 'c'; endChar = 'u' }
            "97::99u"  , { Escaped_zero with unicode = Some 'a'   ; shifted = None    ; layout = Some 'c'; endChar = 'u' }
            "97:98:u"  , { Escaped_zero with unicode = Some 'a'   ; shifted = Some 'b'; layout = None    ; endChar = 'u' }
                                                                  
            "97~"      , { Escaped_zero with unicode = Some 'a'   ;                                        endChar = '~' }
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, parseEscaped input, input)

    [<Test>]
    member _.parseEscaped_WithModifiers () =
        let inputsAndExpected = [
            ";u"     , { Escaped_zero with unicode = Some '\x01'; modifiers = KittyModifiers.None                       ; endChar = 'u' }

            "97;1u"  , { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.None                       ; endChar = 'u' }
            "97;2u"  , { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Shift                      ; endChar = 'u' }
            "97;3u"  , { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Alt                        ; endChar = 'u' }
            "97;5u"  , { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Ctrl                       ; endChar = 'u' }
            "97;9u"  , { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Super                      ; endChar = 'u' }
            "97;17u" , { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Hyper                      ; endChar = 'u' }
            "97;33u" , { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Meta                       ; endChar = 'u' }
            "97;65u" , { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Caps_lock                  ; endChar = 'u' }
            "97;129u", { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Num_lock                   ; endChar = 'u' }
            "97;7u"  , { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Alt ||| KittyModifiers.Ctrl; endChar = 'u' }
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, parseEscaped input, input)

    [<Test>]
    member _.parseEscaped_WithModifiersAndText () =
        let inputsAndExpected = [
            "97;2;u"   , { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Shift; text = None    ; endChar = 'u' }
                                                                  
            "97;2;100u", { Escaped_zero with unicode = Some 'a'   ; modifiers = KittyModifiers.Shift; text = Some 'd'; endChar = 'u' }
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, parseEscaped input, input)
