module ConsoleKeysTest

open NUnit.Framework
open System

open ConsoleKeys

[<TestFixture>]
type ConsoleKeysTest () =
    let keyInfoToString (input: ConsoleKeyInfo) =
        $"{input.KeyChar} {input.Key} {input.Modifiers}"

    [<Test>]
    member _.keyInfoToKey_Special () =
        let inputsAndExpected = [
//                          keyChar key                   shift  alt    control
            ConsoleKeyInfo ('x',    ConsoleKey.Backspace, false, false, false), NoModif      InputKey.Backspace
            ConsoleKeyInfo ('x',    ConsoleKey.Backspace, false, false, true ), Ctrl         InputKey.Backspace
            ConsoleKeyInfo ('x',    ConsoleKey.Backspace, false, true , false), Alt          InputKey.Backspace
            ConsoleKeyInfo ('x',    ConsoleKey.Backspace, false, true , true ), CtrlAlt      InputKey.Backspace
            ConsoleKeyInfo ('x',    ConsoleKey.Backspace, true , false, false), Shift        InputKey.Backspace
            ConsoleKeyInfo ('x',    ConsoleKey.Backspace, true , false, true ), ShiftCtrl    InputKey.Backspace
            ConsoleKeyInfo ('x',    ConsoleKey.Backspace, true , true , false), ShiftAlt     InputKey.Backspace
            ConsoleKeyInfo ('x',    ConsoleKey.Backspace, true , true , true ), ShiftCtrlAlt InputKey.Backspace
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyInfoToKey input false, keyInfoToString input)

    [<Test>]
    member _.keyInfoToKey_Letter_WithoutCapsLock () =
        let inputsAndExpected = [
//                          keyChar key            shift  alt    control
            ConsoleKeyInfo ('x',    ConsoleKey.A , false, false, true ), Ctrl      InputKey.A
            ConsoleKeyInfo ('x',    ConsoleKey.A , false, true , false), Alt       InputKey.A
            ConsoleKeyInfo ('x',    ConsoleKey.A , true , false, true ), ShiftCtrl InputKey.A
            ConsoleKeyInfo ('x',    ConsoleKey.A , true , true , false), ShiftAlt  InputKey.A

            ConsoleKeyInfo ('a',    ConsoleKey.X , false, false, false), NoModif   InputKey.A
            ConsoleKeyInfo ('a',    ConsoleKey.X , true , false, false), NoModif   InputKey.A
            ConsoleKeyInfo ('A',    ConsoleKey.X , false, false, false), Shift     InputKey.A
            ConsoleKeyInfo ('A',    ConsoleKey.X , true , false, false), Shift     InputKey.A
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyInfoToKey input false, keyInfoToString input)

    [<Test>]
    member _.keyInfoToKey_Letter_WithCapsLock () =
        let inputsAndExpected = [
//                          keyChar key            shift  alt    control
            ConsoleKeyInfo ('x',    ConsoleKey.A , false, false, true ), ShiftCtrl InputKey.A
            ConsoleKeyInfo ('x',    ConsoleKey.A , false, true , false), ShiftAlt  InputKey.A
            ConsoleKeyInfo ('x',    ConsoleKey.A , true , false, true ), Ctrl      InputKey.A
            ConsoleKeyInfo ('x',    ConsoleKey.A , true , true , false), Alt       InputKey.A

            ConsoleKeyInfo ('a',    ConsoleKey.X , false, false, false), NoModif   InputKey.A
            ConsoleKeyInfo ('a',    ConsoleKey.X , true , false, false), NoModif   InputKey.A
            ConsoleKeyInfo ('A',    ConsoleKey.X , false, false, false), Shift     InputKey.A
            ConsoleKeyInfo ('A',    ConsoleKey.X , true , false, false), Shift     InputKey.A
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyInfoToKey input true, keyInfoToString input)

    [<Test>]
    member _.keyInfoToKey_Digit () =
        let inputsAndExpected = [
//                          keyChar key            shift  alt    control
            ConsoleKeyInfo ('x',    ConsoleKey.D0, false, false, true ), Ctrl      InputKey.D0
            ConsoleKeyInfo ('x',    ConsoleKey.D0, false, true , false), Alt       InputKey.D0
            ConsoleKeyInfo ('x',    ConsoleKey.D0, true , false, true ), ShiftCtrl InputKey.D0
            ConsoleKeyInfo ('x',    ConsoleKey.D0, true , true , false), ShiftAlt  InputKey.D0

            ConsoleKeyInfo ('0',    ConsoleKey.X , false, false, false), NoModif   InputKey.D0
            ConsoleKeyInfo ('0',    ConsoleKey.X , true , false, false), NoModif   InputKey.D0
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyInfoToKey input false, keyInfoToString input)

    [<Test>]
    member _.keyInfoToKey_Symbol () =
        let inputsAndExpected = [
//                          keyChar key            shift  alt    control
            ConsoleKeyInfo ('!',    ConsoleKey.D0, false, false, false), NoModif   InputKey.Exclamation
            ConsoleKeyInfo ('!',    ConsoleKey.D0, false, false, true ), Ctrl      InputKey.Exclamation
            ConsoleKeyInfo ('!',    ConsoleKey.D0, false, true , false), Alt       InputKey.Exclamation
            ConsoleKeyInfo ('!',    ConsoleKey.D0, false, true , true ), NoModif   InputKey.Exclamation
            ConsoleKeyInfo ('!',    ConsoleKey.D0, true , false, false), NoModif   InputKey.Exclamation
            ConsoleKeyInfo ('!',    ConsoleKey.D0, true , false, true ), Ctrl      InputKey.Exclamation
            ConsoleKeyInfo ('!',    ConsoleKey.D0, true , true , false), Alt       InputKey.Exclamation
            ConsoleKeyInfo ('!',    ConsoleKey.D0, true , true , true ), NoModif   InputKey.Exclamation
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyInfoToKey input false, keyInfoToString input)

    [<Test>]
    member _.keyInfoToKey_Char () =
        let inputsAndExpected = [
//                          keyChar key            shift  alt    control
            ConsoleKeyInfo ('á',    ConsoleKey.X , false, false, false), CharNoModif 'á'
            ConsoleKeyInfo ('á',    ConsoleKey.X , false, true , true ), CharNoModif 'á'
            ConsoleKeyInfo ('á',    ConsoleKey.X , true , false, false), CharNoModif 'á'
            ConsoleKeyInfo ('á',    ConsoleKey.X , true , true , true ), CharNoModif 'á'
        ]

        for input, expected in inputsAndExpected do
            Assert.AreEqual (expected, keyInfoToKey input false, keyInfoToString input)
