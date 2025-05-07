module SettingsTest

open NUnit.Framework

open Settings

[<TestFixture>]
type SettingsTest () =
    let makeSettings () =
        makeBufferSettings (makeGlobalSettings ())

    [<Test>]
    member _.setValue_Int_Valid_1 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue b Scope.  buffer   Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let result = setValue b Scope.  buffer   Name.tabStop "2"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let result = setValue b Scope.``global`` Name.tabStop "3"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 3, getValue b Name.tabStop)

        let result = setValue b Scope.``global`` Name.tabStop "5"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 5, getValue b Name.tabStop)

    [<Test>]
    member _.setValue_Int_Valid_2 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue b Scope.``global`` Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let result = setValue b Scope.``global`` Name.tabStop "2"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let result = setValue b Scope.  buffer   Name.tabStop "3"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 3, getValue b Name.tabStop)

        let result = setValue b Scope.  buffer   Name.tabStop "5"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 5, getValue b Name.tabStop)

    [<Test>]
    member _.unset_Int_1 () =
        let b = makeSettings ()

        let _result = setValue   b Scope.``global`` Name.tabStop "1"
        let _result = setValue   b Scope.  buffer   Name.tabStop "2"

        let  result = unsetValue b Scope.  buffer   Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = unsetValue b Scope.  buffer   Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = unsetValue b Scope.``global`` Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = unsetValue b Scope.``global`` Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

    [<Test>]
    member _.unset_Int_2 () =
        let b = makeSettings ()

        let _result = setValue   b Scope.``global`` Name.tabStop "1"
        let _result = setValue   b Scope.  buffer   Name.tabStop "2"

        let  result = unsetValue b Scope.``global`` Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = unsetValue b Scope.``global`` Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = unsetValue b Scope.  buffer   Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = unsetValue b Scope.  buffer   Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

    [<Test>]
    member _.setValue_Int_IsFixed_1 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let _result = setValue        b Scope.``global`` Name.tabStop "1"
        let  result = setValueAsFixed b Scope.  buffer   Name.tabStop "2"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let  result = setValue        b Scope.  buffer   Name.tabStop "3"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let  result = setValue        b Scope.``global`` Name.tabStop "3"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

    [<Test>]
    member _.setValue_Int_IsFixed_2 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = setValueAsFixed b Scope.``global`` Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = setValue        b Scope.  buffer   Name.tabStop "2"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = setValue        b Scope.``global`` Name.tabStop "2"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

    [<Test>]
    member _.unsetValue_Int_IsFixed_1 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let _result = setValue        b Scope.``global`` Name.tabStop "1"
        let  result = setValueAsFixed b Scope.  buffer   Name.tabStop "2"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let  result = unsetValue      b Scope.  buffer   Name.tabStop

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let  result = unsetValue      b Scope.``global`` Name.tabStop

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

    [<Test>]
    member _.unsetValue_Int_IsFixed_2 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = setValueAsFixed b Scope.``global`` Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = unsetValue      b Scope.  buffer   Name.tabStop

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = unsetValue      b Scope.``global`` Name.tabStop

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

    [<Test>]
    member _.setValue_Int_Invalid () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue b Scope.``global`` Name.tabStop "0"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

    [<Test>]
    member _.setValue_Bool_Valid () =
        let b = makeSettings ()

        Assert.AreEqual (Bool false, getValue b Name.wrapLines)

        let result = setValue b Scope.``global`` Name.wrapLines "true"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Bool true, getValue b Name.wrapLines)

    [<Test>]
    member _.setValue_Bool_InValid () =
        let b = makeSettings ()

        Assert.AreEqual (Bool false, getValue b Name.wrapLines)

        let result = setValue b Scope.``global`` Name.wrapLines "_true_"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Bool false, getValue b Name.wrapLines)

    [<Test>]
    member _.setValue_String_Valid () =
        let b = makeSettings ()

        Assert.AreEqual (String "dark", getValue b Name.colorScheme)

        let result = setValue b Scope.``global`` Name.colorScheme "light"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (String "light", getValue b Name.colorScheme)

    [<Test>]
    member _.setValue_String_Invalid () =
        let b = makeSettings ()

        Assert.AreEqual (String "dark", getValue b Name.colorScheme)

        let result = setValue b Scope.``global`` Name.colorScheme "_light_"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (String "dark", getValue b Name.colorScheme)

    [<Test>]
    member _.getSettingRepr_global_buffer () =
        let b = makeSettings ()

        let _result = setValue b Scope.``global`` Name.tabStop "1"
        let _result = setValue b Scope.  buffer   Name.tabStop "2"

        let result = getSettingRepr b Name.tabStop

        Assert.AreEqual ("tabStop: default '4', global '1', buffer '2'", result)

    [<Test>]
    member _.getSettingRepr_global () =
        let b = makeSettings ()

        let _result = setValue b Scope.``global`` Name.tabStop "1"

        let result = getSettingRepr b Name.tabStop

        Assert.AreEqual ("tabStop: default '4', global '1'", result)

    [<Test>]
    member _.getSettingRepr_buffer () =
        let b = makeSettings ()

        let _result = setValue b Scope.  buffer   Name.tabStop "2"

        let result = getSettingRepr b Name.tabStop

        Assert.AreEqual ("tabStop: default '4', buffer '2'", result)
