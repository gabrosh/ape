module SettingsTest

open NUnit.Framework

open Settings

[<TestFixture>]
type SettingsTest () =
    let makeSettings () =
        makeBufferSettings (makeGlobalSettings ())

    let makeExtractSettings () =
        makeBufferExtractSettings (
            makeBufferSettings (makeGlobalSettings ())
        )

    // setValue - Valid --------------------------------------------------------

    [<Test>]
    member _.setValue_Int_Valid_1 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue b (Some Scope.  buffer  ) Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let result = setValue b (Some Scope.  buffer  ) Name.tabStop "2"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let result = setValue b (Some Scope.``global``) Name.tabStop "3"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 3, getValue b Name.tabStop)

        let result = setValue b (Some Scope.``global``) Name.tabStop "5"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 5, getValue b Name.tabStop)

    [<Test>]
    member _.setValue_Int_Valid_2 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue b (Some Scope.``global``) Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let result = setValue b (Some Scope.``global``) Name.tabStop "2"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let result = setValue b (Some Scope.  buffer  ) Name.tabStop "3"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 3, getValue b Name.tabStop)

        let result = setValue b (Some Scope.  buffer  ) Name.tabStop "5"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 5, getValue b Name.tabStop)

    // unsetValue --------------------------------------------------------------

    [<Test>]
    member _.unsetValue_Int_1 () =
        let b = makeSettings ()

        let _result = setValue   b (Some Scope.``global``) Name.tabStop "1"
        let _result = setValue   b (Some Scope.  buffer  ) Name.tabStop "2"

        let  result = unsetValue b (Some Scope.  buffer  ) Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = unsetValue b (Some Scope.  buffer  ) Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = unsetValue b (Some Scope.``global``) Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = unsetValue b (Some Scope.``global``) Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

    [<Test>]
    member _.unsetValue_Int_2 () =
        let b = makeSettings ()

        let _result = setValue   b (Some Scope.``global``) Name.tabStop "1"
        let _result = setValue   b (Some Scope.  buffer  ) Name.tabStop "2"

        let  result = unsetValue b (Some Scope.``global``) Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = unsetValue b (Some Scope.``global``) Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = unsetValue b (Some Scope.  buffer  ) Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = unsetValue b (Some Scope.  buffer  ) Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

    // setValue, unsetValue - default scope ------------------------------------

    [<Test>]
    member _.setValue_Int_defaultScope_buffer () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue   b None                    Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let result = unsetValue b (Some Scope.  buffer  ) Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

    [<Test>]
    member _.unsetValue_Int_defaultScope_buffer () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue   b (Some Scope.  buffer  ) Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let result = unsetValue b None                    Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

    [<Test>]
    member _.setValue_Int_defaultScope_extract () =
        let b = makeExtractSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue   b None                    Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let result = unsetValue b (Some Scope.  extract ) Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

    [<Test>]
    member _.unsetValue_Int_defaultScope_extract () =
        let b = makeExtractSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue   b (Some Scope.  extract ) Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let result = unsetValue b None                    Name.tabStop

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

    // setValue - AsFixed ------------------------------------------------------

    [<Test>]
    member _.setValue_Int_AsFixed_1 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let _result = setValue        b (Some Scope.``global``) Name.tabStop "1"
        let  result = setValueAsFixed b (Some Scope.  buffer  ) Name.tabStop "2"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let  result = setValue        b (Some Scope.  buffer  ) Name.tabStop "3"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let  result = setValue        b (Some Scope.``global``) Name.tabStop "3"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

    [<Test>]
    member _.setValue_Int_IsFixed_2 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = setValueAsFixed b (Some Scope.``global``) Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = setValue        b (Some Scope.  buffer  ) Name.tabStop "2"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = setValue        b (Some Scope.``global``) Name.tabStop "2"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

    // unsetValue - AsFixed ----------------------------------------------------

    [<Test>]
    member _.unsetValue_Int_AsFixed_1 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let _result = setValue        b (Some Scope.``global``) Name.tabStop "1"
        let  result = setValueAsFixed b (Some Scope.  buffer  ) Name.tabStop "2"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let  result = unsetValue      b (Some Scope.  buffer  ) Name.tabStop

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

        let  result = unsetValue      b (Some Scope.``global``) Name.tabStop

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

    [<Test>]
    member _.unsetValue_Int_AsFixed_2 () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let  result = setValueAsFixed b (Some Scope.``global``) Name.tabStop "1"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = unsetValue      b (Some Scope.  buffer  ) Name.tabStop

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

        let  result = unsetValue      b (Some Scope.``global``) Name.tabStop

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 1, getValue b Name.tabStop)

    // setValue - Valid/Invalid ------------------------------------------------

    [<Test>]
    member _.setValue_Int_Valid () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue b (Some Scope.``global``) Name.tabStop "2"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Int 2, getValue b Name.tabStop)

    [<Test>]
    member _.setValue_Int_Invalid () =
        let b = makeSettings ()

        Assert.AreEqual (Int 4, getValue b Name.tabStop)

        let result = setValue b (Some Scope.``global``) Name.tabStop "0"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Int 4, getValue b Name.tabStop)

    [<Test>]
    member _.setValue_Bool_Valid () =
        let b = makeSettings ()

        Assert.AreEqual (Bool false, getValue b Name.wrapLines)

        let result = setValue b (Some Scope.``global``) Name.wrapLines "true"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Bool true, getValue b Name.wrapLines)

    [<Test>]
    member _.setValue_Bool_InValid () =
        let b = makeSettings ()

        Assert.AreEqual (Bool false, getValue b Name.wrapLines)

        let result = setValue b (Some Scope.``global``) Name.wrapLines "_true_"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (Bool false, getValue b Name.wrapLines)

    [<Test>]
    member _.setValue_String_Valid () =
        let b = makeSettings ()

        Assert.AreEqual (String "dark", getValue b Name.colorScheme)

        let result = setValue b (Some Scope.``global``) Name.colorScheme "light"

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (String "light", getValue b Name.colorScheme)

    [<Test>]
    member _.setValue_String_Invalid () =
        let b = makeSettings ()

        Assert.AreEqual (String "dark", getValue b Name.colorScheme)

        let result = setValue b (Some Scope.``global``) Name.colorScheme "_light_"

        Assert.IsFalse  (Result.isOk result)
        Assert.AreEqual (String "dark", getValue b Name.colorScheme)

    // getSettingRepr ----------------------------------------------------------

    [<Test>]
    member _.getSettingRepr_global_buffer () =
        let b = makeSettings ()

        let _result = setValue b (Some Scope.``global``) Name.tabStop "1"
        let _result = setValue b (Some Scope.  buffer  ) Name.tabStop "2"

        let result = getSettingRepr b Name.tabStop

        Assert.AreEqual ("tabStop: default '4', global '1', buffer '2'", result)

    [<Test>]
    member _.getSettingRepr_global () =
        let b = makeSettings ()

        let _result = setValue b (Some Scope.``global``) Name.tabStop "1"

        let result = getSettingRepr b Name.tabStop

        Assert.AreEqual ("tabStop: default '4', global '1'", result)

    [<Test>]
    member _.getSettingRepr_buffer () =
        let b = makeSettings ()

        let _result = setValue b (Some Scope.  buffer  ) Name.tabStop "2"

        let result = getSettingRepr b Name.tabStop

        Assert.AreEqual ("tabStop: default '4', buffer '2'", result)
