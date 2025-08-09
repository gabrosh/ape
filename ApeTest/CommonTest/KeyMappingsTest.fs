module KeyMappingsTest

open NUnit.Framework

open ConsoleKeys
open KeyMappings

[<TestFixture>]
type KeyMappingsTest () =
    let makeKeyMappings () =
        makeBufferKeyMappings (makeGlobalKeyMappings ())

    let makeExtractKeyMappings () =
        makeBufferExtractKeyMappings (
            makeBufferKeyMappings (makeGlobalKeyMappings ())
        )

    let keyCtrlA      = (Ctrl InputKey.A)
    let modeKeyTriple = (Mode.normal, None, keyCtrlA)

    let seqA = [| NoModif InputKey.A |]
    let seqB = [| NoModif InputKey.B |]
    let seqC = [| NoModif InputKey.C |]
    let seqD = [| NoModif InputKey.D |]

    // mapKey ------------------------------------------------------------------

    [<Test>]
    member _.mapKey_1 () =
        let b = makeKeyMappings ()

        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

        let result = mapKey b (Some Scope.  buffer  ) modeKeyTriple seqA

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqA, getKeySequence b modeKeyTriple)

        let result = mapKey b (Some Scope.  buffer  ) modeKeyTriple seqB

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqB, getKeySequence b modeKeyTriple)

        let result = mapKey b (Some Scope.``global``) modeKeyTriple seqC

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqC, getKeySequence b modeKeyTriple)

        let result = mapKey b (Some Scope.``global``) modeKeyTriple seqD

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqD, getKeySequence b modeKeyTriple)

    [<Test>]
    member _.mapKey_2 () =
        let b = makeKeyMappings ()

        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

        let result = mapKey b (Some Scope.``global``) modeKeyTriple seqA

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqA, getKeySequence b modeKeyTriple)

        let result = mapKey b (Some Scope.``global``) modeKeyTriple seqB

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqB, getKeySequence b modeKeyTriple)

        let result = mapKey b (Some Scope.  buffer  ) modeKeyTriple seqC

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqC, getKeySequence b modeKeyTriple)

        let result = mapKey b (Some Scope.  buffer ) modeKeyTriple seqD

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqD, getKeySequence b modeKeyTriple)

    // unmapKey ----------------------------------------------------------------

    [<Test>]
    member _.unmapKey_1 () =
        let b = makeKeyMappings ()

        let _result = mapKey   b (Some Scope.``global``) modeKeyTriple seqA
        let _result = mapKey   b (Some Scope.  buffer  ) modeKeyTriple seqB

        let  result = unmapKey b (Some Scope.  buffer  ) modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqA, getKeySequence b modeKeyTriple)

        let  result = unmapKey b (Some Scope.  buffer  ) modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqA, getKeySequence b modeKeyTriple)

        let  result = unmapKey b (Some Scope.``global``) modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

        let  result = unmapKey b (Some Scope.``global``) modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

    [<Test>]
    member _.unmapKey_2 () =
        let b = makeKeyMappings ()

        let _result = mapKey   b (Some Scope.``global``) modeKeyTriple seqA
        let _result = mapKey   b (Some Scope.  buffer  ) modeKeyTriple seqB

        let  result = unmapKey b (Some Scope.``global``) modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

        let  result = unmapKey b (Some Scope.``global``) modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

        let  result = unmapKey b (Some Scope.  buffer  ) modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

        let  result = unmapKey b (Some Scope.  buffer  ) modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

    // setValue, unsetValue - default scope ------------------------------------

    [<Test>]
    member _.mapKey_defaultScope_buffer () =
        let b = makeKeyMappings ()

        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

        let result = mapKey   b None                   modeKeyTriple seqA

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqA, getKeySequence b modeKeyTriple)

        let result = unmapKey b (Some Scope.  buffer  ) modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

    [<Test>]
    member _.unmapKey_defaultScope_buffer () =
        let b = makeKeyMappings ()

        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

        let result = mapKey   b (Some Scope.  buffer  ) modeKeyTriple seqA

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqA, getKeySequence b modeKeyTriple)

        let result = unmapKey b None                    modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

    [<Test>]
    member _.mapKey_defaultScope_extract () =
        let b = makeExtractKeyMappings ()

        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

        let result = mapKey   b None                    modeKeyTriple seqA

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqA, getKeySequence b modeKeyTriple)

        let result = unmapKey b (Some Scope.  extract ) modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

    [<Test>]
    member _.unmaptKey_defaultScope_extract () =
        let b = makeExtractKeyMappings ()

        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)

        let result = mapKey   b (Some Scope.  extract ) modeKeyTriple seqA

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (Some seqA, getKeySequence b modeKeyTriple)

        let result = unmapKey b None                    modeKeyTriple

        Assert.IsTrue   (Result.isOk result)
        Assert.AreEqual (None     , getKeySequence b modeKeyTriple)
