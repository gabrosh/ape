module KeyMappingsTest

open NUnit.Framework

open ConsoleKeys
open KeyMappings

[<TestFixture>]
type KeyMappingsTest () =
    let makeKeyMappings () =
        makeBufferKeyMappings (makeGlobalKeyMappings ())

    let keyCtrlA = (Ctrl InputKey.A)

    let seqA = [| NoModif InputKey.A |]
    let seqB = [| NoModif InputKey.B |]
    let seqC = [| NoModif InputKey.C |]

    [<Test>]
    member _.mapKey () =
        let b = makeKeyMappings ()

        Assert.AreEqual (None     , getKeySequence b (Mode.normal, None, keyCtrlA))

        mapKey b Scope.``global`` (Mode.normal, None, keyCtrlA) seqA

        Assert.AreEqual (Some seqA, getKeySequence b (Mode.normal, None, keyCtrlA))

        mapKey b Scope.  buffer   (Mode.normal, None, keyCtrlA) seqB

        Assert.AreEqual (Some seqB, getKeySequence b (Mode.normal, None, keyCtrlA))

        mapKey b Scope.``global`` (Mode.normal, None, keyCtrlA) seqC

        Assert.AreEqual (Some seqC, getKeySequence b (Mode.normal, None, keyCtrlA))

    [<Test>]
    member _.unmapKey () =
        let b = makeKeyMappings ()

        mapKey   b Scope.``global`` (Mode.normal, None, keyCtrlA) seqA
        mapKey   b Scope.  buffer   (Mode.normal, None, keyCtrlA) seqB

        unmapKey b Scope.  buffer   (Mode.normal, None, keyCtrlA)

        Assert.AreEqual (Some seqA, getKeySequence b (Mode.normal, None, keyCtrlA))

        unmapKey b Scope.``global`` (Mode.normal, None, keyCtrlA)

        Assert.AreEqual (None     , getKeySequence b (Mode.normal, None, keyCtrlA))

        mapKey   b Scope.``global`` (Mode.normal, None, keyCtrlA) seqA
        mapKey   b Scope.  buffer   (Mode.normal, None, keyCtrlA) seqB

        unmapKey b Scope.``global`` (Mode.normal, None, keyCtrlA)

        Assert.AreEqual (None     , getKeySequence b (Mode.normal, None, keyCtrlA))

        unmapKey b Scope.  buffer   (Mode.normal, None, keyCtrlA)

        Assert.AreEqual (None     , getKeySequence b (Mode.normal, None, keyCtrlA))
