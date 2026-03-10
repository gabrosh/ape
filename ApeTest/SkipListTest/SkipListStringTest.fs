module SkipListStringTest

open NUnit.Framework
open System
open System.Text

open SkipListString

let private getTestSkipListString () =
    SkipListString (10, 3)

[<TestFixture>]
type SkipListStringTests () =

    [<Test>]
    member _.SkipListString_Add () =
        let sl = getTestSkipListString ()

        sl.Add 'a'
        sl.Add 'b'
        sl.Add 'b'
        sl.Add 'c'
        sl.Add 'c'
        sl.Add 'c'

        Assert.That (sl.AsString (), Is.EqualTo "abbccc")

    [<Test>]
    member _.SkipListString_AddString () =
        let sl = getTestSkipListString ()

        sl.AddString "a"
        sl.AddString "bb"
        sl.AddString "ccc"

        Assert.That (sl.AsString (), Is.EqualTo "abbccc")

    [<Test>]
    member _.SkipListString_Insert () =
        let sl = getTestSkipListString ()

        sl.Insert 0 'a'
        sl.Insert 1 'b'
        sl.Insert 2 'b'
        sl.Insert 2 'c'
        sl.Insert 3 'c'
        sl.Insert 4 'c'

        Assert.That (sl.AsString (), Is.EqualTo "abcccb")

    [<Test>]
    member _.SkipListString_InsertString () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"

        Assert.That (sl.AsString (), Is.EqualTo "abcccb")

    [<Test>]
    member _.SkipListString_RemoveRange_FirstNode () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"
        sl.RemoveRange 0 3

        Assert.That (sl.AsString (), Is.EqualTo "ccb")

    [<Test>]
    member _.SkipListString_RemoveRange_NonFirstNodes () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"
        sl.RemoveRange 2 3

        Assert.That (sl.AsString (), Is.EqualTo "abb")

    [<Test>]
    member _.SkipListString_Get () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"

        Assert.That (string (sl.Get 0), Is.EqualTo "a")
        Assert.That (string (sl.Get 5), Is.EqualTo "b")

    [<Test>]
    member _.SkipListString_GetRangeSeq () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"

        Assert.That (String.Concat (sl.GetRangeSeq 0 6), Is.EqualTo "abcccb")
        Assert.That (String.Concat (sl.GetRangeSeq 1 4), Is.EqualTo "bccc")

    [<Test>]
    member _.SkipListString_Set () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"

        sl.Set 0 'x'
        sl.Set 5 'y'

        Assert.That (string (sl.Get 0), Is.EqualTo "x")
        Assert.That (string (sl.Get 5), Is.EqualTo "y")

    [<Test>]
    member _.SkipListString_Combined () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"
        sl.InsertString 6 "dd"
        sl.InsertString 8 "e"
        sl.InsertString 7 "fff"
        sl.InsertString 6 "zzzzzz"

        Assert.That (String.Concat (sl.GetRangeSeq 0 18), Is.EqualTo "abcccbzzzzzzdfffde")

        sl.RemoveRange 2  3
        sl.RemoveRange 10 3

        Assert.That (String.Concat (sl.GetRangeSeq 0 12), Is.EqualTo "abbzzzzzzdde")

    [<Test>]
    member _.SkipListString_Clear () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"

        Assert.That (sl.AsString (), Is.EqualTo "abcccb")

        sl.Clear ()

        Assert.That (sl.AsString (), Is.EqualTo "")

    [<Test>]
    member _.SkipListString_GetUndoNodes () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"

        let nodes = sl.GetUndoNodes ()

        Assert.That (nodes.Count, Is.EqualTo 3)

    [<Test>]
    member _.SkipListString_SetFromUndoNodes () =
        let sl  = getTestSkipListString ()
        let sl' = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"

        let nodes = sl.GetUndoNodes ()

        Assert.That (nodes.Count, Is.EqualTo 3)

        sl'.SetFromUndoNodes nodes

        Assert.That (sl'.AsString (), Is.EqualTo "abcccb")

    [<Test>]
    member _.SkipListString_IEnumerableOfObj () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"
        sl.InsertString 6 "dd"
        sl.InsertString 8 "e"
        sl.InsertString 7 "fff"
        sl.InsertString 6 "zzzzzz"

        let sb = StringBuilder ()

        for c in sl :> Collections.IEnumerable do
            sb.Append c |> ignore

        Assert.That (sb.ToString (), Is.EqualTo "abcccbzzzzzzdfffde")

    [<Test>]
    member _.SkipListString_IEnumerableOfChar () =
        let sl = getTestSkipListString ()

        sl.InsertString 0 "a"
        sl.InsertString 1 "bb"
        sl.InsertString 2 "ccc"
        sl.InsertString 6 "dd"
        sl.InsertString 8 "e"
        sl.InsertString 7 "fff"
        sl.InsertString 6 "zzzzzz"

        let sb = StringBuilder ()

        for c in sl :> Collections.Generic.IEnumerable<char> do
            sb.Append c |> ignore

        Assert.That (sb.ToString (), Is.EqualTo "abcccbzzzzzzdfffde")
