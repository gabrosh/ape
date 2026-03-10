module SkipListStringFuzzyTest

open NUnit.Framework
open System
open System.Text

open SkipListString

let moduloForClear            = 20
let moduloForGetUndoNodes     = 20
let moduloForSetFromUndoNodes = 20

let private getTestSkipListString () =
    SkipListString (10, 3)

let private setCharAt (s: string) (index: int) (c: char) =
    let sb = StringBuilder s
    sb[index] <- c
    sb.ToString ()

[<TestFixture>]
type SkipListStringFuzzyTests () =
    let rnd = Random ()

    let sl = getTestSkipListString ()
    
    let mutable referenceString = ""

    let mutable undoNodes  = ResizeArray ()
    let mutable undoString = ""

    let getRandomChar () =
        char (rnd.Next (97, 123))

    [<Test>]
    member this.SkipListString_FuzzyTest () =
        for i = 1 to 100_000 do
            let operation = rnd.Next 12

            match operation with
            | 0 -> this.ExecuteGet          ()
            | 1 -> this.ExecuteGetRangeSeq  ()
            | 2 -> this.ExecuteSet          ()
            | 3 -> this.ExecuteAdd          ()
            | 4 -> this.ExecuteAddString    ()
            | 5 -> this.ExecuteInsert       ()
            | 6 -> this.ExecuteInsertString ()
            | 7 -> this.ExecuteRemove       ()
            | 8 -> this.ExecuteRemoveRange  ()
            | 9 ->
                if i % moduloForClear = 0 then
                   this.ExecuteClear ()
            | 10 ->
                if i % moduloForGetUndoNodes = 0 then
                   this.ExecuteGetUndoNodes ()
            | _ ->
                if i % moduloForSetFromUndoNodes = 0 then
                   this.ExecuteSetFromUndoNodes ()

            //sl.CheckConsistency ()

    member private _.ExecuteGet () =
        if referenceString.Length > 0 then
            let index = rnd.Next referenceString.Length
            let expectedChar = referenceString[index]
            let actualChar = sl.Get index

            Assert.That (actualChar.ToString (), Is.EqualTo (expectedChar.ToString ()),
                $"Get at {index}")

    member private _.ExecuteGetRangeSeq () =
        if referenceString.Length > 0 then
            let index = rnd.Next referenceString.Length
            let count = rnd.Next (1, referenceString.Length - index + 1)

            if count > 0 then
                let expectedRange = referenceString.Substring (index, count)
                let actualRange = String.Concat (sl.GetRangeSeq index count)

                Assert.That (actualRange, Is.EqualTo expectedRange,
                    $"GetRangeSeq at {index}, count {count}")

    member private _.ExecuteSet () =
        if referenceString.Length > 0 then
            let index = rnd.Next referenceString.Length
            let setChar = getRandomChar ()

            sl.Set index setChar
            referenceString <- setCharAt referenceString index setChar

            Assert.That (sl.AsString (), Is.EqualTo referenceString,
                $"After Set at {index}")

    member private _.ExecuteAdd () =
        if referenceString.Length < 30 then
            let index = referenceString.Length
            let insertedChar = getRandomChar ()

            sl.Add insertedChar
            referenceString <- referenceString.Insert (index, insertedChar.ToString ())

            Assert.That (sl.AsString (), Is.EqualTo referenceString,
                $"After Add: '{insertedChar}'")

    member private _.ExecuteAddString () =
        if referenceString.Length < 30 then
            let index = referenceString.Length
            let strLen = rnd.Next (1, 10)
            let insertedStr = String (
                Array.init strLen (fun _ -> getRandomChar ())
            )

            sl.AddString insertedStr
            referenceString <- referenceString.Insert (index, insertedStr)

            Assert.That (sl.AsString (), Is.EqualTo referenceString,
                $"After AddString: '{insertedStr}'")

    member private _.ExecuteInsert () =
        if referenceString.Length < 30 then
            let index = rnd.Next (referenceString.Length + 1)
            let insertedChar = getRandomChar ()

            sl.Insert index insertedChar
            referenceString <- referenceString.Insert (index, insertedChar.ToString ())

            Assert.That (sl.AsString (), Is.EqualTo referenceString,
                $"After Insert at {index}: '{insertedChar}'")

    member private _.ExecuteInsertString () =
        if referenceString.Length < 30 then
            let index = rnd.Next (referenceString.Length + 1)
            let strLen = rnd.Next (1, 10)
            let insertedStr = String (
                Array.init strLen (fun _ -> getRandomChar ())
            )

            sl.InsertString index insertedStr
            referenceString <- referenceString.Insert (index, insertedStr)

            Assert.That (sl.AsString (), Is.EqualTo referenceString,
                $"After InsertString at {index}: '{insertedStr}'")

    member private _.ExecuteRemove () =
        if referenceString.Length > 0 then
            let index = rnd.Next referenceString.Length

            sl.Remove index
            referenceString <- referenceString.Remove (index, 1)

            Assert.That (sl.AsString (), Is.EqualTo referenceString,
                $"After Remove at {index}")

    member private _.ExecuteRemoveRange () =
        if referenceString.Length > 0 then
            let index = rnd.Next referenceString.Length
            let count = rnd.Next (1, referenceString.Length - index + 1)

            sl.RemoveRange index count
            referenceString <- referenceString.Remove (index, count)

            Assert.That (sl.AsString (), Is.EqualTo referenceString,
                $"After RemoveRange at {index}, count {count}")

    member private _.ExecuteClear () =
        sl.Clear ()
        referenceString <- ""

        Assert.That (sl.AsString (), Is.EqualTo referenceString,
            $"After Clear")

    member private _.ExecuteGetUndoNodes () =
        undoNodes  <- sl.GetUndoNodes ()
        undoString <- sl.AsString ()

    member private _.ExecuteSetFromUndoNodes () =
        sl.SetFromUndoNodes undoNodes
        referenceString <- undoString

        Assert.That (sl.AsString (), Is.EqualTo referenceString,
            $"After SetFromUndoNodes")
