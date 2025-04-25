module CompletionUtilsTest

open NUnit.Framework

open CompletionUtils

[<TestFixture>]
type CompletionUtilsTest () =

    // getItemsStringAndIndices ------------------------------------------------

    [<Test>]
    member _.getItemsStringAndIndices_noCompletions () =
        let rows, offsets =
            getItemsStringAndIndices 6 (ResizeArray ["xx"])

        Assert.AreEqual ("", rows)
        Assert.AreEqual (ResizeArray [], offsets)

    [<Test>]
    member _.getItemsStringAndIndices_withoutWrapping () =
        let rows, offsets =
            getItemsStringAndIndices 5 (ResizeArray ["x"; "xa"; "xb"])

        Assert.AreEqual ("xa xb", rows)
        Assert.AreEqual (ResizeArray [(0, 2); (3, 2)], offsets)

    [<Test>]
    member _.getItemsStringAndIndices_withWrapping () =
        let rows, offsets =
            getItemsStringAndIndices 8 (ResizeArray ["x"; "xa"; "xb"; "xc"; "xd"])

        Assert.AreEqual ("xa xb  ~~ xc xd", rows)
        Assert.AreEqual (ResizeArray [(0, 2); (3, 2); (10, 2); (13, 2)], offsets)

    [<Test>]
    member _.getItemsStringAndIndices_itemTooLong_firstItem_withoutWrapping () =
        let rows, offsets =
            getItemsStringAndIndices 8 (ResizeArray ["x"; "xaaaaaa"])

        Assert.AreEqual ("xaaaaaa", rows)
        Assert.AreEqual (ResizeArray [(0, 7)], offsets)

    [<Test>]
    member _.getItemsStringAndIndices_itemTooLong_firstItem_withWrapping () =
        let rows, offsets =
            getItemsStringAndIndices 8 (ResizeArray ["x"; "xaaaaaa"; "xb"])

        Assert.AreEqual ("xaaaaa ~~ xb", rows)
        Assert.AreEqual (ResizeArray [(0, 6); (10, 2)], offsets)

    [<Test>]
    member _.getItemsStringAndIndices_itemTooLong_secondItem_withoutWrapping () =
        let rows, offsets =
            getItemsStringAndIndices 8 (ResizeArray ["x"; "xa"; "xbbbbbb"])

        Assert.AreEqual ("xa     ~~ xbbbbb", rows)
        Assert.AreEqual (ResizeArray [(0, 2); (10, 6)], offsets)

    [<Test>]
    member _.getItemsStringAndIndices_itemTooLong_secondItem_withWrapping () =
        let rows, offsets =
            getItemsStringAndIndices 8 (ResizeArray ["x"; "xa"; "xbbbbbb"; "xc"])

        Assert.AreEqual ("xa     ~~ xbbb ~~ xc", rows)
        Assert.AreEqual (ResizeArray [(0, 2); (10, 4); (18, 2)], offsets)
