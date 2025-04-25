module BinarySearchTest

open NUnit.Framework

open BinarySearch

[<TestFixture>]
type BinarySearchTest () =

    // findFirstEqualInSortedArray ---------------------------------------------

    [<Test>]
    member _.findFirstEqualInSortedArray_UpTo_3_Items () =
        let f = findFirstEqualInSortedArray
        let wholeArray = [| 1; 3; 5 |]

        let arr = [| |]
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

        let arr = wholeArray[0..0]  // 1
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (Some 0 , f (compareTo 1) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

        let arr = wholeArray[0..1]  // 1 3
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (Some 0 , f (compareTo 1) arr)
        Assert.AreEqual (None   , f (compareTo 2) arr)
        Assert.AreEqual (Some 1 , f (compareTo 3) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

        let arr = wholeArray[0..2]  // 1 3 5
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (Some 0 , f (compareTo 1) arr)
        Assert.AreEqual (None   , f (compareTo 2) arr)
        Assert.AreEqual (Some 1 , f (compareTo 3) arr)
        Assert.AreEqual (None   , f (compareTo 4) arr)
        Assert.AreEqual (Some 2 , f (compareTo 5) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

    [<Test>]
    member _.findFirstEqualInSortedArray_6_Items () =
        let f = findFirstEqualInSortedArray
        let arr = [| 1; 1; 3; 3; 5; 5 |]

        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (Some 0 , f (compareTo 1) arr)
        Assert.AreEqual (None   , f (compareTo 2) arr)
        Assert.AreEqual (Some 2 , f (compareTo 3) arr)
        Assert.AreEqual (None   , f (compareTo 4) arr)
        Assert.AreEqual (Some 4 , f (compareTo 5) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

    // findLastEqualInSortedArray ----------------------------------------------

    [<Test>]
    member _.findLastEqualInSortedArray_UpTo_3_Items () =
        let f = findLastEqualInSortedArray
        let wholeArray = [| 1; 3; 5 |]

        let arr = [| |]
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

        let arr = wholeArray[0..0]  // 1
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (Some 0 , f (compareTo 1) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

        let arr = wholeArray[0..1]  // 1 3
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (Some 0 , f (compareTo 1) arr)
        Assert.AreEqual (None   , f (compareTo 2) arr)
        Assert.AreEqual (Some 1 , f (compareTo 3) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

        let arr = wholeArray[0..2]  // 1 3 5
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (Some 0 , f (compareTo 1) arr)
        Assert.AreEqual (None   , f (compareTo 2) arr)
        Assert.AreEqual (Some 1 , f (compareTo 3) arr)
        Assert.AreEqual (None   , f (compareTo 4) arr)
        Assert.AreEqual (Some 2 , f (compareTo 5) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

    [<Test>]
    member _.findLastEqualInSortedArray_6_Items () =
        let f = findLastEqualInSortedArray
        let arr = [| 1; 1; 3; 3; 5; 5 |]

        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (Some 1 , f (compareTo 1) arr)
        Assert.AreEqual (None   , f (compareTo 2) arr)
        Assert.AreEqual (Some 3 , f (compareTo 3) arr)
        Assert.AreEqual (None   , f (compareTo 4) arr)
        Assert.AreEqual (Some 5 , f (compareTo 5) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

    // findLastLowerInSortedArray ----------------------------------------------

    [<Test>]
    member _.findLastLowerInSortedArray_UpTo_3_Items () =
        let f = findLastLowerInSortedArray
        let wholeArray = [| 1; 3; 5 |]

        let arr = [| |]
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

        let arr = wholeArray[0..0]  // 1
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (None   , f (compareTo 1) arr)
        Assert.AreEqual (Some 0 , f (compareTo 9) arr)

        let arr = wholeArray[0..1]  // 1 3
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (None   , f (compareTo 1) arr)
        Assert.AreEqual (Some 0 , f (compareTo 2) arr)
        Assert.AreEqual (Some 0 , f (compareTo 3) arr)
        Assert.AreEqual (Some 1 , f (compareTo 9) arr)

        let arr = wholeArray[0..2]  // 1 3 5
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (None   , f (compareTo 1) arr)
        Assert.AreEqual (Some 0 , f (compareTo 2) arr)
        Assert.AreEqual (Some 0 , f (compareTo 3) arr)
        Assert.AreEqual (Some 1 , f (compareTo 4) arr)
        Assert.AreEqual (Some 1 , f (compareTo 5) arr)
        Assert.AreEqual (Some 2 , f (compareTo 9) arr)

    [<Test>]
    member _.findLastLowerInSortedArray_6_Items () =
        let f = findLastLowerInSortedArray
        let arr = [| 1; 1; 3; 3; 5; 5 |]

        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (None   , f (compareTo 1) arr)
        Assert.AreEqual (Some 1 , f (compareTo 2) arr)
        Assert.AreEqual (Some 1 , f (compareTo 3) arr)
        Assert.AreEqual (Some 3 , f (compareTo 4) arr)
        Assert.AreEqual (Some 3 , f (compareTo 5) arr)
        Assert.AreEqual (Some 5 , f (compareTo 9) arr)

    // findFirstGreaterInSortedArray -------------------------------------------

    [<Test>]
    member _.findFirstGreaterInSortedArray_UpTo_3_Items () =
        let f = findFirstGreaterInSortedArray
        let wholeArray = [| 1; 3; 5 |]

        let arr = [| |]
        Assert.AreEqual (None   , f (compareTo 0) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

        let arr = wholeArray[0..0]  // 1
        Assert.AreEqual (Some 0 , f (compareTo 0) arr)
        Assert.AreEqual (None   , f (compareTo 1) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

        let arr = wholeArray[0..1]  // 1 3
        Assert.AreEqual (Some 0 , f (compareTo 0) arr)
        Assert.AreEqual (Some 1 , f (compareTo 1) arr)
        Assert.AreEqual (Some 1 , f (compareTo 2) arr)
        Assert.AreEqual (None   , f (compareTo 3) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

        let arr = wholeArray[0..2]  // 1 3 5
        Assert.AreEqual (Some 0 , f (compareTo 0) arr)
        Assert.AreEqual (Some 1 , f (compareTo 1) arr)
        Assert.AreEqual (Some 1 , f (compareTo 2) arr)
        Assert.AreEqual (Some 2 , f (compareTo 3) arr)
        Assert.AreEqual (Some 2 , f (compareTo 4) arr)
        Assert.AreEqual (None   , f (compareTo 5) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

    [<Test>]
    member _.findFirstGreaterInSortedArray_6_Items () =
        let f = findFirstGreaterInSortedArray
        let arr = [| 1; 1; 3; 3; 5; 5 |]

        Assert.AreEqual (Some 0 , f (compareTo 0) arr)
        Assert.AreEqual (Some 2 , f (compareTo 1) arr)
        Assert.AreEqual (Some 2 , f (compareTo 2) arr)
        Assert.AreEqual (Some 4 , f (compareTo 3) arr)
        Assert.AreEqual (Some 4 , f (compareTo 4) arr)
        Assert.AreEqual (None   , f (compareTo 5) arr)
        Assert.AreEqual (None   , f (compareTo 9) arr)

    // findLastEqOrLtOrFirstGtInSortedArray ------------------------------------

    [<Test>]
    member _.findLastEqOrLtOrFirstGtInSortedArray_UpTo_3_Items () =
        let f = findLastEqOrLtOrFirstGtInSortedArray
        let wholeArray = [| 1; 3; 5 |]

        let arr = [| |]
        Assert.AreEqual ((0, None   ), f (compareTo 0) arr)
        Assert.AreEqual ((0, None   ), f (compareTo 9) arr)

        let arr = wholeArray[0..0]  // 1
        Assert.AreEqual ((0, Some +1), f (compareTo 0) arr)
        Assert.AreEqual ((0, Some  0), f (compareTo 1) arr)
        Assert.AreEqual ((0, Some -1), f (compareTo 9) arr)

        let arr = wholeArray[0..1]  // 1 3
        Assert.AreEqual ((0, Some +1), f (compareTo 0) arr)
        Assert.AreEqual ((0, Some  0), f (compareTo 1) arr)
        Assert.AreEqual ((0, Some -1), f (compareTo 2) arr)
        Assert.AreEqual ((1, Some  0), f (compareTo 3) arr)
        Assert.AreEqual ((1, Some -1), f (compareTo 9) arr)

        let arr = wholeArray[0..2]  // 1 3 5
        Assert.AreEqual ((0, Some +1), f (compareTo 0) arr)
        Assert.AreEqual ((0, Some  0), f (compareTo 1) arr)
        Assert.AreEqual ((0, Some -1), f (compareTo 2) arr)
        Assert.AreEqual ((1, Some  0), f (compareTo 3) arr)
        Assert.AreEqual ((1, Some -1), f (compareTo 4) arr)
        Assert.AreEqual ((2, Some  0), f (compareTo 5) arr)
        Assert.AreEqual ((2, Some -1), f (compareTo 9) arr)

    [<Test>]
    member _.findLastEqOrLtOrFirstGtInSortedArray_6_Items () =
        let f = findLastEqOrLtOrFirstGtInSortedArray
        let arr = [| 1; 1; 3; 3; 5; 5 |]

        Assert.AreEqual ((0, Some +1), f (compareTo 0) arr)
        Assert.AreEqual ((1, Some  0), f (compareTo 1) arr)
        Assert.AreEqual ((1, Some -1), f (compareTo 2) arr)
        Assert.AreEqual ((3, Some  0), f (compareTo 3) arr)
        Assert.AreEqual ((3, Some -1), f (compareTo 4) arr)
        Assert.AreEqual ((5, Some  0), f (compareTo 5) arr)
        Assert.AreEqual ((5, Some -1), f (compareTo 9) arr)

    // findLastEqOrLtInSortedArray ---------------------------------------------

    [<Test>]
    member _.findLastEqOrLtInSortedArray_UpTo_3_Items () =
        let f = findLastEqOrLtInSortedArray
        let wholeArray = [| 1; 3; 5 |]

        let arr = [| |]
        Assert.AreEqual (None  , f (compareTo 0) arr)
        Assert.AreEqual (None  , f (compareTo 9) arr)

        let arr = wholeArray[0..0]  // 1
        Assert.AreEqual (None  , f (compareTo 0) arr)
        Assert.AreEqual (Some 0, f (compareTo 1) arr)
        Assert.AreEqual (Some 0, f (compareTo 9) arr)

        let arr = wholeArray[0..1]  // 1 3
        Assert.AreEqual (None  , f (compareTo 0) arr)
        Assert.AreEqual (Some 0, f (compareTo 1) arr)
        Assert.AreEqual (Some 0, f (compareTo 2) arr)
        Assert.AreEqual (Some 1, f (compareTo 3) arr)
        Assert.AreEqual (Some 1, f (compareTo 9) arr)

        let arr = wholeArray[0..2]  // 1 3 5
        Assert.AreEqual (None  , f (compareTo 0) arr)
        Assert.AreEqual (Some 0, f (compareTo 1) arr)
        Assert.AreEqual (Some 0, f (compareTo 2) arr)
        Assert.AreEqual (Some 1, f (compareTo 3) arr)
        Assert.AreEqual (Some 1, f (compareTo 4) arr)
        Assert.AreEqual (Some 2, f (compareTo 5) arr)
        Assert.AreEqual (Some 2, f (compareTo 9) arr)

    [<Test>]
    member _.findLastEqOrLtInSortedArray_6_Items () =
        let f = findLastEqOrLtInSortedArray
        let arr = [| 1; 1; 3; 3; 5; 5 |]

        Assert.AreEqual (None  , f (compareTo 0) arr)
        Assert.AreEqual (Some 1, f (compareTo 1) arr)
        Assert.AreEqual (Some 1, f (compareTo 2) arr)
        Assert.AreEqual (Some 3, f (compareTo 3) arr)
        Assert.AreEqual (Some 3, f (compareTo 4) arr)
        Assert.AreEqual (Some 5, f (compareTo 5) arr)
        Assert.AreEqual (Some 5, f (compareTo 9) arr)

    // findFirstEqOrGtOrLastLtInSortedArray ------------------------------------

    [<Test>]
    member _.findFirstEqOrGtOrLastLtInSortedArray_UpTo_3_Items () =
        let f = findFirstEqOrGtOrLastLtInSortedArray
        let wholeArray = [| 1; 3; 5 |]

        let arr = [| |]
        Assert.AreEqual ((0, None   ), f (compareTo 0) arr)
        Assert.AreEqual ((0, None   ), f (compareTo 9) arr)

        let arr = wholeArray[0..0]  // 1
        Assert.AreEqual ((0, Some +1), f (compareTo 0) arr)
        Assert.AreEqual ((0, Some  0), f (compareTo 1) arr)
        Assert.AreEqual ((0, Some -1), f (compareTo 9) arr)

        let arr = wholeArray[0..1]  // 1 3
        Assert.AreEqual ((0, Some +1), f (compareTo 0) arr)
        Assert.AreEqual ((0, Some  0), f (compareTo 1) arr)
        Assert.AreEqual ((1, Some +1), f (compareTo 2) arr)
        Assert.AreEqual ((1, Some  0), f (compareTo 3) arr)
        Assert.AreEqual ((1, Some -1), f (compareTo 9) arr)

        let arr = wholeArray[0..2]  // 1 3 5
        Assert.AreEqual ((0, Some +1), f (compareTo 0) arr)
        Assert.AreEqual ((0, Some  0), f (compareTo 1) arr)
        Assert.AreEqual ((1, Some +1), f (compareTo 2) arr)
        Assert.AreEqual ((1, Some  0), f (compareTo 3) arr)
        Assert.AreEqual ((2, Some +1), f (compareTo 4) arr)
        Assert.AreEqual ((2, Some  0), f (compareTo 5) arr)
        Assert.AreEqual ((2, Some -1), f (compareTo 9) arr)

    [<Test>]
    member _.findFirstEqOrGtOrLastLtInSortedArray_6_Items () =
        let f = findFirstEqOrGtOrLastLtInSortedArray
        let arr = [| 1; 1; 3; 3; 5; 5 |]

        Assert.AreEqual ((0, Some +1), f (compareTo 0) arr)
        Assert.AreEqual ((0, Some  0), f (compareTo 1) arr)
        Assert.AreEqual ((2, Some +1), f (compareTo 2) arr)
        Assert.AreEqual ((2, Some  0), f (compareTo 3) arr)
        Assert.AreEqual ((4, Some +1), f (compareTo 4) arr)
        Assert.AreEqual ((4, Some  0), f (compareTo 5) arr)
        Assert.AreEqual ((5, Some -1), f (compareTo 9) arr)

    // findFirstEqOrGtInSortedArray --------------------------------------------

    [<Test>]
    member _.findFirstEqOrGtInSortedArray_UpTo_3_Items () =
        let f = findFirstEqOrGtInSortedArray
        let wholeArray = [| 1; 3; 5 |]

        let arr = [| |]
        Assert.AreEqual (None  , f (compareTo 0) arr)
        Assert.AreEqual (None  , f (compareTo 9) arr)

        let arr = wholeArray[0..0]  // 1
        Assert.AreEqual (Some 0, f (compareTo 0) arr)
        Assert.AreEqual (Some 0, f (compareTo 1) arr)
        Assert.AreEqual (None  , f (compareTo 9) arr)

        let arr = wholeArray[0..1]  // 1 3
        Assert.AreEqual (Some 0, f (compareTo 0) arr)
        Assert.AreEqual (Some 0, f (compareTo 1) arr)
        Assert.AreEqual (Some 1, f (compareTo 2) arr)
        Assert.AreEqual (Some 1, f (compareTo 3) arr)
        Assert.AreEqual (None  , f (compareTo 9) arr)

        let arr = wholeArray[0..2]  // 1 3 5
        Assert.AreEqual (Some 0, f (compareTo 0) arr)
        Assert.AreEqual (Some 0, f (compareTo 1) arr)
        Assert.AreEqual (Some 1, f (compareTo 2) arr)
        Assert.AreEqual (Some 1, f (compareTo 3) arr)
        Assert.AreEqual (Some 2, f (compareTo 4) arr)
        Assert.AreEqual (Some 2, f (compareTo 5) arr)
        Assert.AreEqual (None  , f (compareTo 9) arr)

    [<Test>]
    member _.findFirstEqOrGtInSortedArray_6_Items () =
        let f = findFirstEqOrGtInSortedArray
        let arr = [| 1; 1; 3; 3; 5; 5 |]

        Assert.AreEqual (Some 0, f (compareTo 0) arr)
        Assert.AreEqual (Some 0, f (compareTo 1) arr)
        Assert.AreEqual (Some 2, f (compareTo 2) arr)
        Assert.AreEqual (Some 2, f (compareTo 3) arr)
        Assert.AreEqual (Some 4, f (compareTo 4) arr)
        Assert.AreEqual (Some 4, f (compareTo 5) arr)
        Assert.AreEqual (None  , f (compareTo 9) arr)

    // getIndexForInsertIntoSortedArray ----------------------------------------

    [<Test>]
    member _.getIndexForInsertIntoSortedArray_UpTo_3_Items () =
        let f = getIndexForInsertIntoSortedArray
        let wholeArray = [| 1; 3; 5 |]

        let arr = [| |]
        Assert.AreEqual (0, f (compareTo 0) arr)
        Assert.AreEqual (0, f (compareTo 9) arr)

        let arr = wholeArray[0..0]  // 1
        Assert.AreEqual (0, f (compareTo 0) arr)
        Assert.AreEqual (1, f (compareTo 1) arr)
        Assert.AreEqual (1, f (compareTo 9) arr)

        let arr = wholeArray[0..1]  // 1 3
        Assert.AreEqual (0, f (compareTo 0) arr)
        Assert.AreEqual (1, f (compareTo 1) arr)
        Assert.AreEqual (1, f (compareTo 2) arr)
        Assert.AreEqual (2, f (compareTo 3) arr)
        Assert.AreEqual (2, f (compareTo 9) arr)

        let arr = wholeArray[0..2]  // 1 3 5
        Assert.AreEqual (0, f (compareTo 0) arr)
        Assert.AreEqual (1, f (compareTo 1) arr)
        Assert.AreEqual (1, f (compareTo 2) arr)
        Assert.AreEqual (2, f (compareTo 3) arr)
        Assert.AreEqual (2, f (compareTo 4) arr)
        Assert.AreEqual (3, f (compareTo 5) arr)
        Assert.AreEqual (3, f (compareTo 9) arr)

    [<Test>]
    member _.getIndexForInsertIntoSortedArray_6_Items () =
        let f = getIndexForInsertIntoSortedArray
        let arr = [| 1; 1; 3; 3; 5; 5 |]

        Assert.AreEqual (0, f (compareTo 0) arr)
        Assert.AreEqual (2, f (compareTo 1) arr)
        Assert.AreEqual (2, f (compareTo 2) arr)
        Assert.AreEqual (4, f (compareTo 3) arr)
        Assert.AreEqual (4, f (compareTo 4) arr)
        Assert.AreEqual (6, f (compareTo 5) arr)
        Assert.AreEqual (6, f (compareTo 9) arr)
