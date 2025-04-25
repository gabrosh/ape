module BinarySearch

open System.Collections.Generic

// Returns -1 if a < b; 0 if a = b; +1 if a > b.
let compareTo b a =
    if   a < b then -1
    elif a > b then +1
    else 0

/// Returns index of the first equal element of sorted array according to compareFun.
/// If such element is not found, returns None.
let findFirstEqualInSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    if arr.Count <> 0 then
        let rec loop = fun a b ->
            // If a + 1 = b, then m = a. If a = b, then m = a = b.
            let m = (a + b) / 2
            let c = compareFun arr[m]

            if a = b then
                if c = 0 then (Some m) else None
            elif c < 0 then
                loop (m + 1) b
            else  // c >= 0
                loop a m

        loop 0 (arr.Count - 1)
    else
        None

/// Returns index of the last equal element of sorted array according to compareFun.
/// If such element is not found, returns None.
let findLastEqualInSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    if arr.Count <> 0 then
        let rec loop = fun a b ->
            // If a + 1 = b, then m = b. If a = b, then m = a = b.
            let m = ((a + 1) + b) / 2
            let c = compareFun arr[m]

            if a = b then
                if c = 0 then (Some m) else None
            elif c <= 0 then
                loop m b
            else  // c > 0
                loop a (m - 1)

        loop 0 (arr.Count - 1)
    else
        None

/// Returns the first equal element of sorted array according to compareFun.
/// If such element is not found, throws System.ArgumentException.
let getFirstEqualInSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    let index =
        findFirstEqualInSortedArray compareFun arr |> Option.get
    arr[index]

/// Returns the last equal element of sorted array according to compareFun.
/// If such element is not found, throws System.ArgumentException.
let getLastEqualInSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    let index =
        findLastEqualInSortedArray compareFun arr |> Option.get
    arr[index]

/// Finds the last lower element of sorted array according to compareFun.
/// Returns index of the element. If such element is not found or the array
/// is empty, returns None.
let findLastLowerInSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    if arr.Count <> 0 then
        let rec loop = fun a b ->
            // If a + 1 = b, then m = b. If a = b, then m = a = b.
            let m = ((a + 1) + b) / 2
            let c = compareFun arr[m]

            if a = b then
                if c < 0 then (Some m) else None
            elif c < 0 then
                loop m b
            else  // c >= 0
                loop a (m - 1)

        loop 0 (arr.Count - 1)
    else
        None

/// Finds the first greater element of sorted array according to compareFun.
/// Returns index of the element. If such element is not found or the array
/// is empty, returns None.
let findFirstGreaterInSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    if arr.Count <> 0 then
        let rec loop = fun a b ->
            // If a + 1 = b, then m = a. If a = b, then m = a = b.
            let m = (a + b) / 2
            let c = compareFun arr[m]

            if a = b then
                if c > 0 then (Some m) else None
            elif c <= 0 then
                loop (m + 1) b
            else  // c > 0
                loop a m

        loop 0 (arr.Count - 1)
    else
        None

/// Finds the last equal or lower or the first greater element of sorted array
/// according to compareFun (in this order of priorities). Returns index of
/// the element and the corresponding result of compareFun, that is 0, -1 or +1.
/// If the array is empty, returns (0, None).
let findLastEqOrLtOrFirstGtInSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    if arr.Count <> 0 then
        let rec loop = fun a b ->
            // If a + 1 = b, then m = b. If a = b, then m = a = b.
            let m = ((a + 1) + b) / 2
            let c = compareFun arr[m]

            if a = b then
                (m, Some c)
            elif c <= 0 then
                loop m b
            else  // c > 0
                loop a (m - 1)

        loop 0 (arr.Count - 1)
    else
        (0, None)

/// Returns index of the last equal or lower element of sorted array
/// according to compareFun. If such element is not found, returns None.
let findLastEqOrLtInSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    let result = findLastEqOrLtOrFirstGtInSortedArray compareFun arr

    match result with
    | (m, Some c) when c <= 0 -> Some m
    | _                       -> None

/// Finds the first equal or greater or the last lower element of sorted array
/// according to compareFun (in this order of priorities). Returns index of
/// the element and the corresponding result of compareFun, that is 0, -1 or +1.
/// If the array is empty, returns (0, None).
let findFirstEqOrGtOrLastLtInSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    if arr.Count <> 0 then
        let rec loop = fun a b ->
            // If a + 1 = b, then m = a. If a = b, then m = a = b.
            let m = (a + b) / 2
            let c = compareFun arr[m]

            if a = b then
                (m, Some c)
            elif c < 0 then
                loop (m + 1) b
            else  // c >= 0
                loop a m

        loop 0 (arr.Count - 1)
    else
        (0, None)

/// Returns index of the first equal or greater element of sorted array
/// according to compareFun. If such element is not found, returns None.
let findFirstEqOrGtInSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    let result = findFirstEqOrGtOrLastLtInSortedArray compareFun arr

    match result with
    | (m, Some c) when c >= 0 -> Some m
    | _                       -> None

/// Returns index for inserting a new element into sorted array.
/// Assumes that compareFun tests its input against the new element.
let getIndexForInsertIntoSortedArray<'L, 'T when 'L :> IList<'T>>
    compareFun (arr: 'L) =

    match findLastEqOrLtOrFirstGtInSortedArray compareFun arr with
    | _     , None               -> 0
    | index , Some c when c <= 0 -> index + 1
    | index , Some _             -> index
