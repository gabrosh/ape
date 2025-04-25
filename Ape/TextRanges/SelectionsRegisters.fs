module SelectionsRegisters

open System.Collections.Immutable

open DataTypes
open Position
open Selection
open TextRangesModifier

type Item = {
    name:       char
    selections: ImmutableArray<Selection>
    mainIndex:  int
}

type private PrivateItem = {
    name:       char
    modifier:   TextRangesModifier<Selection>
    mainIndex:  int
}

let private defaultRegisterName = '"'

// checking for changes

let private isSelectionMatchingTo (a: Selection, b: Selection) =
       a.first     = b.first
    && a.last      = b.last
    && a.isForward = b.isForward

let private areSelectionsMatchingTo
    (a: ResizeArray<Selection>)
    (b: ImmutableArray<Selection>)
  =
    if a.Count = b.Length then
        a |> Seq.zip b |> Seq.forall isSelectionMatchingTo
    else
        false

let private isRegisterMatchingTo (a: PrivateItem, b: Item) =
       a.name      = b.name
    && a.mainIndex = b.mainIndex
    && areSelectionsMatchingTo a.modifier.Items b.selections

let private areRegistersMatchingTo
    (a: ResizeArray<PrivateItem>)
    (b: ImmutableArray<Item>)
  =
    if a.Count = b.Length then
        Seq.zip a b |> Seq.forall isRegisterMatchingTo
    else
        false

/// Returns register name.
let getRegisterName register =
    match register with
    | DefaultRegister -> defaultRegisterName
    | SelectedRegister (_isUpper, c) -> c

/// SelectionsRegisters holds a list of selections binded to a name
/// and modifies them according to given modifying specifications.

type SelectionsRegisters () =
    let myItems = ResizeArray<PrivateItem> ()

    // immutable copy of myItems, None if the copy was invalidated
    let mutable myAllImmutable = None

    /// Clears registers repository.
    member _.Clear () =
        myItems.Clear ()

    /// Sets copy of selections and mainIndex to given register.
    member _.Set register (selections: ResizeArray<Selection>) (mainIndex: int) =
        let name = getRegisterName register

        let item = {
            name      = name
            modifier  = TextRangesModifier (ResizeArray selections)
            mainIndex = mainIndex
        }

        let index = myItems.FindIndex (
            fun item -> item.name = name
        )

        if index <> -1 then
            myItems[index] <- item
        else
            myItems.Add item

    /// Returns selections and mainIndex stored in given register.
    member _.Get register : (ResizeArray<Selection> * int) option =
        let name = getRegisterName register

        myItems |> Seq.tryPick (
            fun item ->
                if item.name = name then
                    Some (item.modifier.Items, item.mainIndex)
                else
                    None
        )

    /// Removes given register.
    member _.Remove register =
        let name = getRegisterName register

        let index = myItems.FindIndex (
            fun item -> item.name = name
        )

        if index <> -1 then
            myItems.RemoveAt index

    /// Returns immutable copy of all registers. Returns the same object
    /// if registers repository hasn't changed since the previous call.
    member this.GetAllImmutable () =
        if this.HasChanged () then
            myAllImmutable <- Some (this.MakeAllImmutable ())

        myAllImmutable |> Option.get

    /// Clears registers repository and adds given registers into it.
    member _.SetAllFrom (selsRegisters: ImmutableArray<Item>) =
        myItems.Clear ()

        selsRegisters |> Seq.map (
            fun item -> {
                name      = item.name
                modifier  = TextRangesModifier (ResizeArray item.selections)
                mainIndex = item.mainIndex
            }
        ) |> myItems.AddRange

        myAllImmutable <- Some selsRegisters

    // actions delegated to all modifiers

    member _.StartApplyingModifications () =
        for item in myItems do
            item.modifier.StartApplyingModifications ()

    member _.StopApplyingModifications () =
        for item in myItems do
            item.modifier.StopApplyingModifications ()

    member _.ApplyInsert (insertSpec: InsertSpec) =
        for item in myItems do
            item.modifier.ApplyInsert insertSpec

    member _.ApplyDelete (deleteSpec: DeleteSpec) =
        for item in myItems do
            item.modifier.ApplyDelete deleteSpec

    member _.ApplyOneLineInsert (insertSpec: OneLineInsertSpec) =
        for item in myItems do
            item.modifier.ApplyOneLineInsert insertSpec

    member _.ApplyOneLineDelete (deleteSpec: OneLineDeleteSpec) =
        for item in myItems do
            item.modifier.ApplyOneLineDelete deleteSpec

    /// Modify all selections which are not positioned before EOF.
    member _.AssureAllBeforeEof lastEolPosition =
        for item in myItems do
            let selections = item.modifier.Items

            for i = 0 to selections.Count - 1 do
                let selection = selections[i]

                if selection.first > lastEolPosition ||
                   selection.last  > lastEolPosition
                then
                    selections[i] <- {
                        selection with
                            first = min selection.first lastEolPosition
                            last  = min selection.last  lastEolPosition
                    }

    // auxiliary

    member private _.MakeAllImmutable () =
        myItems |> Seq.map (
            fun item -> {
                name       = item.name
                selections = ImmutableArray.CreateRange item.modifier.Items
                mainIndex  = item.mainIndex
            }
        ) |> ImmutableArray.CreateRange

    member private _.HasChanged () =
        match myAllImmutable with
        | Some allImmutable ->
            not (areRegistersMatchingTo myItems allImmutable)
        | None ->
            true
