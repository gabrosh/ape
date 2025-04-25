module TestMines

#if RELEASE

let checkMine _name =
    ()

#else

let checkMine name =
    let toRaise = false

    match name with
    | "LoadFile"
    | "ReloadFile"
        -> ()
    | "InsertChar"
    | "DeleteChar"
    | "DeletePrevChar"
        -> ()
    | _
        -> ()

    if toRaise then
        invalidOp $"Mine {name} was hit"

#endif
