﻿module UndoProvider

open System.Collections.Immutable

open Common
open DataTypes
open Selection

type BufferState = {
    names:            char list
    wasSaved:         bool
    lines:            ImmutableArray<Chars> option
    selections:       ImmutableArray<Selection>
    mainIndex:        int
    selsRegisters:    ImmutableArray<SelectionsRegisters.Item>
    displayPos:       DisplayPos
    pendingWCActions: WantedColumnsAction list
}

let private isSelectionMatchingTo (a: Selection, b: Selection) =
       a.first     = b.first
    && a.last      = b.last
    && a.isForward = b.isForward

let private areSelectionsMatchingTo
    (a: ImmutableArray<Selection>)
    (b: ImmutableArray<Selection>)
  =
    if a.Length = b.Length then
        Seq.zip a b |> Seq.forall isSelectionMatchingTo
    else
        false

let private areSelRegistersMatchingTo
    (a: ImmutableArray<SelectionsRegisters.Item>)
    (b: ImmutableArray<SelectionsRegisters.Item>)
  =
    // reference equality
    a.Equals b

let private statesDiffer_woLines a b =
    // Test fields in the order of increasing complexity.
       a.displayPos <> b.displayPos
    || a.mainIndex  <> b.mainIndex
    || not (areSelRegistersMatchingTo a.selsRegisters b.selsRegisters)
    || not (areSelectionsMatchingTo   a.selections    b.selections)

let private areLinesTheSame
    (a: ImmutableArray<Chars> option)
    (b: ImmutableArray<Chars> option)
  =
    // reference equality
    (a |> Option.get).Equals (b |> Option.get)

[<TailCall>]
let rec findDifferentLines (states: ResizeArray<BufferState>) i delta =
    if delta = -1 && i = 0 then
        i
    elif delta = +1 && i = states.Count - 1 then
        i
    else
        let oldState = states[i]
        let j = i + delta
        let newState = states[j]

        // Don't return index of the state with the same lines.
        if areLinesTheSame newState.lines oldState.lines then
            findDifferentLines states j delta
        else
            j

/// UndoProvider registers, manages and provides undo states. Undo state can be
/// named by several names and can contain several selections registers. This class
/// also maintains information about which undo state is the the last state saved
/// to the file.

type UndoProvider (state: BufferState) =
    // myStates[...].lines and mySavedLines are always Some.
    let myStates = ResizeArray<BufferState> [state]
    let mutable myIsLastStateVolatile = false
    let mutable myCurrent = 0
    let mutable mySavedLines = myStates[myCurrent].lines

    /// Sets given state as the only state and as saved to the file.
    member this.Reset state =
        myStates.Clear ()
        myStates.Add state

        myIsLastStateVolatile <- false
        myCurrent <- 0

        this.SetCurrentStateAsSaved IntType.MaxValue

    /// Clears myIsLastStateVolatile flag.
    member _.ClearIsLastStateVolatile () =
        myIsLastStateVolatile <- false

    /// Removes the state next to the current state or the current state
    /// (depending on isStateVolatile and myIsLastStateVolatile flags) and all
    /// following states, and registers given state as the current state.
    member _.RegisterState state isStateVolatile =
        let toAddState =
            if state.lines = None then
                statesDiffer_woLines state myStates[myCurrent]
            else
                true

        if toAddState then
            let state =
                // If there are no lines provided, take the latest ones.
                if state.lines = None then
                    { state with lines = myStates[myCurrent].lines }
                else
                    state

            if not (myIsLastStateVolatile && isStateVolatile) then
                myCurrent <- myCurrent + 1
            myIsLastStateVolatile <- isStateVolatile

            myStates.RemoveRange (
                myCurrent, myStates.Count - myCurrent
            )
            myStates.Add state

    /// Returns the state preceding the current state
    /// or None if there is no such state.
    member this.GetUndoState () =
        myIsLastStateVolatile <- false

        if myCurrent > 0 then
            this.SwitchAndGetState (myCurrent - 1)
        else
            None

    /// Returns the first state with different lines before the current state
    /// or the first state in myStates collection if it precedes the current state
    /// or None if there is no such state.
    member this.GetUndoStateFast () =
        myIsLastStateVolatile <- false

        if myCurrent > 0 then
            this.SwitchAndGetState (
                findDifferentLines myStates myCurrent -1
            )
        else
            None

    /// Returns the state following the current state
    /// or None if there is no such state.
    member this.GetRedoState () =
        myIsLastStateVolatile <- false

        if myCurrent < myStates.Count - 1 then
            this.SwitchAndGetState (myCurrent + 1)
        else
            None

    /// Returns the first state with different lines after the current state
    /// or the last state in myStates collection if it precedes the current state
    /// or None if there is no such state.
    member this.GetRedoStateFast () =
        myIsLastStateVolatile <- false

        if myCurrent < myStates.Count - 1 then
            this.SwitchAndGetState (
                findDifferentLines myStates myCurrent +1
            )
        else
            None

    /// Adds given name to the list of the current state's names.
    member _.AddNameToCurrentState name =
        let state = myStates[myCurrent]

        if not (state.names |> List.contains name) then
            myStates[myCurrent] <- {
                state with names = name :: state.names
            }

    /// Returns the first named state equal to or following the current state
    /// or None if there is no such state.
    member this.GetNamedStateForward name =
        myIsLastStateVolatile <- false

        match this.FindNamedStateForward name with
        | Some index ->
            this.SwitchAndGetState index
        | None       ->
            None

    /// Returns the first named state equal to or preceding the current state
    /// or None if there is no such state.
    member this.GetNamedStateBackward name =
        myIsLastStateVolatile <- false

        match this.FindNamedStateBackward name with
        | Some index ->
            this.SwitchAndGetState index
        | None       ->
            None

    /// Removes given selections register from all states.
    member _.RemoveSelectionsRegister register =
        let name = SelectionsRegisters.getRegisterName register

        let newStates = ResizeArray<BufferState> myStates

        let mutable lastWithRemoved = ImmutableArray.Empty

        for i, state in myStates |> Seq.indexed do
            let selsRegisters = state.selsRegisters

            let found = selsRegisters |> Seq.tryFindIndex (
                fun item -> item.name = name
            )

            match found with
            | Some index ->
                let withRemoved = selsRegisters.RemoveAt index

                let newSelsRegisters =
                    if withRemoved <> lastWithRemoved then
                        lastWithRemoved <- withRemoved
                        withRemoved
                    else
                        lastWithRemoved

                newStates[i] <- {
                    state with selsRegisters = newSelsRegisters
                }
            | None       ->
                ()

        myStates.Clear ()
        myStates.AddRange newStates

    member private _.FindNamedStateForward name =
        let fromCurrentUp =
               Seq.init myStates.Count id
            |> Seq.skip myCurrent

        fromCurrentUp |> Seq.tryFind (
            fun i -> myStates[i].names |> List.contains name
        )

    member private _.FindNamedStateBackward name =
        let fromCurrentDown =
               Seq.init myStates.Count id
            |> Seq.take (myCurrent + 1)
            |> Seq.rev

        fromCurrentDown |> Seq.tryFind (
            fun i -> myStates[i].names |> List.contains name
        )

    member private _.SwitchAndGetState newCurrent =
        let oldState = myStates[myCurrent]
        myCurrent <- newCurrent
        let newState = myStates[myCurrent]

        // Don't provide lines if they are the same.
        if areLinesTheSame newState.lines oldState.lines then
            Some { newState with lines = None }
        else
            Some newState

    /// Returns the current state.
    member _.GetCurrentState () =
        myStates[myCurrent]

    /// Sets the current state as saved to the file.
    /// Removes old saved states beyond maxSavedUndos.
    member this.SetCurrentStateAsSaved maxSavedUndos =
        myStates[myCurrent] <- {
            myStates[myCurrent] with wasSaved = true
        }

        mySavedLines <- myStates[myCurrent].lines

        this.RemoveOldStates maxSavedUndos

    member private _.RemoveOldStates savedToRetain =
        let mutable savedUndos = 0
        let mutable i = myCurrent

        while i > 0 && savedUndos < savedToRetain do
            i <- i - 1
            if myStates[i].wasSaved then
                savedUndos <- savedUndos + 1

        myStates.RemoveRange (0, i)
        myCurrent <- myCurrent - i

    /// Returns true if the current state is saved to the file.
    member _.IsCurrentStateSaved =
        areLinesTheSame
            mySavedLines myStates[myCurrent].lines
