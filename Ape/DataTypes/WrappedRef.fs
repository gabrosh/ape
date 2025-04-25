module WrappedRef

open System

// wrapped references

type IWrappedRef<'T> =
    /// Subscribes f as a handler for "wrapped value changed" event.
    abstract member Subscribe:
        f: (unit -> unit) -> IDisposable

    /// Returns wrapped value.
    abstract member Value: 'T
        with get

type WrappedRef<'T when 'T: equality> (value: 'T) =
    let mutable myValue = value

    let myChanged = Event<unit> ()

    member _.Value
        with get ()    = myValue
        and  set value =
            if value <> myValue then
                myValue <- value
                myChanged.Trigger ()

    interface IWrappedRef<'T> with
        member _.Subscribe (f: unit -> unit) =
            myChanged.Publish.Subscribe f

        member _.Value
            with get () = myValue

[<Sealed>]
type WrappedRef2<'T> (wrappedRef: IWrappedRef<'T>) =
    let mutable myWrappedRef = wrappedRef

    let myChanged = Event<unit> ()

    let handleChanged () = myChanged.Trigger ()

    let mutable myChangedDisposable = myWrappedRef.Subscribe handleChanged

    /// Sets wrappedRef as being wrapped within this instance.
    /// Unsubscribes the instance from the events from the old wrappedRef.
    /// Subscribes the instance for the events from the new wrappedRef.
    member _.WrappedRef
        with set value =
            if not (Object.ReferenceEquals (value, myWrappedRef)) then
                myChangedDisposable.Dispose ()
                myWrappedRef <- value
                myChangedDisposable <- myWrappedRef.Subscribe handleChanged
                myChanged.Trigger ()

    interface IWrappedRef<'T> with
        member _.Subscribe (f: unit -> unit) =
            myChanged.Publish.Subscribe f

        member _.Value
            with get () = myWrappedRef.Value

    interface IDisposable with
        member _.Dispose () =
            myChangedDisposable.Dispose ()
