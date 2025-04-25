module ConsoleInputSource

open System
open System.Collections.Concurrent
open System.Threading

open ConsoleKeys
open ConsoleInterop_Common
open ConsoleInterop_Specific

let private windowSizePollingInterval = 100  // ms

type ConsoleInput =
    | KeyboardInputRead of Key
    | WindowSizeChanged of WindowSize

/// ConsoleInputSource provides console input - either the next input key
/// or the new console window size after resizing the console window.

type ConsoleInputSource () as this_ =
    let myWindowSizeLock     = Object ()
    let mutable myWindowSize = consoleInterop.GetConsoleWindowSize ()

    let myQueue = new BlockingCollection<ConsoleInput> ()

    let myInputKeyThread   = Thread this_.InputKeyWorker
    let myWindowSizeThread = Thread this_.WindowSizeWorker

    do
        myInputKeyThread.IsBackground <- true
        myInputKeyThread.Start ()

        myWindowSizeThread.IsBackground <- true
        myWindowSizeThread.Start ()

    /// Returns the current size of the console window.
    member _.GetWindowSize () =
        let mutable lockWasTaken = false
        try
            Monitor.Enter (myWindowSizeLock, &lockWasTaken)
            myWindowSize
        finally
            if lockWasTaken then
                Monitor.Exit myWindowSizeLock

    /// Takes one ConsoleInput item from the internal queue.
    member _.TakeInput () =
        myQueue.Take ()

    member private _.InputKeyWorker () =
        while true do
            let key = keyInfoToKey (Console.ReadKey true) (consoleInterop.GetCapsLock ())
            if key <> CharNoModif '\000' then
                myQueue.Add (KeyboardInputRead key)

    member private this.WindowSizeWorker () =
        while true do
            match this.GetWindowSizeIfChanged () with
            | Some windowSize ->
                myQueue.Add (WindowSizeChanged windowSize)
            | None ->
                ()

            Thread.Sleep windowSizePollingInterval

    member private _.GetWindowSizeIfChanged () =
        let windowSize = consoleInterop.GetConsoleWindowSize ()

        let mutable lockWasTaken = false
        try
            Monitor.Enter (myWindowSizeLock, &lockWasTaken)
            if windowSize = myWindowSize then
                None
            else
                myWindowSize <- windowSize
                Some myWindowSize
        finally
            if lockWasTaken then
                Monitor.Exit myWindowSizeLock
