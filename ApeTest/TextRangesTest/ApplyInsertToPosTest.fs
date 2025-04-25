module ApplyInsertToPosTest

open NUnit.Framework
open System

open Commands.InCommands
open DataTypes
open LinesAccessor
open Position
open Registers
open Selection
open TextAreaBuffer
open UserMessages

let contextRef = TestUtils.makeContextRef 80 25

let defReg = DefaultRegister

[<TestFixture>]
type ApplyInsertToPosTest () =
    let myRegisters = Registers ()
    let myBuffer    = new TextAreaBuffer (contextRef, UserMessages (), myRegisters, "")
    let myAccessor  = LinesAccessor myBuffer.Lines

    // initialization

    let setRegister lines =
        myRegisters.ApplyToSlot DefaultRegister 0 lines

    let iterateBack =
        Seq.unfold (
            fun x -> Some (x, myAccessor.GetPrevChar x)
        )

    let getStartingPositions first preservesLastEol =
        iterateBack myAccessor.EofPosition
        |> Seq.skip (if preservesLastEol then 1 else 0)
        |> Seq.takeWhile ((<=) first)
        |> Seq.toList

    let getExpectedPositions count =
        iterateBack myAccessor.EofPosition
        |> Seq.take count
        |> Seq.toList

    let init lines startLine startChar preservesLastEol insertedLines =
        myBuffer.LoadStrings lines

        let first = { line = startLine; char = startChar }

        myBuffer.Selections.Clear ()
        myBuffer.Selections.Add {
            Selection_Default with
                first = first
                last  = first
                isForward = true
        }

        let insertedLines = Lines (insertedLines |> Seq.map stringToChars)

        setRegister insertedLines

        let startingPositions = getStartingPositions first preservesLastEol
        let deleteSpec = myAccessor.GetInsertSpec first insertedLines true

        (startingPositions, deleteSpec)

    // command and simple assertions

    let performCommand (command: obj) count =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand true false (ModifyingCommand x) count
        | _                        ->
            invalidOp ""

    let command command =
        performCommand command 1

    // others

    let toTuple3 (a: 'T array) =
        a[0], a[1], a[2]

    let getNewPositions insertSpec positions =
        positions |> List.map (applyInsertToPos insertSpec)

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // applyInsertToPos --------------------------------------------------------

    [<TestCase( [|""  ; "xy"|], 0, false )>]  // empty line
    [<TestCase( [|"ab"; "xy"|], 0, false )>]  // first char of line
    [<TestCase( [|"ab"; "xy"|], 1, false )>]  // last char of line
    [<TestCase( [|"ab"; "xy"|], 2, false )>]  // EOL
    [<TestCase( [|""        |], 0, false )>]  // empty file
    [<TestCase( [|"ab"      |], 2, false )>]  // EOF
    member _.applyInsertToPos_OneLine start startChar preservesLastEol =
        let positions, insertSpec =
            init start 0 startChar preservesLastEol ["cd"]

        command (PasteBefore defReg)

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count
        let newPositions      = positions |> getNewPositions insertSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|""  ; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 1, false )>]
    [<TestCase( [|"ab"; "xy"|], 2, false )>]
    [<TestCase( [|""        |], 0, false )>]
    [<TestCase( [|"ab"      |], 2, false )>]
    member _.applyInsertToPos_TwoLines start startChar preservesLastEol =
        let positions, insertSpec =
            init start 0 startChar preservesLastEol ["cd"; "ef"]

        command (PasteBefore defReg)

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count
        let newPositions      = positions |> getNewPositions insertSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|""  ; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 1, false )>]
    [<TestCase( [|"ab"; "xy"|], 2, false )>]
    [<TestCase( [|""        |], 0, false )>]
    [<TestCase( [|"ab"      |], 2, false )>]
    member _.applyInsertToPos_ThreeLines start startChar preservesLastEol =
        let positions, insertSpec =
            init start 0 startChar preservesLastEol ["cd"; "ef"; "gh"]

        command (PasteBefore defReg)

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count
        let newPositions      = positions |> getNewPositions insertSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|""  ; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 1, false )>]
    [<TestCase( [|"ab"; "xy"|], 2, false )>]
    [<TestCase( [|""        |], 0, true  )>]
    [<TestCase( [|"ab"      |], 2, true  )>]
    member _.applyInsertToPos_NewLine start startChar preservesLastEol =
        let positions, insertSpec =
            init start 0 startChar preservesLastEol [""; ""]

        command (PasteBefore defReg)

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count
        let newPositions      = positions |> getNewPositions insertSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|""  ; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 1, false )>]
    [<TestCase( [|"ab"; "xy"|], 2, false )>]
    [<TestCase( [|""        |], 0, false )>]
    [<TestCase( [|"ab"      |], 2, false )>]
    member _.applyInsertToPos_NewLineAndOneLine start startChar preservesLastEol =
        let positions, insertSpec =
            init start 0 startChar preservesLastEol [""; "cd"]

        command (PasteBefore defReg)

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count
        let newPositions      = positions |> getNewPositions insertSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|""  ; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 1, false )>]
    [<TestCase( [|"ab"; "xy"|], 2, false )>]
    [<TestCase( [|""        |], 0, true  )>]
    [<TestCase( [|"ab"      |], 2, true  )>]
    member _.applyInsertToPos_OneLineAndNewLine start startChar preservesLastEol =
        let positions, insertSpec =
            init start 0 startChar preservesLastEol ["cd"; ""]

        command (PasteBefore defReg)

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count
        let newPositions      = positions |> getNewPositions insertSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|""  ; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 1, false )>]
    [<TestCase( [|"ab"; "xy"|], 2, false )>]
    [<TestCase( [|""        |], 0, false )>]
    [<TestCase( [|"ab"      |], 2, false )>]
    member _.applyInsertToPos_NewLineAndTwoLines start startChar preservesLastEol =
        let positions, insertSpec =
            init start 0 startChar preservesLastEol [""; "cd"; "ef"]

        command (PasteBefore defReg)

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count
        let newPositions      = positions |> getNewPositions insertSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|""  ; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 0, false )>]
    [<TestCase( [|"ab"; "xy"|], 1, false )>]
    [<TestCase( [|"ab"; "xy"|], 2, false )>]
    [<TestCase( [|""        |], 0, true  )>]
    [<TestCase( [|"ab"      |], 2, true  )>]
    member _.applyInsertToPos_TwoLinesAndNewLine start startChar preservesLastEol =
        let positions, insertSpec =
            init start 0 startChar preservesLastEol ["cd"; "ef"; ""]

        command (PasteBefore defReg)

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count
        let newPositions      = positions |> getNewPositions insertSpec

        Assert.AreEqual (expectedPositions, newPositions)
