module ApplyDeleteToPosTest

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
type ApplyDeleteToPosTest () =
    let myBuffer   = new TextAreaBuffer (contextRef, UserMessages (), Registers (), "")
    let myAccessor = LinesAccessor myBuffer.Lines

    // initialization

    let iterateBack =
        Seq.unfold (
            fun x -> Some (x, myAccessor.GetPrevChar x)
        )

    let getStartingPositions last =
        iterateBack myAccessor.EofPosition
        |> Seq.takeWhile ((<) last)
        |> Seq.toList

    let getExpectedPositions count preservesLastEol =
        iterateBack myAccessor.EofPosition
        |> Seq.skip (if preservesLastEol then 1 else 0)
        |> Seq.take count
        |> Seq.toList

    let init lines anchorLine anchorChar cursorLine cursorChar =
        myBuffer.LoadStrings lines

        let first = { line = anchorLine; char = anchorChar }
        let last  = { line = cursorLine; char = cursorChar }

        myBuffer.Selections.Clear ()
        myBuffer.Selections.Add {
            Selection_Default with
                first = first
                last  = last
                isForward = true
        }

        let startingPositions = getStartingPositions last
        let deleteSpec = myAccessor.GetDeleteSpec first last

        (startingPositions, deleteSpec)

    // command and simple assertions

    let performCommand (command: obj) count =
        match command with
        | :? ModifyingCommand as x ->
            myBuffer.PerformCommand false false (ModifyingCommand x) count
        | _                        ->
            invalidOp ""

    let command command =
        performCommand command 1

    // others

    let toTuple2 (a: 'T array) =
        a[0], a[1]

    let getNewPositions deleteSpec positions =
        positions |> List.map (applyDeleteToPos deleteSpec)

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // applyDeleteToPos ---------------------------------------------------------

    [<TestCase( [|"cd"  ; "xy"|], [|0; 1|], false )>]  // empty line
    [<TestCase( [|"cdab"; "xy"|], [|0; 1|], false )>]  // first char of line
    [<TestCase( [|"acdb"; "xy"|], [|1; 2|], false )>]  // last char of line
    [<TestCase( [|"abcd"; "xy"|], [|2; 3|], false )>]  // EOL
    [<TestCase( [|"cd"        |], [|0; 1|], false )>]  // empty file
    [<TestCase( [|"abcd"      |], [|2; 3|], false )>]  // EOF
    member _.applyDeleteToPos_OneLine start numbers preservesLastEol =
        let anchorChar, cursorChar = toTuple2 numbers

        let positions, deleteSpec =
            init start 0 anchorChar 0 cursorChar

        command Delete

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count preservesLastEol
        let newPositions      = positions |> getNewPositions deleteSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|"cd"  ; "ef"  ; "xy"|], [|0; 1|], false )>]
    [<TestCase( [|"cd"  ; "efab"; "xy"|], [|0; 1|], false )>]
    [<TestCase( [|"acd" ; "efb" ; "xy"|], [|1; 1|], false )>]
    [<TestCase( [|"abcd"; "ef"  ; "xy"|], [|2; 1|], false )>]
    [<TestCase( [|"cd"  ; "ef"        |], [|0; 1|], false )>]
    [<TestCase( [|"abcd"; "ef"        |], [|2; 1|], false )>]
    member _.applyDeleteToPos_TwoLines start numbers preservesLastEol =
        let anchorChar, cursorChar = toTuple2 numbers

        let positions, deleteSpec =
            init start 0 anchorChar 1 cursorChar

        command Delete

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count preservesLastEol
        let newPositions      = positions |> getNewPositions deleteSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|"cd"  ; "ef"; "gh"  ; "xy"|], [|0; 1|], false )>]
    [<TestCase( [|"cd"  ; "ef"; "ghab"; "xy"|], [|0; 1|], false )>]
    [<TestCase( [|"acd" ; "ef"; "ghb" ; "xy"|], [|1; 1|], false )>]
    [<TestCase( [|"abcd"; "ef"; "gh"  ; "xy"|], [|2; 1|], false )>]
    [<TestCase( [|"cd"  ; "ef"; "gh"        |], [|0; 1|], false )>]
    [<TestCase( [|"abcd"; "ef"; "gh"        |], [|2; 1|], false )>]
    member _.applyDeleteToPos_ThreeLines start numbers preservesLastEol =
        let anchorChar, cursorChar = toTuple2 numbers

        let positions, deleteSpec =
            init start 0 anchorChar 2 cursorChar

        command Delete

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count preservesLastEol
        let newPositions      = positions |> getNewPositions deleteSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|""  ; ""  ; "xy"|], [|0; 0|], false )>]
    [<TestCase( [|""  ; "ab"; "xy"|], [|0; 0|], false )>]
    [<TestCase( [|"a" ; "b" ; "xy"|], [|1; 1|], false )>]
    [<TestCase( [|"ab"; ""  ; "xy"|], [|2; 2|], false )>]
    [<TestCase( [|""              |], [|0; 0|], true  )>]
    [<TestCase( [|"ab"            |], [|2; 2|], true  )>]
    member _.applyDeleteToPos_NewLine start numbers preservesLastEol =
        let anchorChar, cursorChar = toTuple2 numbers

        let positions, deleteSpec =
            init start 0 anchorChar 0 cursorChar

        command Delete

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count preservesLastEol
        let newPositions      = positions |> getNewPositions deleteSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|""  ; "cd"  ; "xy"|], [|0; 1|], false )>]
    [<TestCase( [|""  ; "cdab"; "xy"|], [|0; 1|], false )>]
    [<TestCase( [|"a" ; "cdb" ; "xy"|], [|1; 1|], false )>]
    [<TestCase( [|"ab"; "cd"  ; "xy"|], [|2; 1|], false )>]
    [<TestCase( [|""  ; "cd"        |], [|0; 1|], false )>]
    [<TestCase( [|"ab"; "cd"        |], [|2; 1|], false )>]
    member _.applyDeleteToPos_NewLineAndOneLine start numbers preservesLastEol =
        let anchorChar, cursorChar = toTuple2 numbers

        let positions, deleteSpec =
            init start 0 anchorChar 1 cursorChar

        command Delete

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count preservesLastEol
        let newPositions      = positions |> getNewPositions deleteSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|"cd"  ; ""  ; "xy"|], [|0; 2|], false )>]
    [<TestCase( [|"cd"  ; "ab"; "xy"|], [|0; 2|], false )>]
    [<TestCase( [|"acd" ; "b" ; "xy"|], [|1; 3|], false )>]
    [<TestCase( [|"abcd"; ""  ; "xy"|], [|2; 4|], false )>]
    [<TestCase( [|"cd"              |], [|0; 2|], true  )>]
    [<TestCase( [|"abcd"            |], [|2; 4|], true  )>]
    member _.applyDeleteToPos_OneLineAndNewLine start numbers preservesLastEol =
        let anchorChar, cursorChar = toTuple2 numbers

        let positions, deleteSpec =
            init start 0 anchorChar 0 cursorChar

        command Delete

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count preservesLastEol
        let newPositions      = positions |> getNewPositions deleteSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|""  ; "cd"; "ef"  ; "xy"|], [|0; 1|], false )>]
    [<TestCase( [|""  ; "cd"; "efab"; "xy"|], [|0; 1|], false )>]
    [<TestCase( [|"a" ; "cd"; "efb" ; "xy"|], [|1; 1|], false )>]
    [<TestCase( [|"ab"; "cd"; "ef"  ; "xy"|], [|2; 1|], false )>]
    [<TestCase( [|""  ; "cd"; "ef"        |], [|0; 1|], false )>]
    [<TestCase( [|"ab"; "cd"; "ef"        |], [|2; 1|], false )>]
    member _.applyDeleteToPos_NewLineAndTwoLines start numbers preservesLastEol =
        let anchorChar, cursorChar = toTuple2 numbers

        let positions, deleteSpec =
            init start 0 anchorChar 2 cursorChar

        command Delete

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count preservesLastEol
        let newPositions      = positions |> getNewPositions deleteSpec

        Assert.AreEqual (expectedPositions, newPositions)

    [<TestCase( [|"cd"  ; "ef"; ""  ; "xy"|], [|0; 2|], false )>]
    [<TestCase( [|"cd"  ; "ef"; "ab"; "xy"|], [|0; 2|], false )>]
    [<TestCase( [|"acd" ; "ef"; "b" ; "xy"|], [|1; 2|], false )>]
    [<TestCase( [|"abcd"; "ef"; ""  ; "xy"|], [|2; 2|], false )>]
    [<TestCase( [|"cd"  ; "ef"            |], [|0; 2|], true  )>]
    [<TestCase( [|"abcd"; "ef"            |], [|2; 2|], true  )>]
    member _.applyDeleteToPos_TwoLinesAndNewLine start numbers preservesLastEol =
        let anchorChar, cursorChar = toTuple2 numbers

        let positions, deleteSpec =
            init start 0 anchorChar 1 cursorChar

        command Delete

        let count             = positions.Length
        let expectedPositions = getExpectedPositions count preservesLastEol
        let newPositions      = positions |> getNewPositions deleteSpec

        Assert.AreEqual (expectedPositions, newPositions)
