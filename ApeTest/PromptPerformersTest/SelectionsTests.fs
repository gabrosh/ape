module PromptSelectionsTests

open NUnit.Framework
open System

open Commands.InCommands
open Registers
open Selection
open PromptBuffer
open UserMessages

let contextRef      = TestUtils.makeContextRef 80 25
let extraContextRef = TestUtils.makeExtraContextRef 1

[<TestFixture>]
type SelectionsTest () =
    let myBuffer = new PromptBuffer (contextRef, extraContextRef, UserMessages (), Registers ())

    // initialization

    let initWithDirection
        isForward cursorBeforeEol line firstLine firstChar lastLine lastChar =

        myBuffer.LoadString line

        TestUtils.set_cursorBeforeEol contextRef cursorBeforeEol

        myBuffer.Selections.Clear ()

        myBuffer.Selections.Add {
            Selection_Default with
                first     = { line = firstLine; char = firstChar }
                last      = { line = lastLine ; char = lastChar  }
                firstWC   = { hard = firstChar; soft = firstChar }
                lastWC    = { hard = lastChar ; soft = lastChar  }
                isForward = isForward
        }

    let init         = initWithDirection true
    let initBackward = initWithDirection false

    let toTuple2 (a: 'T array) =
        a[0], a[1]

    let toTuple4 (a: 'T array) =
        a[0], a[1], a[2], a[3]

    // command and simple assertions

    let performCommand (command: obj) =
        match command with
        | :? SelectionsCommand as x ->
            myBuffer.PerformCommand false false (SelectionsCommand x)
        | _                         ->
            invalidOp ""

    let command command =
        performCommand command

    let assertFirst (selection: Selection) anchorLine anchorChar =
        Assert.AreEqual (
            ( anchorLine           , anchorChar           ),
            ( selection.first.line , selection.first.char )
        )

    let assertLast (selection: Selection) cursorLine cursorChar =
        Assert.AreEqual (
            ( cursorLine           , cursorChar           ),
            ( selection.last.line  , selection.last.char  )
        )

    let assertDirection (selection: Selection) isForward =
        Assert.AreEqual (selection.isForward, isForward)

    let assertMainIndex mainIndex =
        Assert.AreEqual (mainIndex, myBuffer.Selections.MainIndex)

    // assertions

    let assertSelectionWithDirection
        isForward firstLine firstChar lastLine lastChar =

        assertFirst     myBuffer.Main firstLine firstChar
        assertLast      myBuffer.Main lastLine  lastChar
        assertDirection myBuffer.Main isForward

    let assertSelection = assertSelectionWithDirection true

    // commands followed by assertion(s)

    let commandAssertMainIndex command mainIndex =
        performCommand command
        assertMainIndex mainIndex

    // teardown

    [<OneTimeTearDown>]
    member _.OneTimeTearDown () =
      (myBuffer :> IDisposable).Dispose ()

    // SelectWholeBuffer -------------------------------------------------------

    [<Test>]
    member _.SelectWholeBuffer () =
        init false "aa"
            0 1 0 1

        command SelectWholeBuffer

        assertSelection
            0 0 0 2

    // ExpandToFullLines ------------------------------------------------------

    [<TestCase( [|0; 0|] )>]
    [<TestCase( [|0; 2|] )>]
    [<TestCase( [|2; 2|] )>]
    member _.ExpandToFullLines_OneLine chars =
        let anchorChar, cursorChar =
            toTuple2 chars

        init false "aa"
            0 anchorChar 0 cursorChar

        command ExpandToFullLines

        assertSelection
            0 0 0 2

    // ReduceToCursor ---------------------------------------------------------

    [<TestCase( [|0; 0|] )>]
    [<TestCase( [|0; 2|] )>]
    [<TestCase( [|2; 2|] )>]
    member _.ReduceToCursor_Forward chars =
        let anchorChar, cursorChar =
            toTuple2 chars

        init false "aa"
            0 anchorChar 0 cursorChar

        command ReduceToCursor

        assertSelection
            0 cursorChar 0 cursorChar

    [<TestCase( [|0; 0|] )>]
    [<TestCase( [|0; 2|] )>]
    [<TestCase( [|2; 2|] )>]
    member _.ReduceToCursor_Backward chars =
        let cursorChar, anchorChar =
            toTuple2 chars

        initBackward false "aa"
            0 cursorChar 0 anchorChar

        command ReduceToCursor

        assertSelectionWithDirection false
            0 cursorChar 0 cursorChar

    // ForwardDirection -------------------------------------------------------

    [<TestCase( true , true, [|0; 0|] )>]
    [<TestCase( true , true, [|0; 2|] )>]
    [<TestCase( true , true, [|2; 2|] )>]
    [<TestCase( false, true, [|0; 0|] )>]
    [<TestCase( false, true, [|0; 2|] )>]
    [<TestCase( false, true, [|2; 2|] )>]
    member _.ForwardDirection isForwardStart isForwardEnd chars =
        let firstChar, lastChar =
            toTuple2 chars

        initWithDirection isForwardStart false "aa"
            0 firstChar 0 lastChar

        command ForwardDirection

        assertSelectionWithDirection isForwardEnd
            0 firstChar 0 lastChar

    // FlipDirection ----------------------------------------------------------

    [<TestCase( true , false, [|0; 0|] )>]
    [<TestCase( true , false, [|0; 2|] )>]
    [<TestCase( true , false, [|2; 2|] )>]
    [<TestCase( false, true , [|0; 0|] )>]
    [<TestCase( false, true , [|0; 2|] )>]
    [<TestCase( false, true , [|2; 2|] )>]
    member _.FlipDirection isForwardStart isForwardEnd chars =
        let firstChar, lastChar =
            toTuple2 chars

        initWithDirection isForwardStart false "aa"
            0 firstChar 0 lastChar

        command FlipDirection

        assertSelectionWithDirection isForwardEnd
            0 firstChar 0 lastChar
