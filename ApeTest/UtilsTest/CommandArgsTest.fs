module CommandArgsTest

open NUnit.Framework

open CommandArgs
open StringInCompl

let private StringInCompl_NotQuoted orig = {
    quoteType = NotQuoted
    orig      = orig
    unescaped = orig
}

let private StringInCompl_Quoted orig unescaped = {
    quoteType = Quoted
    orig      = orig
    unescaped = unescaped
}

let private StringInCompl_AtQuoted orig unescaped = {
    quoteType = AtQuoted
    orig      = orig
    unescaped = unescaped
}

[<TestFixture>]
type CommandArgsTest () =

    // getArgsMapRight ---------------------------------------------------------

    member _.inputs_getArgsMapRight_Ok = [|
        ""              , 0 , [|"x"; "y"; "z"|]
        "a"             , 0 , [|"x"; "y"; "z"|]
        "a b"           , 0 , [|"x"; "y"; "z"|]
        "a b c"         , 0 , [|"x"; "y"; "z"|]
        "  "            , 0 , [|"x"; "y"; "z"|]
        "  a  "         , 0 , [|"x"; "y"; "z"|]
        "  a  b  "      , 0 , [|"x"; "y"; "z"|]
        "  a  b  c  "   , 0 , [|"x"; "y"; "z"|]

        "a"             , 1 , [|"x"; "y"; "z"|]
        "a b"           , 1 , [|"x"; "y"; "z"|]
        "a b c"         , 1 , [|"x"; "y"; "z"|]
        "  a  "         , 1 , [|"x"; "y"; "z"|]
        "  a  b  "      , 1 , [|"x"; "y"; "z"|]
        "  a  b  c  "   , 1 , [|"x"; "y"; "z"|]

        "a b"           , 2 , [|"x"; "y"; "z"|]
        "a b c"         , 2 , [|"x"; "y"; "z"|]
        "  a  b  "      , 2 , [|"x"; "y"; "z"|]
        "  a  b  c  "   , 2 , [|"x"; "y"; "z"|]

        "a b c"         , 3 , [|"x"; "y"; "z"|]
        "  a  b  c  "   , 3 , [|"x"; "y"; "z"|]

        "a \"b\" c"     , 3 , [|"x"; "y"; "z"|]
        "a \"\\\\\" c"  , 3 , [|"x"; "y"; "z"|]
        "a \"\\\"\" c"  , 3 , [|"x"; "y"; "z"|]
        "a \"\\t\" c"   , 3 , [|"x"; "y"; "z"|]

        "a @\"b\" c"    , 3 , [|"x"; "y"; "z"|]
        "a @\"\"\"\" c" , 3 , [|"x"; "y"; "z"|]
    |]

    member _.expected_getArgsMapRight_Ok: ArgsMapResult array = [|
        Ok (Map [| ("x", None    ); ("y", None     ); ("z", None    ) |])
        Ok (Map [| ("x", None    ); ("y", None     ); ("z", Some "a") |])
        Ok (Map [| ("x", None    ); ("y", Some "a" ); ("z", Some "b") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "b" ); ("z", Some "c") |])
        Ok (Map [| ("x", None    ); ("y", None     ); ("z", None    ) |])
        Ok (Map [| ("x", None    ); ("y", None     ); ("z", Some "a") |])
        Ok (Map [| ("x", None    ); ("y", Some "a" ); ("z", Some "b") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "b" ); ("z", Some "c") |])

        Ok (Map [| ("x", None    ); ("y", None     ); ("z", Some "a") |])
        Ok (Map [| ("x", None    ); ("y", Some "a" ); ("z", Some "b") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "b" ); ("z", Some "c") |])
        Ok (Map [| ("x", None    ); ("y", None     ); ("z", Some "a") |])
        Ok (Map [| ("x", None    ); ("y", Some "a" ); ("z", Some "b") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "b" ); ("z", Some "c") |])

        Ok (Map [| ("x", None    ); ("y", Some "a" ); ("z", Some "b") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "b" ); ("z", Some "c") |])
        Ok (Map [| ("x", None    ); ("y", Some "a" ); ("z", Some "b") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "b" ); ("z", Some "c") |])

        Ok (Map [| ("x", Some "a"); ("y", Some "b" ); ("z", Some "c") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "b" ); ("z", Some "c") |])

        Ok (Map [| ("x", Some "a"); ("y", Some "b" ); ("z", Some "c") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "\\"); ("z", Some "c") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "\""); ("z", Some "c") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "\t"); ("z", Some "c") |])

        Ok (Map [| ("x", Some "a"); ("y", Some "b" ); ("z", Some "c") |])
        Ok (Map [| ("x", Some "a"); ("y", Some "\""); ("z", Some "c") |])
    |]

    [<Test>]
    member this.getArgsMapRight_Ok ([<Range(0, 25)>] index) =
        let args, mandatoryCount, names =
            this.inputs_getArgsMapRight_Ok[index]
        let expected =
            this.expected_getArgsMapRight_Ok[index]

        let actual = getArgsMapRight args mandatoryCount names

        Assert.AreEqual (expected, actual)

    member _.inputs_getArgsMapRight_Error = [|
        ""             , 1 , [|"x"; "y"|]
        "  "           , 1 , [|"x"; "y"|]

        "a b c"        , 1 , [|"x"; "y"|]
        "  a  b  c  "  , 1 , [|"x"; "y"|]

        "\"\\a\""      , 1 , [|"x"; "y"|]

        "\"a"          , 1 , [|"x"; "y"|]
        "\"a\"b"       , 1 , [|"x"; "y"|]
        "\"a\"\"b\""   , 1 , [|"x"; "y"|]
        "\"\\\""       , 1 , [|"x"; "y"|]

        "@\"a"         , 1 , [|"x"; "y"|]
        "@\"a\"b"      , 1 , [|"x"; "y"|]
        "@\"a\"@\"b\"" , 1 , [|"x"; "y"|]
        "@\"\"\""      , 1 , [|"x"; "y"|]
    |]

    member _.expected_getArgsMapRight_Error: ArgsMapResult array = [|
        Error $"{NOT_ENOUGH_ARGUMENTS}: ''"
        Error $"{NOT_ENOUGH_ARGUMENTS}: '  '"

        Error $"{TOO_MANY_ARGUMENTS}: 'a b c'"
        Error $"{TOO_MANY_ARGUMENTS}: '  a  b  c  '"

        Error $"{ARGUMENTS_PARSING_ERROR}: '\"\\a\"'"

        Error $"{ARGUMENTS_PARSING_ERROR}: '\"a'"
        Error $"{ARGUMENTS_PARSING_ERROR}: '\"a\"b'"
        Error $"{ARGUMENTS_PARSING_ERROR}: '\"a\"\"b\"'"
        Error $"{ARGUMENTS_PARSING_ERROR}: '\"\\\"'"

        Error $"{ARGUMENTS_PARSING_ERROR}: '@\"a'"
        Error $"{ARGUMENTS_PARSING_ERROR}: '@\"a\"b'"
        Error $"{ARGUMENTS_PARSING_ERROR}: '@\"a\"@\"b\"'"
        Error $"{ARGUMENTS_PARSING_ERROR}: '@\"\"\"'"
    |]

    [<Test>]
    member this.getArgsMapRight_Error ([<Range(0, 12)>] index) =
        let args, mandatoryCount, names =
            this.inputs_getArgsMapRight_Error[index]
        let expected =
            this.expected_getArgsMapRight_Error[index]

        let actual = getArgsMapRight args mandatoryCount names

        Assert.AreEqual (expected, actual)

    // getArgsForCompl ---------------------------------------------------------

    member _.inputs_getArgsForCompl_Ok = [|
        ""              , 4
        "a"             , 4
        "a b"           , 4
        "a b c"         , 4
        "  "            , 4
        "  a  "         , 4
        "  a  b  "      , 4
        "  a  b  c  "   , 4

        "a \"b\" c"     , 4
        "a \"\\\\\" c"  , 4
        "a \"\\\"\" c"  , 4
        "a \"\\t\" c"   , 4

        "a @\"b\" c"    , 4
        "a @\"\"\"\" c" , 4

        "\"a"           , 4
        "\"\\\""        , 4
        "@\"a"          , 4
        "@\"\"\""       , 4
    |]

    member _.expected_getArgsForCompl_Ok: ArgsForComplResult array = [|
        Ok ([|             |], StringInCompl_NotQuoted ""  )
        Ok ([|             |], StringInCompl_NotQuoted "a" )
        Ok ([|"a"          |], StringInCompl_NotQuoted "b" )
        Ok ([|"a"; "b"     |], StringInCompl_NotQuoted "c" )
        Ok ([|             |], StringInCompl_NotQuoted ""  )
        Ok ([|"a"          |], StringInCompl_NotQuoted ""  )
        Ok ([|"a"; "b"     |], StringInCompl_NotQuoted ""  )
        Ok ([|"a"; "b"; "c"|], StringInCompl_NotQuoted ""  )

        Ok ([|"a"; "b"     |], StringInCompl_NotQuoted "c" )
        Ok ([|"a"; "\\"    |], StringInCompl_NotQuoted "c" )
        Ok ([|"a"; "\""    |], StringInCompl_NotQuoted "c" )
        Ok ([|"a"; "\t"    |], StringInCompl_NotQuoted "c" )

        Ok ([|"a"; "b"     |], StringInCompl_NotQuoted "c" )
        Ok ([|"a"; "\""    |], StringInCompl_NotQuoted "c" )

        Ok ([|             |], StringInCompl_Quoted    "\"a"     "a" )
        Ok ([|             |], StringInCompl_Quoted    "\"\\\""  "\"")
        Ok ([|             |], StringInCompl_AtQuoted  "@\"a"    "a" )
        Ok ([|             |], StringInCompl_AtQuoted  "@\"\"\"" "\"")
    |]

    [<Test>]
    member this.getArgsForCompl_Ok ([<Range(0, 17)>] index) =
        let args, maxCount =
            this.inputs_getArgsForCompl_Ok[index]
        let expected =
            this.expected_getArgsForCompl_Ok[index]

        let actual = getArgsForCompl args maxCount

        Assert.AreEqual (expected, actual)

    member _.inputs_getArgsForCompl_Error = [|
        "a b c"        , 2
        "  a  b  c  "  , 3

        "\"a\"b"       , 3
        "\"a\"\"b\""   , 3
        "\"\\a\""      , 3

        "@\"a\"b"      , 3
        "@\"a\"@\"b\"" , 3
    |]

    member _.expected_getArgsForCompl_Error: ArgsForComplResult array = [|
        Error $"{TOO_MANY_ARGUMENTS}: 'a b c'"
        Error $"{TOO_MANY_ARGUMENTS}: '  a  b  c  '"

        Error $"{ARGUMENTS_PARSING_ERROR}: '\"a\"b'"
        Error $"{ARGUMENTS_PARSING_ERROR}: '\"a\"\"b\"'"
        Error $"{ARGUMENTS_PARSING_ERROR}: '\"\\a\"'"

        Error $"{ARGUMENTS_PARSING_ERROR}: '@\"a\"b'"
        Error $"{ARGUMENTS_PARSING_ERROR}: '@\"a\"@\"b\"'"
    |]

    [<Test>]
    member this.getArgsForCompl_Error ([<Range(0, 6)>] index) =
        let args, maxCount =
            this.inputs_getArgsForCompl_Error[index]
        let expected =
            this.expected_getArgsForCompl_Error[index]

        let actual = getArgsForCompl args maxCount

        Assert.AreEqual (expected, actual)
