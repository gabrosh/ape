module CommandArgsTest

open NUnit.Framework

open CommandArgs

[<TestFixture>]
type CommandArgsTest () =

    // getArgsMapRight ---------------------------------------------------------

    member _.inputs_getArgsMapRight_Ok = [|
        Some ""              , 0 , [|"x"; "y"; "z"|]
        Some "a"             , 0 , [|"x"; "y"; "z"|]
        Some "a b"           , 0 , [|"x"; "y"; "z"|]
        Some "a b c"         , 0 , [|"x"; "y"; "z"|]
        Some "  "            , 0 , [|"x"; "y"; "z"|]
        Some "  a  "         , 0 , [|"x"; "y"; "z"|]
        Some "  a  b  "      , 0 , [|"x"; "y"; "z"|]
        Some "  a  b  c  "   , 0 , [|"x"; "y"; "z"|]

        Some "a"             , 1 , [|"x"; "y"; "z"|]
        Some "a b"           , 1 , [|"x"; "y"; "z"|]
        Some "a b c"         , 1 , [|"x"; "y"; "z"|]
        Some "  a  "         , 1 , [|"x"; "y"; "z"|]
        Some "  a  b  "      , 1 , [|"x"; "y"; "z"|]
        Some "  a  b  c  "   , 1 , [|"x"; "y"; "z"|]

        Some "a b"           , 2 , [|"x"; "y"; "z"|]
        Some "a b c"         , 2 , [|"x"; "y"; "z"|]
        Some "  a  b  "      , 2 , [|"x"; "y"; "z"|]
        Some "  a  b  c  "   , 2 , [|"x"; "y"; "z"|]

        Some "a b c"         , 3 , [|"x"; "y"; "z"|]
        Some "  a  b  c  "   , 3 , [|"x"; "y"; "z"|]

        Some "a \"b\" c"     , 3 , [|"x"; "y"; "z"|]
        Some "a \"\\\\\" c"  , 3 , [|"x"; "y"; "z"|]
        Some "a \"\\\"\" c"  , 3 , [|"x"; "y"; "z"|]
        Some "a \"\\t\" c"   , 3 , [|"x"; "y"; "z"|]

        Some "a @\"b\" c"    , 3 , [|"x"; "y"; "z"|]
        Some "a @\"\"\"\" c" , 3 , [|"x"; "y"; "z"|]
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
        Some ""             , 1 , [|"x"; "y"|]
        Some "  "           , 1 , [|"x"; "y"|]

        Some "a b c"        , 1 , [|"x"; "y"|]
        Some "  a  b  c  "  , 1 , [|"x"; "y"|]

        Some "\"\\a\""      , 1 , [|"x"; "y"|]

        Some "\"a"          , 1 , [|"x"; "y"|]
        Some "\"a\"b"       , 1 , [|"x"; "y"|]
        Some "\"a\"\"b\""   , 1 , [|"x"; "y"|]
        Some "\"\\\""       , 1 , [|"x"; "y"|]

        Some "@\"a"         , 1 , [|"x"; "y"|]
        Some "@\"a\"b"      , 1 , [|"x"; "y"|]
        Some "@\"a\"@\"b\"" , 1 , [|"x"; "y"|]
        Some "@\"\"\""      , 1 , [|"x"; "y"|]
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
        Some ""              , 4
        Some "a"             , 4
        Some "a b"           , 4
        Some "a b c"         , 4
        Some "  "            , 4
        Some "  a  "         , 4
        Some "  a  b  "      , 4
        Some "  a  b  c  "   , 4

        Some "a \"b\" c"     , 4
        Some "a \"\\\\\" c"  , 4
        Some "a \"\\\"\" c"  , 4
        Some "a \"\\t\" c"   , 4

        Some "a @\"b\" c"    , 4
        Some "a @\"\"\"\" c" , 4

        Some "\"a"           , 4
        Some "\"\\\""        , 4
        Some "@\"a"          , 4
        Some "@\"\"\""       , 4
    |]

    member _.expected_getArgsForCompl_Ok: ArgsForComplResult array = [|
        Ok ([|             |], ""  )
        Ok ([|             |], "a" )
        Ok ([|"a"          |], "b" )
        Ok ([|"a"; "b"     |], "c" )
        Ok ([|             |], ""  )
        Ok ([|"a"          |], ""  )
        Ok ([|"a"; "b"     |], ""  )
        Ok ([|"a"; "b"; "c"|], ""  )
                                   
        Ok ([|"a"; "b"     |], "c" )
        Ok ([|"a"; "\\"    |], "c" )
        Ok ([|"a"; "\""    |], "c" )
        Ok ([|"a"; "\t"    |], "c" )
                                   
        Ok ([|"a"; "b"     |], "c" )
        Ok ([|"a"; "\""    |], "c" )
                                   
        Ok ([|             |], "a" )
        Ok ([|             |], "\"")
        Ok ([|             |], "a" )
        Ok ([|             |], "\"")
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
        Some "a b c"        , 2
        Some "  a  b  c  "  , 3

        Some "\"a\"b"       , 3
        Some "\"a\"\"b\""   , 3
        Some "\"\\a\""      , 3

        Some "@\"a\"b"      , 3
        Some "@\"a\"@\"b\"" , 3
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
