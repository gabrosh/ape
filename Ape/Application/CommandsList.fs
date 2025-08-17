module CommandsList

open CompletionCommon
open CompletionBasic
open CompletionKeyMappings
open CompletionRegisters
open CompletionSettings
open ExecutionBasic
open ExecutionKeyMappings
open ExecutionRegisters
open ExecutionSettings

let public commandsList = [
    "help"          , "h"   , argsMapSpec_help             , execute_help             , complete_none
    "execCfg"       , "c"   , argsMapSpec_execCfg          , execute_execCfg          , complete_execCfg
    "quit"          , "q"   , argsMapSpec_quit             , execute_quit             , complete_none
    "quit!"         , "q!"  , argsMapSpec_quitBang         , execute_quitBang         , complete_none
    "write"         , "w"   , argsMapSpec_write            , execute_write            , complete_write
    "write!"        , "w!"  , argsMapSpec_writeBang        , execute_writeBang        , complete_writeBang
    "writeQuit"     , "wq"  , argsMapSpec_writeQuit        , execute_writeQuit        , complete_none
    "edit"          , "e"   , argsMapSpec_edit             , execute_edit             , complete_edit
    "edit!"         , "e!"  , argsMapSpec_editBang         , execute_editBang         , complete_editBang
    "view"          , "v"   , argsMapSpec_view             , execute_view             , complete_view
    "view!"         , "v!"  , argsMapSpec_viewBang         , execute_viewBang         , complete_viewBang
    "reload"        , "r"   , argsMapSpec_reload           , execute_reload           , complete_none
    "reload!"       , "r!"  , argsMapSpec_reloadBang       , execute_reloadBang       , complete_none
    "extract"       , "x"   , argsMapSpec_extract          , execute_extract          , complete_extract
    "bufferDelete"  , "bd"  , argsMapSpec_bufferDelete     , execute_bufferDelete     , complete_none
    "bufferDelete!" , "bd!" , argsMapSpec_bufferDeleteBang , execute_bufferDeleteBang , complete_none
    "bufferNext"    , "bn"  , argsMapSpec_bufferNext       , execute_bufferNext       , complete_none
    "bufferPrev"    , "bp"  , argsMapSpec_bufferPrev       , execute_bufferPrev       , complete_none
    "bufferFirst"   , "bf"  , argsMapSpec_bufferFirst      , execute_bufferFirst      , complete_none
    "bufferLast"    , "bl"  , argsMapSpec_bufferLast       , execute_bufferLast       , complete_none
    "bufferBegin"   , "bb"  , argsMapSpec_bufferBegin      , execute_bufferBegin      , complete_none
    "bufferEnd"     , "be"  , argsMapSpec_bufferEnd        , execute_bufferEnd        , complete_none
    "set"           , "s"   , argsMapSpec_set              , execute_set              , complete_set
    "unset"         , "us"  , argsMapSpec_unset            , execute_unset            , complete_unset
    "get"           , "g"   , argsMapSpec_get              , execute_get              , complete_get
    "map"           , "m"   , argsMapSpec_map              , execute_map              , complete_map
    "unmap"         , "um"  , argsMapSpec_unmap            , execute_unmap            , complete_unmap
    "reg"           , ""    , argsMapSpec_reg              , execute_reg              , complete_reg
    "unreg"         , ""    , argsMapSpec_unreg            , execute_unreg            , complete_unreg
]

let public commandsListCfg = [
    "execCfg"       , "c"   , argsMapSpec_execCfg          , execute_execCfg          , complete_none
    "set"           , "s"   , argsMapSpec_set              , execute_set              , complete_none
    "unset"         , "us"  , argsMapSpec_unset            , execute_unset            , complete_none
    "map"           , "m"   , argsMapSpec_map              , execute_map              , complete_none
    "unmap"         , "um"  , argsMapSpec_unmap            , execute_unmap            , complete_none
    "reg"           , ""    , argsMapSpec_reg              , execute_reg              , complete_none
    "unreg"         , ""    , argsMapSpec_unreg            , execute_unreg            , complete_none
]
