module Urls exposing (..)

import Models exposing (..)


resumeUrl : Flags -> String
resumeUrl flags =
    flags.api ++ "/resume"
