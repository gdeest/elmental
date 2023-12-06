module Main exposing (..)

import Browser
import Html exposing (..)
import Codegen.SampleTypes
import Codegen.Submission
import Codegen.Report

main =
  Browser.sandbox { init = init, view = view, update = update }

type alias Model =
  String 

init =
  "hello, world!"

type Msg 
  = NoOp

update msg model =
  case msg of
    NoOp ->
      model

view model =
  div [] [ text model ] 
