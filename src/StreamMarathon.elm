module StreamMarathon exposing (..)

import Log
--import SelectCopy
import Schedule exposing (Slot)
import View

import Browser
import Browser.Dom as Dom
import Browser.Events
import Task
import Time exposing (Posix, Zone)

type Msg
  = UI (View.Msg)
  | CurrentZone Zone
  | CurrentTime Posix
  | WindowSize (Int, Int)

type alias Model =
  { time : Posix
  , zone : Zone
  , windowWidth : Int
  , windowHeight : Int
  , windowSize : Int
  --, selectedUser : Maybe String
  }

main = Browser.document
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = View.document UI
  }

init : String -> (Model, Cmd Msg)
init href =
  ( { time = Time.millisToPosix 0
    , zone = Time.utc
    , windowWidth = 480
    , windowHeight = 480
    , windowSize = 480
    --, selectedUser = Nothing
    }
  , Cmd.batch 
    [ Task.perform CurrentZone Time.here
    , Task.perform CurrentTime Time.now
    , Dom.getViewport
      |> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
      |> Task.perform WindowSize
    ]
  )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI _ ->
      (model, Cmd.none)
    CurrentZone zone ->
      ( {model | zone = zone}, Cmd.none)
    CurrentTime time ->
      ( {model | time = time}, Cmd.none)
    WindowSize (width, height) ->
      ( {model | windowWidth = width, windowHeight = height, windowSize = min width height}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (60 * 1000) CurrentTime
    , Browser.Events.onResize (\w h -> WindowSize (w, h))
    ]
