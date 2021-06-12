module View exposing (Msg, document, view)

import Schedule exposing (Schedule, Slot)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Region as Region
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on)
import Json.Decode
import Time exposing (Posix)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)

type alias Msg
  = Never

document tagger model =
  { title = "2HOL Stream Marathon"
  , body = [Html.map tagger (view model)]
  }

view model = 
  Html.div [ Html.Attributes.class "view" ]
    [ layout
      [ Background.color (rgb255 23 20 31)
      , height fill
      , Font.color (rgb255 218 216 222)
      ] <|
      column [ height fill, width fill, spacing 5 ]
      [ displayHeader model
      , displaySchedule model Schedule.schedule
      , displayFooter model
      ]
    ]

--displaySchedule : Model -> Schedule -> Element msg
displaySchedule model schedule =
  List.foldl (displaySlotAccum model) ("", []) schedule
    |> Tuple.second
    |> List.reverse
    |> column [ width fill, height (fill |> minimum 360), spacing 5 ]

--displaySlotAccum : Model -> Slot -> (String, List (Element msg)) -> (String, List (Element msg))
displaySlotAccum model slot (prior, results) =
  (dateMonthDay model.zone slot.end, (displaySlot model prior slot) :: results)

--displaySlot : Model -> String -> Slot -> Element msg
displaySlot model prior {username, start, end} =
  let
    startDate = dateMonthDay model.zone start
    delta = (Time.posixToMillis end) - (Time.posixToMillis start)
    hours = (delta // (60 * 60 * 1000))
  in
  row
    [ width fill
    , height (fillPortion hours)
    ]
    [ column
      [ width (fillPortion 1 |> minimum 100 )
      , height fill
      , Background.color (rgb 1.0 0.733 0.208)
      , Font.color (rgb 0 0 0)
      , Font.size (timeSize model.windowHeight)
      ]
      [ dateMonthDayHourMinute prior model.zone start
      , el [ height fill ] none
      , dateMonthDayHourMinute startDate model.zone end
      ]
    , column
      [ width (fillPortion 5)
      , alignTop
      , padding 10
      , Font.size (nameSize model.windowHeight)
      ]
      [ link []
        { url = "https://twitch.tv/" ++ username
        , label = text username
        }
      ]
    ]

displayHeader model =
  row
    [ width fill
    , Font.size (titleSize model.windowHeight)
    ]
    [ column
      [ width (fillPortion 1 |> minimum 100 )
      , height fill
      , Background.color (rgb 1.0 0.733 0.208)
      , Font.color (rgb 0 0 0)
      , Font.bold
      ]
      [ el [ height fill] none
      , el [ width fill, Font.center ] (text "2HOL")
      , el [ height fill] none
      ]
    , column
      [ width (fillPortion 5)
      , padding 10
      ]
      [ text "Stream Marathon"
      ]
    ]

--displayFooter : Model -> Element msg
displayFooter model =
  row
    [ Region.footer
    , spacing 10
    , alignBottom
    , alignRight
    , Font.size (scaled model.windowHeight -2)
    ]
    [ {-link []
      { url = "https://github.com/JustinLove/stream-credits"
      , label = row [] [ icon "github", text "stream-credits" ]
      }
    ,-} link []
      { url = "https://twitter.com/wondible"
      , label = row [] [ icon "twitter", text "@wondible" ]
      }
    , link []
      { url = "https://twitch.tv/wondible"
      , label = row [] [ icon "twitch", text "wondible" ]
      }
    ]

icon : String -> Element msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]
  |> html

dateMonthDayHourMinute : String -> Time.Zone -> Posix -> Element msg
dateMonthDayHourMinute prior zone time =
  let
    date = dateMonthDay zone time
  in
  row
    [ width fill
    ]
    [ if date == prior then none else dateMonthDay zone time |> text
    , el [ width fill ] none
    , dateHourMinute zone time |> text
    ]

dateMonthDay : Time.Zone -> Posix -> String
dateMonthDay zone time =
  let
    month = Time.toMonth zone time |> formatMonth
    day = Time.toDay zone time |> String.fromInt |> String.padLeft 2 '0'
  in
    month ++ "-" ++ day

dateHourMinute : Time.Zone -> Posix -> String
dateHourMinute zone time =
  let
    hour = Time.toHour zone time |> String.fromInt |> String.padLeft 2 '0'
    minute = Time.toMinute zone time |> String.fromInt |> String.padLeft 2 '0'
  in
    hour ++ ":" ++ minute

formatMonth : Time.Month -> String
formatMonth month =
  case month of
    Time.Jan -> "01"
    Time.Feb -> "02"
    Time.Mar -> "03"
    Time.Apr -> "04"
    Time.May -> "05"
    Time.Jun -> "06"
    Time.Jul -> "07"
    Time.Aug -> "08"
    Time.Sep -> "09"
    Time.Oct -> "10"
    Time.Nov -> "11"
    Time.Dec -> "12"

titleSize height = scaled height 3
timeSize height = scaled height 2
nameSize height = scaled height 2

scaled height = modular (max ((toFloat height)/30) 15) 1.25 >> round
scheduleRange =
  let
    first = List.head Schedule.schedule
      |> Maybe.map .start
      |> Maybe.map Time.posixToMillis
      |> Maybe.withDefault 0
    last = List.head (List.reverse Schedule.schedule)
      |> Maybe.map .end
      |> Maybe.map Time.posixToMillis
      |> Maybe.withDefault 0
  in
    last - first
