module View exposing (Msg, document, view)

import Schedule exposing (Schedule, Slot)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
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
  { title = "Two Hours One Life Community Stream Marathon"
  , body = [Html.map tagger (view model)]
  }

view model = 
  Html.div [ Html.Attributes.class "view" ]
    [ layout
      [ height fill
      ] <|
      column [ height fill, width fill, spacing (itemSpacing model.windowSize) ]
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
    |> column [ width fill, height (fill |> minimum 360), spacing (itemSpacing model.windowSize) ]

--displaySlotAccum : Model -> Slot -> (String, List (Element msg)) -> (String, List (Element msg))
displaySlotAccum model slot (prior, results) =
  (dateMonthDay model.zone slot.end, (displaySlot model prior slot) :: results)

--displaySlot : Model -> String -> Slot -> Element msg
displaySlot model prior {username, displayname, start, end} =
  let
    startDate = dateMonthDay model.zone start
    delta = (Time.posixToMillis end) - (Time.posixToMillis start)
    hours = (delta // (60 * 60 * 1000))
  in
  row
    [ width fill
    , height (fillPortion hours)
    , Border.color black
    , Border.width (if inTimeRange model.time (start, end) then 5 else 0)
    ]
    [ column
      [ width (fillPortion 1 |> minimum (columnSize model.windowSize) )
      , height fill
      , padding 10
      , Background.color yellow
      , Font.color black
      , Font.size (timeSize model.windowSize)
      ]
      [ dateMonthDayHourMinute prior model.zone start
      , el [ height fill ] none
      , dateMonthDayHourMinute startDate model.zone end
      ]
    , column
      [ width (fillPortion 3)
      , alignTop
      , padding 10
      , Font.size (nameSize model.windowSize)
      ]
      [ row []
        [ icon "twitch"
        , text " "
        , link []
          { url = "https://twitch.tv/" ++ username
          , label = text displayname
          }
        ]
      ]
    ]

displayHeader model =
  row
    [ width fill
    , Font.size (titleSize model.windowSize)
    ]
    [ column
      [ width (fillPortion 1 |> minimum (columnSize model.windowSize) )
      , height fill
      , padding 10
      , Background.color yellow
      , Font.color black
      , Font.bold
      , Font.size (titleSize model.windowSize)
      ]
      [ el [ height fill] none
      , link
        [ width fill
        , Font.center
        , htmlAttribute <| Html.Attributes.class "thol-link"
        ]
        { url = "https://twohoursonelife.com/"
        , label = text "2HOL"
        }
      , el [ height fill] none
      ]
    , column
      [ width (fillPortion 3)
      , padding 10
      , spacing 4
      ]
      [ el [ Font.size (titleSize model.windowSize) ] (text "Stream Marathon")
      , paragraph [ Font.size (textSize model.windowSize) ]
        [ text "Come watch us and "
        , link [ ]
          { url = "https://twohoursonelife.com/"
          , label = el [ Font.color yellow ] (text "join the family in Two Hours One Life")
          }
        , text "."
        ]
      ]
    ]

--displayFooter : Model -> Element msg
displayFooter model =
  row
    [ Region.footer
    , spacing 10
    , alignBottom
    , alignRight
    , Font.size (scaled model.windowSize -2)
    ]
    [ link []
      { url = "https://github.com/JustinLove/stream-marathon"
      , label = row [] [ icon "github", text "stream-marathon" ]
      }
    , link []
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
    month ++ " " ++ day

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
    Time.Jan -> "Jan"
    Time.Feb -> "Feb"
    Time.Mar -> "March"
    Time.Apr -> "April"
    Time.May -> "May"
    Time.Jun -> "June"
    Time.Jul -> "July"
    Time.Aug -> "Aug"
    Time.Sep -> "Sep"
    Time.Oct -> "Oct"
    Time.Nov -> "Nov"
    Time.Dec -> "Dec"

titleSize height = scaled height 2
textSize height = scaled height -1
timeSize height = scaled height -1
nameSize height = scaled height 1
itemSpacing height = height // 100

columnSize height = (timeSize height) * 10

scaled height = modular (atMost (atLeast ((toFloat height)/20) 15) 20) 1.25 >> round

black = rgb 0 0 0
white = rgb 1 1 1
yellow = rgb 1.0 0.733 0.208

inTimeRange : Posix -> (Posix, Posix) -> Bool
inTimeRange test (start, end) =
  let t = Time.posixToMillis test in
  (Time.posixToMillis start) <= t && t < (Time.posixToMillis end)

atLeast : Float -> Float -> Float
atLeast = max

atMost : Float -> Float -> Float
atMost = min
