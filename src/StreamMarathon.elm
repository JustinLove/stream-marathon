module StreamMarathon exposing (..)

--import Decode exposing (Stream)
--import LocalStorage
import Log
--import Twitch.Helix exposing (UserId)
--import Twitch.Helix.Request as Helix
--import Twitch.Tmi.Chat as Chat
--import Twitch.Tmi.ChatSamples as Chat
--import TwitchId
--import SelectCopy
import Schedule exposing (Slot)
import View

import Browser
import Browser.Dom as Dom
--import Http
import Task
import Time exposing (Posix, Zone)
--import Set
--import Dict exposing (Dict)
--import Json.Decode as Decode
--import Json.Encode as Encode

--requestLimit = 100
--unauthenticatedRequestRate = 30
--authenticatedRequestRate = 800

type Msg
  = UI (View.Msg)
  | CurrentZone Zone
  | CurrentTime Posix
  --= HttpError String Http.Error
  --| Channel (List String)
  --| Users (List User)
  --| UserUpdate (List User)
  --| UnknownUsers (List User)
  --| HostingUser (List User)
  --| Streams (List Stream)
  --| ChannelStream (List Stream)
  --| NextRequest Posix
  --| Focused (Result Dom.Error ())

type alias Model =
  { time : Posix
  , zone : Zone
  , windowWidth : Int
  , windowHeight : Int
  --, location : Url
  --, users : Dict String User
  --, liveStreams : Dict String Stream
  --, auth : Maybe String
  --, authLogin : Maybe String
  --, pendingUsers : List String
  --, pendingUserStreams : List String
  --, pendingRequests : List (Cmd Msg)
  --, outstandingRequests : Int
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
  --let
    --url = Url.fromString href
      --|> Maybe.withDefault (Url Url.Http "" Nothing "" Nothing Nothing)
  --in
  ( { time = Time.millisToPosix 0
    , zone = Time.utc
    , windowWidth = 480
    , windowHeight = 480
    --, location = url
    --, users = Dict.empty
    --, liveStreams = Dict.empty
    --, pendingUsers = []
    --, pendingUserStreams = []
    --, pendingRequests = []
    --, outstandingRequests = 0
    --, selectedUser = Nothing
    }
    --|> update (SocketEvent 0 (PortSocket.Message sampleHost)) |> Tuple.first
    --|> update (AudioStart (Time.millisToPosix 0)) |> Tuple.first
  , Cmd.batch 
    [ Task.perform CurrentZone Time.here
    , Task.perform CurrentTime Time.now
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
    {-HttpError source (Http.BadStatus 401) ->
      let m2 = logout model in
      ( m2
      , Cmd.batch
        [ saveState m2
        , Log.warn ("fetch auth error: " ++ source)
        ]
      )
    HttpError source (error) ->
      (model, Log.httpError ("fetch error: " ++ source) error)
      
    Channel (login::_) ->
      { model
      | autoChannel = HostOn login
      , channelStatus = Unknown
      }
      |> appendUnauthenticatedRequests [ fetchChannelStream login ]
      |> persist
    Channel _ ->
      (model, Log.warn "channel did not find user")
    Users users ->
      { model
      | users = addUsers (List.map persistUser users) model.users
      , pendingUserStreams = List.append model.pendingUserStreams
        <| List.map .id users
      }
      |> fetchNextUserBatch requestLimit
      |> fetchNextUserStreamBatch requestLimit
      |> persist
    UserUpdate users ->
      { model
      | users = addUsers (List.map persistUser users) model.users
      }
      |> persist
    UnknownUsers users ->
      ( { model
        | users = addUsers users model.users
        }
      , Cmd.none)
    HostingUser (user::_) ->
      ( { model
        | channelStatus = Hosting user.id
        }
      , Cmd.none
      )
    HostingUser _ ->
      (model, Log.warn "hosting did not find user")
    Streams streams ->
      ( fetchNextGameBatch requestLimit
        <| fetchNextUserStreamBatch requestLimit
        <| fetchUnknownUsers streams <| fetchUnknownFollows streams
        <| fetchExpiredFollows streams
        { model
        | liveStreams = List.foldl (\s liveStreams ->
            Dict.insert s.userId s liveStreams
          ) model.liveStreams streams
        , users = List.foldl (\s users ->
            Dict.update s.userId (Maybe.map (\user -> {user | displayName = s.userName})) users
          ) model.users streams
        }
      , Cmd.none)
    ChannelStream (stream::_) ->
      ( { model
        | channelStatus = Live
        , autoHostStatus = case model.autoHostStatus of
          Pending -> AutoEnabled
          _ -> model.autoHostStatus
        }
      , Cmd.none)
    ChannelStream [] ->
      ( { model | channelStatus = case model.channelStatus of
          Unknown -> Offline
          Offline -> Offline
          Hosting _ -> model.channelStatus
          Live -> Offline
        }
      , Cmd.none)
    LivePoll time ->
      ( {model | time = time}
        |> pollAutoChannel
      , Cmd.none
      )
    Response subMsg ->
      let
        (m2, cmd2) = update subMsg { model | outstandingRequests = model.outstandingRequests - 1}
      in
      if m2.channelStatus == Offline
      && m2.autoHostStatus == Pending
      && m2.outstandingRequests == 0
      && List.isEmpty m2.pendingRequests then
        case View.sortedStreams m2 of
          top :: _ ->
            let
              muser = Dict.get top.userId m2.users
              name = muser |> Maybe.map .displayName |> Maybe.withDefault "unknown"
            in
              ( {m2 | autoHostStatus = AutoEnabled}
              , Cmd.batch
                [ cmd2
                , hostChannel m2.ircConnection name
                , Log.info ("picking " ++ name)
                ])
          _ ->
            ( {m2 | autoHostStatus = AutoEnabled}
            , Cmd.batch
              [ cmd2
              , Log.info "no streams"
              ])
      else
        (m2, cmd2)
    NextRequest time ->
      case model.pendingRequests of
        next :: rest ->
          ( { model
            | pendingRequests = rest
            , outstandingRequests = model.outstandingRequests + (if next == Cmd.none then 0 else 1)
            , time = time
            }, next)
        _ -> (model, Cmd.none)
    TextSize {text, width} ->
      ( {model | labelWidths = Dict.insert text width model.labelWidths}, Cmd.none)
    Focused _ ->
      (model, Cmd.none)
      -}

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (60 * 1000) CurrentTime
    ]
    {-
    [ if List.isEmpty model.pendingRequests then
        Sub.none
      else
        case model.auth of
          Just _ ->
            Time.every (1000*60*1.05/authenticatedRequestRate) NextRequest
          Nothing ->
            Time.every (1000*60*1.05/unauthenticatedRequestRate) NextRequest
    ]
    -}
