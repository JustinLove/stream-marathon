module Schedule exposing (Schedule, Streamer, Slot, schedule)

import Iso8601

import Time exposing (Posix)

type alias Schedule = List Slot

type alias Streamer =
  { username : String
  , displayname : String
  }

type alias Slot =
  { streamers : List Streamer
  , start : Posix
  , end : Posix
  }

time hour =
  humanTime ("2021-12-11T"++hour++":00:00-05:00")

schedule : List Slot
schedule =
  [ { streamers =
      [ { username = "persephone_awides"
        , displayname = "Persephone_Awides"
        }
      ]
    , start = time "06"
    , end = time "08"
    }
  , { streamers =
      [ { username = "cartoonjessie"
        , displayname = "CartoonJessie"
        }
      , { username = "porthosuk"
        , displayname = "PorthosUK"
        }
      ]
    , start = time "08"
    , end = time "10"
    }
  , { streamers =
      [ { username = "lizzeegames"
        , displayname = "lizzeegames"
        }
      ]
    , start = time "10"
    , end = time "12"
    }
  , { streamers =
      [ { username = "wondible"
        , displayname = "wondible"
        }
      ]
    , start = time "12"
    , end = time "14"
    }
  , { streamers =
      [ { username = "joosyfine"
        , displayname = "joosyfine"
        }
      ]
    , start = time "14"
    , end = time "16"
    }
  , { streamers =
      [ { username = "secretkittykat"
        , displayname = "secretkittykat"
        }
      ]
    , start = time "16"
    , end = time "18"
    }
  ]

humanTime : String -> Posix
humanTime s =
  Iso8601.toTime s
    --|> Result.mapError (Debug.log ("time error " ++ s))
    |> Result.withDefault (Time.millisToPosix 0)
