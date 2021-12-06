module Schedule exposing (Schedule, Slot, schedule)

import Iso8601

import Time exposing (Posix)

type alias Schedule = List Slot

type alias Slot =
  { username : String
  , displayname : String
  , start : Posix
  , end : Posix
  }

time hour =
  humanTime ("2021-12-11T"++hour++":00:00-05:00")

schedule : List Slot
schedule =
  [ { username = "persephone_awides"
    , displayname = "Persephone_Awides"
    , start = time "06"
    , end = time "08"
    }
  , { username = "cartoonjessie"
    , displayname = "CartoonJessie and/or PorthosUK"
    , start = time "08"
    , end = time "10"
    }
  , { username = "lizzeegames"
    , displayname = "lizzeegames"
    , start = time "10"
    , end = time "12"
    }
  , { username = "secretkittykat"
    , displayname = "secretkittykat"
    , start = time "12"
    , end = time "14"
    }
  , { username = "joosyfine"
    , displayname = "joosyfine"
    , start = time "14"
    , end = time "16"
    }
  , { username = "wondible"
    , displayname = "wondible"
    , start = time "16"
    , end = time "18"
    }
  ]

humanTime : String -> Posix
humanTime s =
  Iso8601.toTime s
    --|> Result.mapError (Debug.log ("time error " ++ s))
    |> Result.withDefault (Time.millisToPosix 0)
