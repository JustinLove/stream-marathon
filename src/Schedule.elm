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
  humanTime ("2021-06-19T"++hour++":00:00-04:00")

schedule : List Slot
schedule =
  [
    { username = "cartoonjessie"
    , displayname = "CartoonJessie"
    , start = time "04"
    , end = time "08"
    }
  , { username = "persephone_awides"
    , displayname = "Persephone_Awides"
    , start = time "08"
    , end = time "10"
    }
  , { username = "twisted_100"
    , displayname = "Twisted_100"
    , start = time "10"
    , end = time "13"
    }
  , { username = "rainiedash"
    , displayname = "RainieDash"
    , start = time "13"
    , end = time "17"
    }
  , { username = "secretkittykat"
    , displayname = "secretkittykat"
    , start = time "17"
    , end = time "19"
    }
  , { username = "wondible"
    , displayname = "wondible"
    , start = time "19"
    , end = time "21"
    }
  , { username = "angelatinelive"
    , displayname = "AngelatineLIVE"
    , start = time "21"
    , end = time "23"
    }
  ]

humanTime : String -> Posix
humanTime s =
  Iso8601.toTime s
    --|> Result.mapError (Debug.log ("time error " ++ s))
    |> Result.withDefault (Time.millisToPosix 0)
