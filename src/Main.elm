import StartApp
import Http
import Task
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Graph exposing (..)
import String
import Debug

-- BOOTSTRAP

app : StartApp.App Model
app =
  StartApp.start { init = init, inputs = [], update = update, view = view }

main : Signal.Signal Html.Html
main =
  app.html

port runner : Signal (Task.Task Never ())
port runner =
  app.tasks


-- MODEL

init : (Model, Effects Action)
init =
  ( { mode = Init
    , href = ""
    , ping = 3
    , nodes = []
    , times = []
    , error = Nothing
    }, 
    Effects.none )

type alias Model
  = { mode: Mode
    , href: String
    , ping: Int
    , nodes: List Node
    , times: List Time
    , error: Maybe Http.Error
    }

type Mode
  = Init
  | Load
  | Done

type alias Node
  = { uuid: String
    , name: String
    , region: String
    }

type alias Time
  = { node: Node
    , href: String
    , msec: Int
    , code: Int
    }


-- UPDATE

type Action 
  = NoOp
  | Href String
  | Ping String
  | QueryTimes
  | OnQueryTimes (Result Http.Error (List Time))

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case Debug.log "action" action of

    NoOp -> (model, Effects.none)

    Href href ->
      ({ model | href = href }, Effects.none)

    Ping ping ->
      ({ model | ping = Result.withDefault 3 (String.toInt ping) }, Effects.none)

    QueryTimes  ->
      ( { model |
          mode = Load
        , times = []
        , error = Nothing
        }, 
        queryTimes model.href model.ping 
          |> Task.toResult
          |> Task.map OnQueryTimes
          |> Effects.task )

    OnQueryTimes (Ok times) ->
      ( { model |
          mode = Done
        , times = times
        }, 
        Effects.none )

    OnQueryTimes (Err error) ->
      ( { model |
          mode = Done
        , error = Just error
        }, 
        Effects.none )


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "ui text container" ]
    [ section
      [ ]
      [ h1 [] [ text "Ping Pong" ]
      , hrefInput address
      , pingInput address
      , button [ onClick address QueryTimes ] [ text "Ping" ]
      ]
    ]

hrefInput : Signal.Address Action -> Html
hrefInput address =
  section
    [ ]
    [ input [ on "input" targetValue (Signal.message address << Href) ] [ ] ]

pingInput : Signal.Address Action -> Html
pingInput address =
  section
    [ ]
    [ input [ on "input" targetValue (Signal.message address << Ping), value "3" ] [ ] ]


-- GRAPH

graphTimes : String -> Int -> Graph
graphTimes href ping =
  Graph [
    Field "times" [
      Param "href" (ValueString href),
      Param "ping" (ValueNumber ping)
    ] [
      Field "node" [] [
        Field "id" [] [],
        Field "data" [] []
      ],
      Field "href" [] [],
      Field "msec" [] [],
      Field "code" [] []
    ]
  ]


-- TASKS

queryTimes : String -> Int -> Task.Task Http.Error (List Time)
queryTimes href ping =
  query (graphTimes href ping) timesDecoder

query : Graph -> Json.Decoder a -> Task.Task Http.Error (a)
query graph decod =
  Http.post decod "http://pingpong.mo.sap.corp:4000" (graphToBody graph)


-- DECODER

timeDecoder : Json.Decoder Time
timeDecoder = 
  Json.object4 Time 
    ("node" := Json.object3 Node 
                ("id" := Json.string) 
                (Json.at ["data", "name"] Json.string) 
                (Json.at ["data", "region"] Json.string)) 
    ("href" := Json.string) 
    ("msec" := Json.int) 
    ("code" := Json.int)

timesDecoder : Json.Decoder (List Time)
timesDecoder = 
  Json.object1 identity
    (Json.at ["data", "times"] (Json.list timeDecoder))
