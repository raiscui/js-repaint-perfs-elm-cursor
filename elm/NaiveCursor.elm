module NaiveCursor (..) where

import Effects
import Task
import Html exposing (div, span, table, tbody, tr, td, text)
import Html.Attributes exposing (class)
import StartApp
import Cursor exposing (..)
import Focus exposing ((=>))
import Automaton exposing (..)


type alias Query =
  { elapsedClassName : String
  , formatElapsed : String
  , query : String
  }


type alias LastSample =
  { countClassName : String
  , nbQueries : Int
  , topFiveQueries : List Query
  }


type alias Database =
  { dbname : String
  , lastSample : LastSample
  }


type alias Model =
  List Database


type alias DatabaseC =
  Cursor Database Database


type alias ModelC =
  List DatabaseC


initialModel : Model
initialModel =
  []


dbnew : ModelC
dbnew =
  List.map db2c initialModel


db2c : Database -> DatabaseC
db2c database =
  createC (Signal.mailbox database) database



-- autoModel2C =
--   pure db2c


autodbList2cLIst : Automaton Model ModelC
autodbList2cLIst =
  pure dbList2cLIst


dbList2cLIst : Model -> ModelC
dbList2cLIst dbList =
  List.map db2c dbList


actions2 : Signal ModelC
actions2 =
  run autodbList2cLIst dbnew dispatchGenerateData


type Action
  = LoadData ModelC


update : Action -> ModelC -> ( ModelC, Effects.Effects Action )
update action model =
  case action of
    LoadData model ->
      ( model, Effects.none )


elapsedClassNameL : Focus.Focus Query String
elapsedClassNameL =
  Focus.create .elapsedClassName (\f r -> { r | elapsedClassName = f r.elapsedClassName })


formatElapsedL : Focus.Focus Query String
formatElapsedL =
  Focus.create .formatElapsed (\f r -> { r | formatElapsed = f r.formatElapsed })


queryL : Focus.Focus Query String
queryL =
  Focus.create .query (\f r -> { r | query = f r.query })


viewTopFiveQueries : Cursor Query Query -> Html.Html
viewTopFiveQueries query =
  let
    cursor1 =
      query

    -- cursor1 =
    --   query
    elapsedClassName =
      cursor1 >=> elapsedClassNameL

    formatElapsed =
      cursor1 >=> formatElapsedL

    query1 =
      cursor1 >=> queryL
  in
    td
      [ class ("Query " ++ (getC elapsedClassName)) ]
      [ text <| getC formatElapsed
      , div
          [ class "popover left" ]
          [ div
              [ class "popover-content" ]
              [ text <| getC query1 ]
          , div
              [ class "arrow" ]
              []
          ]
      ]


dbnameL : Focus.Focus Database String
dbnameL =
  Focus.create .dbname (\f r -> { r | dbname = f r.dbname })


lastSampleL : Focus.Focus Database LastSample
lastSampleL =
  Focus.create .lastSample (\f r -> { r | lastSample = f r.lastSample })


countClassNameL : Focus.Focus LastSample String
countClassNameL =
  Focus.create .countClassName (\f r -> { r | countClassName = f r.countClassName })


nbQueriesL : Focus.Focus LastSample Int
nbQueriesL =
  Focus.create .nbQueries (\f r -> { r | nbQueries = f r.nbQueries })


topFiveQueriesL : Focus.Focus LastSample (List Query)
topFiveQueriesL =
  Focus.create .topFiveQueries (\f r -> { r | topFiveQueries = f r.topFiveQueries })


viewDatabase : Cursor Database Database -> Html.Html
viewDatabase database =
  let
    cursor =
      database

    dbname =
      cursor >=> dbnameL

    lastSample =
      cursor >=> lastSampleL

    countClassName =
      lastSample >=> countClassNameL

    nbQueries =
      lastSample >=> nbQueriesL

    topFiveQueries =
      lastSample >=> topFiveQueriesL
  in
    tr
      []
      ([ td
          [ class "dbname" ]
          [ text <| getC dbname ]
       , td
          [ class "query-count" ]
          [ span
              [ class <| getC countClassName ]
              [ text (toString <| getC nbQueries) ]
          ]
       ]
        ++ (List.map (\q -> viewTopFiveQueries (createC (Signal.mailbox q) q)) (getC topFiveQueries))
      )


view : Signal.Address Action -> ModelC -> Html.Html
view address model =
  div
    []
    [ table
        [ class "table table-striped latest-data" ]
        [ tbody
            []
            (List.map viewDatabase model)
        ]
    ]


actions : Signal Action
actions =
  Signal.map LoadData actions2


app : StartApp.App ModelC
app =
  StartApp.start
    { init = ( dbnew, Effects.none )
    , update = update
    , view = view
    , inputs = [ actions ]
    }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


port dispatchGenerateData : Signal Model
