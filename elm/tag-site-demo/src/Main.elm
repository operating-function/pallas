port module Main exposing (main)

import Filesize as Filesize
import Maybe exposing (..)
import Set exposing (..)
import Json.Decode as Json
import Json.Encode
import String
import Tuple
import Browser
import Dict exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html exposing (..)
import Time

type alias Name = String

type alias DBStatus =
    { bytes     : Int
    , imgCount  : Int
    , pairCount : Int
    }

type UserCmd
    = UC_Search ImgRequest
    | UC_Info Int
    | UC_Status

type Response
    = SearchResponseBadTag (List String) DBStatus
    | SearchResponseOK ImgResults DBStatus
    | SearchResponseStatus DBStatus
--    | SearchResponseInfo Int (List String) String

encodeImgRequest : ImgRequest -> Json.Value
encodeImgRequest req = Json.Encode.object
                       [ ("offset", Json.Encode.int req.offset)
                       , ("tags", Json.Encode.list Json.Encode.string req.tags)]

encodeUserCmd : UserCmd -> Json.Value
encodeUserCmd cmd = case cmd of
    UC_Search imgRequest -> Json.Encode.object [
        ("tag", Json.Encode.string "Search"),
        ("contents", encodeImgRequest imgRequest)
        ]
    UC_Info imgId -> Json.Encode.object [
        ("tag", Json.Encode.string "Info"),
        ("contents", Json.Encode.int imgId)
        ]
    UC_Status -> Json.Encode.object [
        ("tag", Json.Encode.string "Status")
        ]

decodeTagged : String -> Json.Decoder a -> Json.Decoder a
decodeTagged tag more =
    let
        check got =
            if got == tag then
                more
            else
                Json.fail ("Expected _.tag = " ++ tag)
    in
        Json.field "tag" Json.string
     |> Json.andThen check

decodeImgResults : Json.Decoder ImgResults
decodeImgResults = Json.map4 ImgResults
    (Json.field "total" Json.int)
    (Json.field "offset" Json.int)
    (Json.field "query" (Json.list Json.string))
    (Json.field "slice"
         (Json.list
              (Json.map2 Tuple.pair
                   (Json.index 0 Json.int)
                   (Json.index 1 Json.string))))

decodeDBStatus : Json.Decoder DBStatus
decodeDBStatus = Json.map3 DBStatus
    (Json.field "bytes" Json.int)
    (Json.field "imgs" Json.int)
    (Json.field "pairs" Json.int)

decodeResponse : Json.Encode.Value -> Result Json.Error Response
decodeResponse = Json.decodeValue <| Json.oneOf
    [ decodeTagged "BadTag"
        (Json.map2 SearchResponseBadTag
          (Json.field "contents" (Json.list Json.string))
          (Json.field "status" decodeDBStatus)),
      decodeTagged "OK"
        (Json.map2 SearchResponseOK
          (Json.field "contents" decodeImgResults)
          (Json.field "status" decodeDBStatus)),
      decodeTagged "Status"
        (Json.map SearchResponseStatus
          (Json.field "contents" decodeDBStatus))
    ]

sendCmd : UserCmd -> Cmd msg
sendCmd uc = sendMessage (encodeUserCmd uc)


main : Program Int Model Msg
main =
  Browser.element
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

type alias PendingQuery = List String

-- What state can the demo be in?

type alias ImgRequest =
    { offset : Int
    , tags   : List String
    }

-- What do we display?
type alias ImgResults =
    { total  : Int
    , offset : Int
    , query  : List String
    , slice  : List (Int, String)
    }

type alias BadTagResult = Set String

type alias QueryState =
    { lastStatus     : Maybe DBStatus
    , lastGood       : Maybe ImgResults
    , waitingRequest : Maybe ImgRequest
    , pendingQuery   : Maybe ImgRequest
    , currentBadTags : Maybe BadTagResult
    }

emptyQueryState : QueryState
emptyQueryState =
    { lastStatus     = Nothing
    , lastGood       = Nothing
    , currentBadTags = Nothing
    , waitingRequest = Nothing
    , pendingQuery   = Nothing
    }

setLastStatus : DBStatus -> QueryState -> QueryState
setLastStatus s q = { q | lastStatus = Just s }

setLastGood : Maybe ImgResults -> QueryState -> QueryState
setLastGood res q = { q | lastGood = res }

setWaitingRequest : Maybe ImgRequest -> QueryState -> QueryState
setWaitingRequest req q = { q | waitingRequest = req }

setPendingQuery : Maybe ImgRequest -> QueryState -> QueryState
setPendingQuery req q = { q | pendingQuery = req }

setCurrentBadTags : Maybe BadTagResult -> QueryState -> QueryState
setCurrentBadTags bt q = { q | currentBadTags = bt }

type alias Model =
    { rawQuery : String
    , queryState : QueryState
    }

applyQueryState : Model -> (QueryState -> QueryState) -> Model
applyQueryState m fun = { m | queryState = fun m.queryState }


init : Int -> (Model, Cmd Msg)
init i = ({ rawQuery = "", queryState = emptyQueryState }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ messageReceiver Recv
    , Time.every 1000 Tick
    ]

-- Update

type Msg
    = ChangeQuery String
    | PageBackward
    | PageForward
    | Recv Json.Encode.Value
    | Tick Time.Posix

noCmd : a -> (a, Cmd b)
noCmd a = (a, Cmd.none)



port sendMessage : Json.Encode.Value -> Cmd msg
port messageReceiver : (Json.Encode.Value -> msg) -> Sub msg

sendSearch : Model -> String -> List String -> Int -> (Model, Cmd Msg)
sendSearch model query tags offset  =
    let request = { offset = offset
                  , tags = tags }
    in case model.queryState.waitingRequest of
      Nothing ->
        ({ model | rawQuery = query,
                   queryState = model.queryState
                                |> setWaitingRequest (Just request) },
         sendCmd (UC_Search request))
      Just waiting ->
        ({ model | rawQuery = query,
                   queryState = model.queryState
                                |> setPendingQuery (Just request)},
         Cmd.none)

-- Given a minimally modified model from update (ie, changing only lastGood or
-- currentBadTags), update waitingRequest and pendingQuery and start the next
-- request to the server side.
--
tryQueuedRequest : Model -> (QueryState -> QueryState) -> (Model, Cmd Msg)
tryQueuedRequest model fun =
    let qs = model.queryState
    in case qs.pendingQuery of
        Nothing -> ( { model
                     | queryState = model.queryState
                                    |> setWaitingRequest Nothing
                                    |> fun }
                   , Cmd.none)
        Just pending -> ( { model
                          | queryState = model.queryState
                                         |> setPendingQuery Nothing
                                         |> setWaitingRequest (Just pending)
                                         |> fun }
                        , sendCmd (UC_Search pending) )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeQuery newQuery -> sendSearch model newQuery (tokenizeTags newQuery) 0

    PageBackward -> case model.queryState.lastGood of
        Nothing -> (model, Cmd.none)
        Just qs -> sendSearch model
                              (String.concat (List.intersperse ", " qs.query))
                              qs.query
                              (qs.offset - 25)

    PageForward  -> case model.queryState.lastGood of
        Nothing -> (model, Cmd.none)
        Just qs -> sendSearch model
                              (String.concat (List.intersperse ", " qs.query))
                              qs.query
                              (qs.offset + 25)

    Recv jsonMsg -> case (decodeResponse jsonMsg) of
        Err e ->
            let foo = Debug.log "PORT RECV ERR2" e
            in noCmd model
        Ok (SearchResponseBadTag badTags status) ->
            tryQueuedRequest model
                             ( setCurrentBadTags (Just (Set.fromList badTags))
                             >> setLastStatus status )
        Ok (SearchResponseOK imgResults status) ->
            tryQueuedRequest model ( setLastGood (Just imgResults)
                                   >> setLastStatus status
                                   >> setCurrentBadTags Nothing)
        Ok (SearchResponseStatus status) ->
            ({ model | queryState = setLastStatus status model.queryState },
            Cmd.none)

    -- When there is no ongoing pending request, make a request for the current
    -- db status.
    Tick _ -> (model, Cmd.none)
        -- case model.queryState.waitingRequest of
        -- Nothing -> (model, sendCmd UC_Status)
        -- Just x -> (model, Cmd.none)

-- VIEW

tokenizeTags : String -> List String
tokenizeTags str =
    List.filter (\a -> (not (String.isEmpty a)))
                (List.map String.trim (String.split "," str))

tagView : Set String -> String -> Html Msg
tagView badTags t =
    let css = case Set.member t badTags of
                True -> [style "color" "red"]
                False -> []
    in span css [ text t ]

tagViewDiv : Model -> Html Msg
tagViewDiv model =
    let badTags = withDefault Set.empty model.queryState.currentBadTags
    in div [ ]
         (List.intersperse (span [] [ text ", " ])
           (List.map (tagView badTags) (tokenizeTags model.rawQuery)))

imgResult : (Int, String) -> Html Msg
imgResult (id, thumbnail) =
    div [ style "flex-basis" "350px"
        , style "max-width" "250px"
        , style "max-height" "250px"
        , style "overflow" "hidden"
        ]
        [img [ src thumbnail
             , style "object-fit" "cover"
             , style "max-width" "100%"
             , style "height" "auto"
             , style "justify-content" "center"
             , style "vertical-align" "middle"
             ] []]

imgHeadline : ImgResults -> String
imgHeadline imgResults = if imgResults.total == 0
    then "No results found"
    else ("Showing " ++ String.fromInt (1 + imgResults.offset) ++
          " - " ++
          String.fromInt (imgResults.offset +
                          (List.length imgResults.slice)) ++
          " of " ++
          String.fromInt (imgResults.total))

imgResultGrid : Maybe ImgResults -> Html Msg
imgResultGrid mybImgResults =
    div [] [
       div [ style "display" "flex"
           , style "flex-wrap" "wrap"
           , style "justify-content" "center"
           , style "gap" "5px"
           ]
           (List.map imgResult (case mybImgResults of
               Nothing -> []
               Just result -> result.slice))
      ]

imgDiv : QueryState -> Html Msg
imgDiv t = div [] [
    imgResultGrid t.lastGood
    ]

backDisabled : Model -> Bool
backDisabled model = case model.queryState.lastGood of
    Nothing      -> True
    Just results -> results.offset == 0

forwardDisabled model = case model.queryState.lastGood of
    Nothing      -> True
    Just results -> results.offset + 25 >= results.total

headerControlDiv : Model -> ImgResults -> Html Msg
headerControlDiv model imgResults =
    div []
        [ text (imgHeadline imgResults)
        , div [ style "float" "right"
              , style "display" "table"
              ]
              [ div [ style "display" "table-cell" ]
                    [ button [ onClick PageBackward
                             , disabled (backDisabled model)
                             ] [ text "<<" ] ],
                div [ style "display" "table-cell" ]
                    [ button [ onClick PageForward
                             , disabled (forwardDisabled model)
                             ] [ text ">>" ] ] ]
        ]

dbStatusDiv : Maybe DBStatus -> Html Msg
dbStatusDiv mybStatus =
    div [ style "float" "right" ]
        (case mybStatus of
          Nothing -> []
          Just status -> [ text ("Images: " ++
                                 (String.fromInt status.imgCount) ++
                                 " / " ++
                                 "Tagpairs: " ++
                                 (String.fromInt status.pairCount) ++
                                 " / " ++
                                 "Files: " ++
                                 Filesize.formatBase2 status.bytes) ])

headerDiv : Model -> Html Msg
headerDiv model =
    div [ style "width" "100%"
        , style "position" "sticky"
        , style "top" "0"
        , style "background" "white"
        , style "padding-bottom" "10px"
        , style "border-bottom" "thick double"
        ]
        [ h3 [] [text "Tag Search"]
        , div []
              [ input [ placeholder "Search",
                        value model.rawQuery,
                        onInput ChangeQuery ] []
              , dbStatusDiv model.queryState.lastStatus]
        , tagViewDiv model
        , case (model.queryState.lastGood, model.queryState.waitingRequest) of
            (_, Just _) -> text "Searching..."
            (Nothing, _) -> text "Type to search"
            (Just imgResults, _) -> headerControlDiv model imgResults
        ]

view : Model -> Html Msg
view model =
  div []
    [ headerDiv model
    , imgDiv model.queryState
    ]
