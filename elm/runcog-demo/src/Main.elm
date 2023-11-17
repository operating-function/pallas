module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Decode as Json
import Json.Encode
import Json.Helpers
import Time

-- CogIds are 64-bit numbers rendered as Strings since cogids are 64 bit
-- numbers but Javascript only allows 53 bits in its numbers.
--
-- TODO: Make these btc base58 for nicer display?
type CogId = CogId String

cogIdAsStr : CogId -> String
cogIdAsStr (CogId str) = str

type CogStatus
    = SPINNING
    | FINISHED
    | CRASHED String String -- Crash with printed string of op and arg.
    | TIMED_OUT String      -- Timed out with string of timeout time.

statusStr : CogStatus -> String
statusStr cs = case cs of
    SPINNING -> "Spinning"
    FINISHED -> "Finished"
    CRASHED _ _ -> "Crashed"
    TIMED_OUT _ -> "Timed Out"

type alias CogState =
    { cogId : CogId
    , status : CogStatus
    }

type alias Model =
    { cogs : List CogState
    }

type Msg
    = SpinCog
    | SpunCog (Result Http.Error CogId)
    | SendStop CogId
    | StoppedCog (Result Http.Error ())
    | FetchStatusTick Time.Posix
    | ReceivedStatus (Result Http.Error (List CogState))

-- -----------------------------------------------------------------------

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

init : () -> (Model, Cmd Msg)
init _ = ({ cogs = []}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ Time.every 1000 FetchStatusTick
    ]

cogidDecoder : Decoder CogId
cogidDecoder = Json.Decode.map CogId (field "cogid" string)

cogStatusDecoder : Decoder CogStatus
cogStatusDecoder =
    let table = Dict.fromList
            [ ("Spinning", Json.succeed SPINNING)
            , ("Finished", Json.succeed FINISHED)
            , ("Crashed", Json.map2 CRASHED
                (field "op" string)
                (field "arg" string))
            , ("TimedOut", Json.map TIMED_OUT (field "ms" string))
            ]
    in Json.Helpers.decodeSumObjectWithSingleField "CogStatus" table

cogstateDecoder : Decoder CogState
cogstateDecoder =
    Json.Decode.map2 CogState
        (field "cogid" string |> Json.Decode.map CogId)
        (field "status" cogStatusDecoder)

cogstateListDecoder : Decoder (List CogState)
cogstateListDecoder = Json.Decode.list cogstateDecoder

-- TODO: This becomes a file picker action later.
spinCog : Cmd Msg
spinCog = Http.post
  { url = "/spin"
  , body = Http.emptyBody
  , expect = Http.expectJson SpunCog cogidDecoder
  }

fetchStatus : Cmd Msg
fetchStatus = Http.get
  { url = "/status"
  , expect = Http.expectJson ReceivedStatus cogstateListDecoder
  }

sendStop : CogId -> Cmd Msg
sendStop (CogId raw) = Http.post
  { url = "/stop"
  , body = Http.jsonBody (Json.Encode.object [
               ("cogid", Json.Encode.string raw) ] )
  , expect = Http.expectWhatever StoppedCog
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SpinCog ->
            let m = Debug.log "SpinCog" model
            in (m, spinCog)
        SpunCog (Ok cogid) -> (model, fetchStatus)
        SendStop cogid -> (model, sendStop cogid)
        StoppedCog (Ok ()) -> (model, fetchStatus)
        FetchStatusTick _ -> (model, fetchStatus)
        ReceivedStatus (Ok newList) ->
            let n = Debug.log "received status" newList
            in ({ model | cogs = n }, Cmd.none)
        _ -> (model, Cmd.none)

cogRow : CogState -> Html Msg
cogRow cs = tr [] [
    td [] [ text (cogIdAsStr cs.cogId) ],
    td [] [ text (statusStr cs.status) ],
    td [] [ button [ onClick (SendStop cs.cogId) ] [ text "Stop" ] ]
    ]

view : Model -> Html Msg
view model = div [] [
    button [ onClick SpinCog ] [ text "launch" ],
    table []
          [
          thead []
                [
                th [] [text "CogId"],
                th [] [text "Status"],
                th [] [text "Actions"]
                ],
          tbody [] (List.map cogRow model.cogs)
          ]
    ]
