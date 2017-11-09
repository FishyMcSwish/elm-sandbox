module Main exposing (..)

import Http
import Html exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline exposing (required)
import RemoteData exposing (WebData)


type PageState
    = Landing


type Msg
    = NoOp
    | GotPrice (WebData PriceInfoJSON)



--  | PriceResponse String


type alias Model =
    { pageState : PageState
    , priceInfo : WebData PriceInfoJSON
    }


type alias PriceInfoJSON =
    { trade_id : Int
    , price : String
    , size : String
    , bid : String
    , ask : String
    , volume : String
    , time : String
    }


errorPriceInfo : PriceInfoJSON
errorPriceInfo =
    { trade_id = 0
    , price = "error"
    , size = "error"
    , bid = "error"
    , ask = "error"
    , volume = "error"
    , time = "error"
    }


initialModel : Model
initialModel =
    { pageState = Landing
    , priceInfo = RemoteData.NotAsked
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getInitialData )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPrice response ->
            ( { model | priceInfo = response }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.priceInfo of
        RemoteData.NotAsked ->
            text "not asked"

        RemoteData.Loading ->
            div [] [ text ("loading") ]

        RemoteData.Failure err ->
            div [] [ text ("failure") ]

        RemoteData.Success priceInfo ->
            div [] [ text (priceInfo.price) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


getInitialData : Cmd Msg
getInitialData =
    getPrice
        |> Cmd.map GotPrice


getPrice : Cmd (WebData PriceInfoJSON)
getPrice =
    (Http.get "https://api.gdax.com/products/btc-usd/ticker" decodePrice)
        |> RemoteData.sendRequest


decodePrice : Decode.Decoder PriceInfoJSON
decodePrice =
    Pipeline.decode PriceInfoJSON
        |> required "trade_id" Decode.int
        |> required "price" Decode.string
        |> required "bid" Decode.string
        |> required "ask" Decode.string
        |> required "volume" Decode.string
        |> required "size" Decode.string
        |> required "time" Decode.string
