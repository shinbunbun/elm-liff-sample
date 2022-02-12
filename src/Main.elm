port module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- PORTS


port sendMessage : String -> Cmd msg


port dataReceiver : (Model -> msg) -> Sub msg



-- MODEL


type alias Model =
  { draft : String
  , message : String
  , liffReceiveData : LIFFReceiveData
  }


init : () -> ( Model, Cmd Msg )
init flags =
  ( { draft = "", message = "", liffReceiveData = LIFFReceiveData "" [] }
  , Cmd.none
  )



-- UPDATE


type Msg
  = DraftChanged String
  | Send
  | Recv Model



-- ユーザーがエンターキーを押すか、Send ボタンをクリックしたとき、`sendMessage`ポートを使っています。
-- これがどんなふうにWebSocketとつながっているのかindex.htmlにあるJavaScriptと対応させてみてください。
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DraftChanged draft ->
      ( { model | draft = draft }
      , Cmd.none
      )

    Send ->
      ( { model | draft = "" }
      , sendMessage model.draft
      )

    Recv receiveModel ->
      ( { model
          | message = receiveModel.message
          , liffReceiveData = receiveModel.liffReceiveData
        }
      , Cmd.none
      )



-- SUBSCRIPTIONS
-- `messageReceiver`ポートを使って、JavaScriptから送られるメッセージを待ち受けています。
-- どうやってWebSocketとつながっているのかは、index.htmlファイルを見てください。
--


subscriptions : Model -> Sub Msg
subscriptions _ =
  dataReceiver Recv



-- VIEW


get : Result D.Error a -> D.Error -> LIFFReceiveData -> LIFFReceiveData
get r e l =
  l


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Elm liff sample" ]

    {- , p []
       [ text model.message ]
    -}
    , table []
        [ tbody []
            [ viewDataList model
            ]
        ]

    {- , input
           [ type_ "text"
           , placeholder "Draft"
           , onInput DraftChanged

           , on "keydown" (ifIsEnter Send)
           , value model.draft
           ]
           []
       , button [ onClick Send ] [ text "Send" ]
    -}
    ]


viewDataList : Model -> Html Msg
viewDataList model =
  div [] (List.map viewOneData model.liffReceiveData.data)


viewOneData : Array String -> Html Msg
viewOneData arr =
  tr []
    [ td [ style "border" "1px solid #333" ] [ text (viewPatternMatch (Array.get 0 arr)) ]
    , td [ style "border" "1px solid #333" ] [ text (viewPatternMatch (Array.get 1 arr)) ]
    ]


viewPatternMatch : Maybe String -> String
viewPatternMatch maybeA =
  case maybeA of
    Nothing ->
      ""

    Just a ->
      a



-- DETECT ENTER
{- ifIsEnter : msg -> D.Decoder msg
   ifIsEnter msg =
     D.field "key" D.string
       |> D.andThen
           (\key ->
             if key == "Enter" then
               D.succeed msg

             else
               D.fail "some other key"
           )
-}


type alias LIFFReceiveData =
  { tag : String
  , data : List (Array String)
  }



-- lIFFReceiveDataDecoder : Decoder LIFFReceiveData
-- lIFFReceiveDataDecoder =
--   D.succeed LIFFReceiveData
--     |> Json.Decode.Pipeline.required "tag" string
