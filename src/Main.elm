port module Main exposing (..)

import Array exposing (Array, fromList)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (Decoder, string, succeed)
import Json.Decode.Pipeline as P exposing (required)



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


type alias LIFFReceiveData =
  { tag : String
  , data : List (Array String)
  , decodedIdToken : String
  }


type alias Model =
  { draft : String
  , message : String
  , liffReceiveData : LIFFReceiveData
  }


init : () -> ( Model, Cmd Msg )
init flags =
  ( { draft = "", message = "", liffReceiveData = LIFFReceiveData "" [] "" }
  , Cmd.none
  )



-- UPDATE


type Msg
  = DraftChanged String
  | Send String
  | Recv Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DraftChanged draft ->
      ( { model | draft = draft }
      , Cmd.none
      )

    Send message ->
      ( model
      , sendMessage message
      )

    Recv receiveModel ->
      ( { model
          | message = receiveModel.message
          , liffReceiveData = receiveModel.liffReceiveData
        }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  dataReceiver Recv



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Elm liff sample" ]
    , p [] [ text "User Data" ]
    , table [ style "max-width" "100%" ]
        [ tbody []
            [ viewDataList model
            ]
        ]
    , p [] [ text "Decoded ID Token" ]
    , table []
        [ tbody []
            [ viewDecodedIdToken model
            ]
        ]
    , button [ onClick (Send "logout") ] [ text "logout" ]
    , button [ onClick (Send "shareTargetPicker") ] [ text "shareTargetPicker" ]
    , button [ onClick (Send "scanCodeV2") ] [ text "scanCodeV2" ]
    ]


viewDecodedIdToken : Model -> Html Msg
viewDecodedIdToken model =
  case D.decodeString viewIdTokenDecoder model.liffReceiveData.decodedIdToken of
    Ok idToken ->
      table []
        [ tbody []
            [ viewOneData (fromList [ "iss", idToken.iss ])
            , viewOneData (fromList [ "sub", idToken.sub ])
            , viewOneData (fromList [ "aud", idToken.aud ])
            , viewOneData (fromList [ "exp", String.fromInt idToken.exp ])
            , viewOneData (fromList [ "iat", String.fromInt idToken.iat ])
            , viewOneData (fromList [ "name", idToken.name ])
            , viewOneData (fromList [ "picture", idToken.picture ])
            ]
        ]

    Err err ->
      table []
        [ tbody []
            [ viewOneData (fromList [ "error", "" ])
            ]
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


type alias IdToken =
  { iss : String
  , sub : String
  , aud : String
  , exp : Int
  , iat : Int
  , name : String
  , picture : String
  }


viewIdTokenDecoder : Decoder IdToken
viewIdTokenDecoder =
  D.succeed IdToken
    |> P.required "iss" string
    |> P.required "sub" string
    |> P.required "aud" string
    |> P.required "exp" D.int
    |> P.required "iat" D.int
    |> P.required "name" string
    |> P.required "picture" string
