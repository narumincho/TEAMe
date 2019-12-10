module Main exposing (main)

import Browser
import Browser.Navigation
import Html
import Url





type Model
    = Model


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () _ _ =
    ( Model, Cmd.none )


view : Model -> Browser.Document Msg
view _ =
    { title = "TEAMe"
    , body = [ Html.text "TEAMe" ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )
