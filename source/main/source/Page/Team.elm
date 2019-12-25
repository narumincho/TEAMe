module Page.Team exposing (Command, Message, Model, init, update, view)

import Html.Styled


type Model
    = Model


type Message
    = Message


type Command
    = Command


update : Message -> Model -> ( Model, Maybe Command )
update message model =
    ( model, Nothing )


init : ( Model, Maybe Command )
init =
    ( Model, Nothing )


view : Model -> Html.Styled.Html Message
view _ =
    Html.Styled.text "チーム"
