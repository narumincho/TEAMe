module Page.MyPage exposing (Command, Message, Model, init, update, view)

import Html.Styled


type Model
    = Model


type Message
    = Message


type Command
    = Command


init : ( Model, Maybe Command )
init =
    ( Model, Nothing )


update : Message -> Model -> ( Model, Maybe Command )
update _ _ =
    ( Model, Nothing )


view : Model -> Html.Styled.Html Message
view _ =
    Html.Styled.text "マイページ"
