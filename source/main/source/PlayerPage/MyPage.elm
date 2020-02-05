module PlayerPage.MyPage exposing (Command, Message, Model, init, update, view)

import Data
import Html.Styled
import Style


type Model
    = Model { player : Data.Player }


type Message
    = Message


type Command
    = Command


init : Data.Player -> ( Model, Maybe Command )
init player =
    ( Model { player = player }, Nothing )


update : Message -> Model -> ( Model, Maybe Command )
update _ model =
    ( model, Nothing )


view : Model -> Html.Styled.Html Message
view (Model record) =
    Html.Styled.div
        []
        [ Style.header (Just (Data.RolePlayer record.player))
        , Html.Styled.text "選手のマイページ"
        , Style.playerBottomNavigation
        ]
