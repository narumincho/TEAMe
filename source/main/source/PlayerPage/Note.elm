module PlayerPage.Note exposing (Command, Message, Model, init, update, view)

import Data
import Html.Styled as S
import Style


type Model
    = Model { player : Data.Player }


type Message
    = Message


type Command
    = Command


update : Message -> Model -> ( Model, Maybe Command )
update _ model =
    ( model, Nothing )


init : Data.Player -> ( Model, Maybe Command )
init player =
    ( Model { player = player }, Nothing )


view : Model -> S.Html Message
view (Model record) =
    S.div
        []
        [ Style.header (Just (Data.RolePlayer record.player))
        , S.div [] [ S.text "PDCAを見るためのノート" ]
        , Style.playerBottomNavigation
        ]
