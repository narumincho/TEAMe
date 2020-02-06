module PlayerPage.Team exposing (Message, Model, init, update, view)

import Data
import Html.Styled as S
import Style


type Model
    = Model { player : Data.Player }


type Message
    = Message


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    ( model, Cmd.none )


init : Data.Player -> ( Model, Cmd Message )
init player =
    ( Model { player = player }, Cmd.none )


view : Model -> S.Html Message
view (Model record) =
    S.div
        []
        [ Style.header (Just (Data.RolePlayer record.player))
        , S.div [] [ S.text "選手のチームページ" ]
        , Style.playerBottomNavigation
        ]
