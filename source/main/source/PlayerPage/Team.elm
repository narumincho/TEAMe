module PlayerPage.Team exposing (Message, Model, init, update, view)

import Data
import Html.Styled as S
import PageLocation
import Style
import SubCommand


type Model
    = Model { player : Data.Player }


type Message
    = Message


update : Message -> Model -> ( Model, SubCommand.SubCommand Message )
update message model =
    ( model, SubCommand.none )


init : Data.Player -> ( Model, SubCommand.SubCommand Message )
init player =
    ( Model { player = player }, SubCommand.none )


view : Model -> S.Html Message
view (Model record) =
    Style.pageContainer
        [ Style.header (Just (Data.RolePlayer record.player))
        , Style.pageMainViewContainer [ S.text "選手のチームページ" ]
        , Style.playerBottomNavigation PageLocation.Team
        ]
