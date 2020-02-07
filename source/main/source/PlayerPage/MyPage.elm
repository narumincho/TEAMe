module PlayerPage.MyPage exposing (Message, Model, init, update, view)

import Data
import Html.Styled
import PageLocation
import Style


type Model
    = Model { player : Data.Player }


type Message
    = Message


init : Data.Player -> ( Model, Cmd Message )
init player =
    ( Model { player = player }, Cmd.none )


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    ( model, Cmd.none )


view : Model -> Html.Styled.Html Message
view (Model record) =
    Style.pageContainer
        [ Style.header (Just (Data.RolePlayer record.player))
        , Style.pageMainViewContainer [ Html.Styled.text "選手のマイページ" ]
        , Style.playerBottomNavigation PageLocation.Top
        ]
