module ManagerPage.Team exposing (Message(..), Model, init, update, view)

import Data
import Html.Styled as S
import Style


type Model
    = Model { manager : Data.Manager }


type Message
    = Message


init : Data.Manager -> ( Model, Cmd Message )
init manager =
    ( Model { manager = manager }, Cmd.none )


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    ( model, Cmd.none )


view : Model -> S.Html Message
view (Model record) =
    S.div
        []
        [ Style.header (Just (Data.RoleManager record.manager))
        , S.text "導者のチームページ"
        , Style.managerBottomNavigation
        ]
