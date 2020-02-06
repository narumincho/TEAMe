module ManagerPage.Team exposing (Message(..), Model, init, update, view)

import Data
import Html.Styled as S
import Style
import SubCommand


type Model
    = Model { manager : Data.Manager }


type Message
    = Message


init : Data.Manager -> ( Model, SubCommand.SubCommand Message )
init manager =
    ( Model { manager = manager }, SubCommand.None )


update : Message -> Model -> ( Model, SubCommand.SubCommand Message )
update message model =
    ( model, SubCommand.None )


view : Model -> S.Html Message
view (Model record) =
    Style.pageContainer
        [ Style.header (Just (Data.RoleManager record.manager))
        , Style.pageMainViewContainer [ S.text "導者のチームページ" ]
        , Style.managerBottomNavigation
        ]
