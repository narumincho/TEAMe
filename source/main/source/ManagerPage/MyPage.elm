module ManagerPage.MyPage exposing (Message(..), Model, init, update, view)

import Data
import Html.Styled as S
import Style
import SubCommand


type Model
    = Model { updatingGoal : Bool, nextGoal : String }


type Message
    = InputGoal String
    | CancelUpdateGoal
    | UpdateGoal
    | UpdateGoalResponse


init : Data.Manager -> ( Model, SubCommand.SubCommand Message )
init manager =
    ( Model { updatingGoal = False, nextGoal = Data.managerGetGoal manager }
    , SubCommand.ChangeInputText { id = goalInputDomId, text = Data.managerGetGoal manager }
    )


update : Data.Manager -> Message -> Model -> ( Model, SubCommand.SubCommand Message )
update manager message (Model record) =
    case message of
        InputGoal goal ->
            ( Model { record | nextGoal = goal }
            , SubCommand.None
            )

        CancelUpdateGoal ->
            ( Model { record | nextGoal = Data.managerGetGoal manager }
            , SubCommand.None
            )

        UpdateGoal ->
            ( Model record
            , SubCommand.None
            )

        UpdateGoalResponse ->
            ( Model record
            , SubCommand.None
            )


view : Data.Manager -> Model -> S.Html Message
view manager model =
    Style.pageContainer
        [ Style.header (Just (Data.RoleManager manager))
        , mainView manager model
        , Style.managerBottomNavigation
        ]


mainView : Data.Manager -> Model -> S.Html Message
mainView manager (Model record) =
    Style.pageMainViewContainer
        ([ S.div [] [ S.text "指導目標" ]
         , Style.inputText goalInputDomId "goal" InputGoal
         ]
            ++ (if record.nextGoal /= Data.managerGetGoal manager then
                    [ Style.normalButton CancelUpdateGoal "キャンセル"
                    , Style.normalButton UpdateGoal "変更"
                    ]

                else
                    []
               )
        )


goalInputDomId : String
goalInputDomId =
    "goal"
