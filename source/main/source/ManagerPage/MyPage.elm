module ManagerPage.MyPage exposing (Message(..), Model, init, update, view)

import Data
import Graphql.Http
import Html.Styled as S
import PageLocation
import Style
import SubCommand


type Model
    = Model { updatingGoal : Bool, nextGoal : String }


type Message
    = InputGoal String
    | CancelUpdateGoal
    | UpdateGoal
    | UpdateGoalResponse (Result (Graphql.Http.Error Data.UserData) Data.UserData)


init : Data.Manager -> ( Model, SubCommand.SubCommand Message )
init manager =
    ( Model { updatingGoal = False, nextGoal = Data.managerGetGoal manager }
    , SubCommand.setInputText { id = goalInputDomId, text = Data.managerGetGoal manager }
    )


update : Data.AccessToken -> Data.Manager -> Message -> Model -> ( Model, SubCommand.SubCommand Message )
update accessToken manager message (Model record) =
    case message of
        InputGoal goal ->
            ( Model { record | nextGoal = goal }
            , SubCommand.none
            )

        CancelUpdateGoal ->
            ( Model { record | nextGoal = Data.managerGetGoal manager }
            , SubCommand.none
            )

        UpdateGoal ->
            ( Model { record | updatingGoal = True }
            , SubCommand.addCommand
                (Graphql.Http.mutationRequest Data.apiUrl
                    (Data.updatePersonalGoal accessToken record.nextGoal)
                    |> Graphql.Http.send UpdateGoalResponse
                )
            )

        UpdateGoalResponse response ->
            case response of
                Ok newUser ->
                    ( Model { record | updatingGoal = False }
                    , SubCommand.batch
                        [ SubCommand.updateUser newUser
                        , SubCommand.setInputText
                            { id = goalInputDomId
                            , text =
                                newUser
                                    |> Data.userGetManager
                                    |> Maybe.map Data.managerGetGoal
                                    |> Maybe.withDefault "?"
                            }
                        ]
                    )

                Err _ ->
                    ( Model { record | updatingGoal = False }
                    , SubCommand.addNotification "指導目標の変更に失敗しました"
                    )


view : Data.Manager -> Model -> S.Html Message
view manager model =
    Style.pageContainer
        [ Style.header (Just (Data.RoleManager manager))
        , mainView manager model
        , Style.managerBottomNavigation PageLocation.Top
        ]


mainView : Data.Manager -> Model -> S.Html Message
mainView manager (Model record) =
    Style.pageMainViewContainer
        ([ Style.goalTitle "指導目標"
         ]
            ++ (if record.updatingGoal then
                    [ S.div [] [ S.text "指導目標を変更中……" ] ]

                else
                    [ Style.inputText goalInputDomId "goal" InputGoal ]
                        ++ (if record.nextGoal /= Data.managerGetGoal manager then
                                [ S.div
                                    []
                                    [ Style.normalButton CancelUpdateGoal "キャンセル"
                                    , Style.normalButton UpdateGoal "変更"
                                    ]
                                ]

                            else
                                []
                           )
               )
        )


goalInputDomId : String
goalInputDomId =
    "goal"
