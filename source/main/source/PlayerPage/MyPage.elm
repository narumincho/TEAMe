module PlayerPage.MyPage exposing (Message, Model, init, update, view)

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


init : Data.Player -> ( Model, SubCommand.SubCommand Message )
init player =
    ( Model { updatingGoal = False, nextGoal = Data.playerGetGoal player }
    , SubCommand.setInputText
        { id = goalInputDomId, text = Data.playerGetGoal player }
    )


update : Data.AccessToken -> Data.Player -> Message -> Model -> ( Model, SubCommand.SubCommand Message )
update accessToken player message (Model record) =
    case message of
        InputGoal goal ->
            ( Model { record | nextGoal = goal }
            , SubCommand.none
            )

        CancelUpdateGoal ->
            ( Model { record | nextGoal = Data.playerGetGoal player }
            , SubCommand.setInputText { id = goalInputDomId, text = Data.playerGetGoal player }
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
                                    |> Data.userGetPlayer
                                    |> Maybe.map Data.playerGetGoal
                                    |> Maybe.withDefault "?"
                            }
                        ]
                    )

                Err _ ->
                    ( Model { record | updatingGoal = False }
                    , SubCommand.addNotification "個人目標の変更に失敗しました"
                    )


view : Data.Player -> Model -> S.Html Message
view player model =
    Style.pageContainer
        [ Style.header (Just (Data.RolePlayer player))
        , mainView player model
        , Style.playerBottomNavigation PageLocation.Top
        ]


mainView : Data.Player -> Model -> S.Html Message
mainView player (Model record) =
    Style.pageMainViewContainer
        ([ Style.goalTitle "個人目標"
         ]
            ++ (if record.updatingGoal then
                    [ S.div [] [ S.text "個人目標を変更中……" ] ]

                else
                    [ Style.inputText goalInputDomId "goal" InputGoal ]
                        ++ (if record.nextGoal /= Data.playerGetGoal player then
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
