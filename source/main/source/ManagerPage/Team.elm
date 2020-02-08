module ManagerPage.Team exposing (Message, Model, init, update, view)

import Data
import Graphql.Http
import Html.Styled as S
import Maybe.Extra
import PageLocation
import Style
import SubCommand


type Model
    = Model { updatingGoal : Bool, nextGoal : String, playerList : Maybe (List Data.Player) }


type Message
    = InputGoal String
    | CancelUpdateGoal
    | UpdateGoal
    | UpdateGoalResponse (Result (Graphql.Http.Error Data.TeamData) Data.TeamData)
    | TeamUserResponse (Result (Graphql.Http.Error (List Data.UserData)) (List Data.UserData))


init : Data.TeamData -> ( Model, SubCommand.SubCommand Message )
init teamData =
    ( Model { updatingGoal = False, nextGoal = teamData.goal, playerList = Nothing }
    , SubCommand.batch
        [ SubCommand.setInputText { id = goalInputDomId, text = teamData.goal }
        , Graphql.Http.queryRequest Data.apiUrl (Data.getUserByUserIdList teamData.playerIdList)
            |> Graphql.Http.send TeamUserResponse
            |> SubCommand.addCommand
        ]
    )


update : Data.AccessToken -> Data.TeamData -> Message -> Model -> ( Model, SubCommand.SubCommand Message )
update accessToken teamData message (Model record) =
    case message of
        InputGoal goal ->
            ( Model { record | nextGoal = goal }
            , SubCommand.none
            )

        CancelUpdateGoal ->
            ( Model { record | nextGoal = teamData.goal }
            , SubCommand.setInputText { id = goalInputDomId, text = teamData.goal }
            )

        UpdateGoal ->
            ( Model { record | updatingGoal = True }
            , SubCommand.addCommand
                (Graphql.Http.mutationRequest Data.apiUrl
                    (Data.updateTeamGoal accessToken record.nextGoal)
                    |> Graphql.Http.send UpdateGoalResponse
                )
            )

        UpdateGoalResponse response ->
            case response of
                Ok newTeam ->
                    ( Model { record | updatingGoal = False }
                    , SubCommand.batch
                        [ SubCommand.updateTeam newTeam
                        , SubCommand.setInputText
                            { id = goalInputDomId
                            , text = newTeam.goal
                            }
                        ]
                    )

                Err _ ->
                    ( Model { record | updatingGoal = False }
                    , SubCommand.addNotification "指導目標の変更に失敗しました"
                    )

        TeamUserResponse response ->
            case response of
                Ok playerList ->
                    ( Model
                        { record
                            | playerList = playerList |> List.map Data.userGetPlayer |> Maybe.Extra.values |> Just
                        }
                    , SubCommand.none
                    )

                Err _ ->
                    ( Model record
                    , SubCommand.addNotification "チームのメンバー取得に失敗しました"
                    )


view : Data.Manager -> Data.TeamData -> Model -> S.Html Message
view manager teamData (Model record) =
    Style.pageContainer
        [ Style.header (Just (Data.RoleManager manager))
        , mainView teamData (Model record)
        , Style.managerBottomNavigation PageLocation.Team
        ]


mainView : Data.TeamData -> Model -> S.Html Message
mainView teamData (Model record) =
    Style.pageMainViewContainer
        ([ Style.goalTitle "チーム目標"
         ]
            ++ (if record.updatingGoal then
                    [ S.div [] [ S.text "チーム目標を変更中……" ] ]

                else
                    [ Style.inputText goalInputDomId "goal" InputGoal ]
                        ++ (if record.nextGoal /= teamData.goal then
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
            ++ (case record.playerList of
                    Just playerList ->
                        playerList
                            |> List.map
                                (\player ->
                                    S.div []
                                        ([ Style.userImage
                                            { name = Data.playerGetName player
                                            , imageFileHash = Data.playerGetImageFileHash player
                                            }
                                         , S.div [] [ S.text (Data.playerGetName player) ]
                                         ]
                                            ++ (player
                                                    |> Data.playerGetCycleDataList
                                                    |> List.map
                                                        (\cycle ->
                                                            S.div []
                                                                [ S.text
                                                                    ("P="
                                                                        ++ cycle.plan
                                                                        ++ " D="
                                                                        ++ cycle.do
                                                                        ++ " C="
                                                                        ++ cycle.check
                                                                        ++ " A="
                                                                        ++ cycle.act
                                                                    )
                                                                ]
                                                        )
                                               )
                                        )
                                )

                    Nothing ->
                        [ S.div [] [ S.text "選手の情報を読込中" ] ]
               )
        )


goalInputDomId : String
goalInputDomId =
    "team-goal"
