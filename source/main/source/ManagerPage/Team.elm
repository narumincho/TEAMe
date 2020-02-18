module ManagerPage.Team exposing (Message, Model, init, update, view)

import Css
import Data
import Graphql.Http
import Html.Styled as S
import Html.Styled.Attributes as A
import Maybe.Extra
import PageLocation
import Style
import SubCommand
import Time


type Model
    = Model { updatingGoal : Bool, nextGoal : String, playerList : Maybe (List Data.Player) }


type Message
    = InputGoal String
    | CancelUpdateGoal
    | UpdateGoal
    | UpdateGoalResponse (Result (Graphql.Http.Error Data.TeamData) Data.TeamData)
    | InputInformation String
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

        InputInformation _ ->
            ( Model record
            , SubCommand.none
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


view : Time.Zone -> Data.Manager -> Data.TeamData -> Model -> S.Html Message
view timeZone manager teamData (Model record) =
    Style.pageContainer
        [ Style.header (Just (Data.RoleManager manager))
        , mainView timeZone teamData (Model record)
        , Style.managerBottomNavigation PageLocation.Team
        ]


mainView : Time.Zone -> Data.TeamData -> Model -> S.Html Message
mainView timeZone teamData (Model record) =
    Style.pageMainViewContainer
        ([ Style.goalTitle "チーム目標"
         ]
            ++ (if record.updatingGoal then
                    [ S.div [] [ S.text "チーム目標を変更中……" ], Style.loading ]

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
            ++ [ Style.teamPlayerListView timeZone record.playerList ]
        )


goalInputDomId : String
goalInputDomId =
    "team-goal"
