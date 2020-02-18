module PlayerPage.Team exposing (Message, Model, init, update, view)

import Data
import Graphql.Http
import Html.Styled as S
import Maybe.Extra
import PageLocation
import Style
import SubCommand
import Time


type Model
    = Model
        { updatingGoal : Bool
        , nextGoal : String
        , updatingInformation : Bool
        , nextInformation : String
        , playerList : Maybe (List Data.Player)
        }


type Message
    = InputGoal String
    | CancelUpdateGoal
    | UpdateGoal
    | UpdateGoalResponse (Result (Graphql.Http.Error Data.TeamData) Data.TeamData)
    | InputInformation String
    | CancelUpdateInformation
    | UpdateInformation
    | UpdateInformationResponse (Result (Graphql.Http.Error Data.TeamData) Data.TeamData)
    | TeamUserResponse (Result (Graphql.Http.Error (List Data.UserData)) (List Data.UserData))


init : Data.TeamData -> ( Model, SubCommand.SubCommand Message )
init teamData =
    ( Model
        { updatingGoal = False
        , nextGoal = teamData.goal
        , updatingInformation = False
        , nextInformation = teamData.information
        , playerList = Nothing
        }
    , SubCommand.batch
        [ SubCommand.setInputText { id = goalInputDomId, text = teamData.goal }
        , SubCommand.setInputText { id = informationInputDomId, text = teamData.information }
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

        InputInformation text ->
            ( Model { record | nextInformation = text }
            , SubCommand.none
            )

        CancelUpdateInformation ->
            ( Model { record | nextInformation = teamData.information }
            , SubCommand.setInputText { id = informationInputDomId, text = teamData.information }
            )

        UpdateInformation ->
            ( Model { record | updatingInformation = True }
            , SubCommand.addCommand
                (Graphql.Http.mutationRequest Data.apiUrl
                    (Data.updateTeamInformation accessToken record.nextInformation)
                    |> Graphql.Http.send UpdateInformationResponse
                )
            )

        UpdateInformationResponse response ->
            case response of
                Ok newTeam ->
                    ( Model { record | updatingInformation = False }
                    , SubCommand.batch
                        [ SubCommand.updateTeam newTeam
                        , SubCommand.setInputText
                            { id = informationInputDomId
                            , text = newTeam.information
                            }
                        ]
                    )

                Err _ ->
                    ( Model { record | updatingInformation = False }
                    , SubCommand.addNotification "チームの共有事項の変更に失敗しました"
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


view : Time.Zone -> Data.Player -> Data.TeamData -> Model -> S.Html Message
view timeZone player teamData (Model record) =
    Style.pageContainer
        [ Style.header (Just (Data.RolePlayer player))
        , mainView timeZone teamData (Model record)
        , Style.playerBottomNavigation PageLocation.Team
        ]


mainView : Time.Zone -> Data.TeamData -> Model -> S.Html Message
mainView timeZone teamData (Model record) =
    Style.pageMainViewContainer
        [ goalView teamData (Model record)
        , informationView teamData (Model record)
        , Style.teamPlayerListView timeZone record.playerList
        ]


goalView : Data.TeamData -> Model -> S.Html Message
goalView teamData (Model record) =
    S.div
        []
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
        )


informationView : Data.TeamData -> Model -> S.Html Message
informationView teamData (Model record) =
    S.div
        []
        ([ Style.goalTitle "チームの共有事項"
         ]
            ++ (if record.updatingInformation then
                    [ S.div [] [ S.text "チームの共有事項を変更中……" ], Style.loading ]

                else
                    [ Style.multiLineTextBox informationInputDomId "information" InputInformation ]
                        ++ (if record.nextInformation /= teamData.information then
                                [ S.div
                                    []
                                    [ Style.normalButton CancelUpdateInformation "キャンセル"
                                    , Style.normalButton UpdateInformation "変更"
                                    ]
                                ]

                            else
                                []
                           )
               )
        )


goalInputDomId : String
goalInputDomId =
    "team-goal"


informationInputDomId : String
informationInputDomId =
    "team-information"
