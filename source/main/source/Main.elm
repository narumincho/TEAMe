port module Main exposing (main)

import Api.Enum.Role
import Api.Mutation
import Browser
import Browser.Navigation
import Css
import Css.Animations
import Data
import Graphql.Http
import Graphql.SelectionSet
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import ManagerPage.MyPage
import ManagerPage.Team
import PageLocation
import PlayerPage.MyPage
import PlayerPage.Note
import PlayerPage.Team
import Style
import SubCommand
import Url



{-
   マイページ
       目標
       グラフ
       年間計画
       コンディションチェック


   ノート
     PDCA
     Pは目標と手段が見える
     PDCAライブラリ
     新しいPDCA/cycleを追加
       練習のタイプ
       練習の目標
       練習の手段
           手段によってかわる局面など

       文字、選択肢、複数回答

   チームページ
       今日の練習メニュー
       みんなの課題
       練習メニューの提出


-}


port setText : { id : String, text : String } -> Cmd msg


type Model
    = Model
        { mainModel : MainModel
        , navigationKey : Browser.Navigation.Key
        , notificationList : List String
        }


type MainModel
    = NoLogIn NoLogInData
    | WaitUser WaitUserData
    | NotSelectedRole NotSelectedRoleData
    | ManagerLogIn ManagerLogInData
    | PlayerLogIn PlayerLogInData


type alias NoLogInData =
    { logInViewModel : LogInViewModel
    , pageLocation : PageLocation.PageLocation
    }


type alias WaitUserData =
    { accessToken : Data.AccessToken
    , pageLocation : PageLocation.PageLocation
    }


type NotSelectedRoleData
    = NotSelectedRoleData
        { accessToken : Data.AccessToken
        , userData : Data.NoRoleUser
        }
    | NotSelectedRoleManager
        { accessToken : Data.AccessToken
        , userData : Data.NoRoleUser
        , teamName : String
        , creating : Bool
        }
    | NotSelectedRolePlayer
        { accessToken : Data.AccessToken
        , userData : Data.NoRoleUser
        , teamList : Maybe (List Data.TeamData)
        , joining : Bool
        }


type alias ManagerLogInData =
    { accessToken : Data.AccessToken
    , manager : Data.Manager
    , pageModel : ManagerPageModel
    }


type alias PlayerLogInData =
    { accessToken : Data.AccessToken
    , player : Data.Player
    , pageModel : PlayerPageModel
    }


type LogInViewModel
    = DisplayedLogInButton
    | WaitLogInUrl
    | ErrorLogIn String


type PlayerPageModel
    = PagePlayerMyPage PlayerPage.MyPage.Model
    | PagePlayerNote PlayerPage.Note.Model
    | PagePlayerTeam PlayerPage.Team.Model


type ManagerPageModel
    = PageManagerMyPage ManagerPage.MyPage.Model
    | PageManagerTeam ManagerPage.Team.Model


type Message
    = UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | MessagePlayerMyPage PlayerPage.MyPage.Message
    | MessagePlayerNote PlayerPage.Note.Message
    | MessagePlayerTeam PlayerPage.Team.Message
    | MessageManagerMyPage ManagerPage.MyPage.Message
    | MessageManagerTeam ManagerPage.Team.Message
    | RequestLineLogInUrl
    | ResponseLineLogInUrl (Result (Graphql.Http.Error String) String)
    | ResponseUserData (Result (Graphql.Http.Error Data.UserData) Data.UserData)
    | SelectRole Api.Enum.Role.Role
    | InputTeamName String
    | CreateTeam
    | CreateTeamResponse (Result (Graphql.Http.Error Data.UserData) Data.UserData)
    | AllTeamResponse (Result (Graphql.Http.Error (List Data.TeamData)) (List Data.TeamData))
    | SelectTeam Data.TeamId
    | JoinTeamResponse (Result (Graphql.Http.Error Data.UserData) Data.UserData)
    | LogInSampleUser Data.AccessToken


type alias Flag =
    Maybe String


main : Program Flag Model Message
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }


init : Flag -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Message )
init accessTokenFromLocalStorage url key =
    let
        ( accessTokenFromUrl, pageLocation ) =
            PageLocation.initFromUrl url
    in
    (case ( accessTokenFromLocalStorage, accessTokenFromUrl ) of
        ( _, Just accessToken ) ->
            waitUserModelAndCommand pageLocation accessToken
                |> Tuple.mapFirst
                    (\logInState ->
                        Model
                            { mainModel = logInState
                            , navigationKey = key
                            , notificationList = []
                            }
                    )

        ( Just accessToken, Nothing ) ->
            waitUserModelAndCommand pageLocation (Data.accessTokenFromString accessToken)
                |> Tuple.mapFirst
                    (\logInState ->
                        Model
                            { mainModel = logInState
                            , navigationKey = key
                            , notificationList = []
                            }
                    )

        ( Nothing, Nothing ) ->
            ( Model
                { mainModel =
                    NoLogIn
                        { logInViewModel = DisplayedLogInButton
                        , pageLocation = pageLocation
                        }
                , navigationKey = key
                , notificationList = []
                }
            , Cmd.none
            )
    )
        |> Tuple.mapSecond
            (\command ->
                Cmd.batch
                    [ command
                    , Browser.Navigation.replaceUrl key
                        (PageLocation.toUrlAsString pageLocation)
                    ]
            )


waitUserModelAndCommand : PageLocation.PageLocation -> Data.AccessToken -> ( MainModel, Cmd Message )
waitUserModelAndCommand pageLocation accessToken =
    ( WaitUser
        { accessToken = accessToken
        , pageLocation = pageLocation
        }
    , Graphql.Http.queryRequest Data.apiUrl (Data.getUserPrivateData accessToken)
        |> Graphql.Http.send ResponseUserData
    )


pageLocationToInitPlayerPageModel : Data.Player -> PageLocation.PageLocation -> ( PlayerPageModel, Cmd Message )
pageLocationToInitPlayerPageModel player pageLocation =
    case pageLocation of
        PageLocation.Top ->
            PlayerPage.MyPage.init player
                |> Tuple.mapBoth PagePlayerMyPage (Cmd.map MessagePlayerMyPage)

        PageLocation.PlayerNote ->
            PlayerPage.Note.init player
                |> Tuple.mapBoth PagePlayerNote (Cmd.map MessagePlayerNote)

        PageLocation.Team ->
            PlayerPage.Team.init player
                |> Tuple.mapBoth PagePlayerTeam (Cmd.map MessagePlayerTeam)

        _ ->
            PlayerPage.MyPage.init player
                |> Tuple.mapBoth PagePlayerMyPage (Cmd.map MessagePlayerMyPage)


pageLocationToInitManagerPageModel :
    Data.Manager
    -> PageLocation.PageLocation
    -> ( ManagerPageModel, SubCommand.SubCommand Message )
pageLocationToInitManagerPageModel manager pageLocation =
    case pageLocation of
        PageLocation.Top ->
            ManagerPage.MyPage.init manager
                |> Tuple.mapBoth PageManagerMyPage (SubCommand.map MessageManagerMyPage)

        PageLocation.Team ->
            ManagerPage.Team.init manager
                |> Tuple.mapBoth PageManagerTeam (SubCommand.map MessageManagerTeam)

        _ ->
            ManagerPage.MyPage.init manager
                |> Tuple.mapBoth PageManagerMyPage (SubCommand.map MessageManagerMyPage)


update : Message -> Model -> ( Model, Cmd Message )
update message (Model rec) =
    case message of
        UrlRequest urlRequest ->
            ( Model rec
            , case urlRequest of
                Browser.Internal url ->
                    Browser.Navigation.pushUrl rec.navigationKey (Url.toString url)

                Browser.External urlString ->
                    Browser.Navigation.load urlString
            )

        _ ->
            case rec.mainModel of
                NoLogIn noLogInRecord ->
                    updateNoLogIn message noLogInRecord
                        |> Tuple.mapFirst
                            (\newNoLogInRecord ->
                                Model
                                    { rec | mainModel = newNoLogInRecord }
                            )

                WaitUser waitUserDataRecord ->
                    let
                        ( newLogInState, newMessageList, cmd ) =
                            updateWaitUserData message waitUserDataRecord
                    in
                    ( Model
                        { rec
                            | mainModel = newLogInState
                            , notificationList = rec.notificationList ++ newMessageList
                        }
                    , cmd
                    )

                NotSelectedRole notSelectedRoleData ->
                    let
                        ( newLogInState, newMessageList, cmd ) =
                            updateNoSelectedRole message notSelectedRoleData
                    in
                    ( Model
                        { rec
                            | mainModel = newLogInState
                            , notificationList = rec.notificationList ++ newMessageList
                        }
                    , cmd
                    )

                ManagerLogIn managerData ->
                    let
                        ( newManagerLogInData, newMessageList, cmd ) =
                            updateManager message managerData
                    in
                    ( Model
                        { rec
                            | mainModel = ManagerLogIn newManagerLogInData
                            , notificationList = rec.notificationList ++ newMessageList
                        }
                    , cmd
                    )

                PlayerLogIn playerData ->
                    let
                        ( newPlayerData, newMessageList, cmd ) =
                            updatePlayer message playerData
                    in
                    ( Model
                        { rec
                            | mainModel = PlayerLogIn newPlayerData
                            , notificationList = rec.notificationList ++ newMessageList
                        }
                    , cmd
                    )


updateNoLogIn :
    Message
    -> NoLogInData
    -> ( MainModel, Cmd Message )
updateNoLogIn message noLogInRecord =
    case message of
        UrlChange url ->
            ( NoLogIn { noLogInRecord | pageLocation = PageLocation.fromUrl url }
            , Cmd.none
            )

        RequestLineLogInUrl ->
            ( NoLogIn
                { noLogInRecord
                    | logInViewModel = WaitLogInUrl
                }
            , Graphql.Http.mutationRequest Data.apiUrl
                (Api.Mutation.getLineLogInUrl
                    { path = PageLocation.toUrlAsString noLogInRecord.pageLocation }
                    |> Graphql.SelectionSet.map Data.urlAsStringFromGraphQLScalaValue
                )
                |> Graphql.Http.send ResponseLineLogInUrl
            )

        ResponseLineLogInUrl result ->
            case result of
                Ok url ->
                    ( NoLogIn noLogInRecord
                    , Browser.Navigation.load url
                    )

                Err errorMessage ->
                    ( NoLogIn { noLogInRecord | logInViewModel = ErrorLogIn "エラー" }
                    , Cmd.none
                    )

        LogInSampleUser accessToken ->
            waitUserModelAndCommand noLogInRecord.pageLocation accessToken

        _ ->
            ( NoLogIn noLogInRecord
            , Cmd.none
            )


updateWaitUserData : Message -> WaitUserData -> ( MainModel, List String, Cmd Message )
updateWaitUserData message waitUserData =
    case message of
        ResponseUserData userDataResult ->
            case userDataResult of
                Ok userData ->
                    responseUserData waitUserData userData

                Err _ ->
                    ( WaitUser waitUserData, [ "ユーザーの情報取得に失敗" ], Cmd.none )

        _ ->
            ( WaitUser waitUserData, [], Cmd.none )


responseUserData : WaitUserData -> Data.UserData -> ( MainModel, List String, Cmd Message )
responseUserData waitUserData userData =
    case userData of
        Data.NoRole noRoleUser ->
            ( NotSelectedRole
                (NotSelectedRoleData
                    { accessToken = waitUserData.accessToken
                    , userData = noRoleUser
                    }
                )
            , [ "はじまして!" ]
            , Cmd.none
            )

        Data.RoleManager manager ->
            let
                ( pageModel, command ) =
                    pageLocationToInitManagerPageModel manager waitUserData.pageLocation
                        |> Tuple.mapSecond subCommandToNewDataAndCommand
            in
            ( ManagerLogIn
                { accessToken = waitUserData.accessToken
                , manager =
                    command.newUser
                        |> Maybe.andThen Data.userGetManager
                        |> Maybe.withDefault manager
                , pageModel = pageModel
                }
            , [ "監督としてログイン成功!" ]
                ++ (case command.newNotification of
                        Just notification ->
                            [ notification ]

                        Nothing ->
                            []
                   )
            , command.command
            )

        Data.RolePlayer player ->
            let
                ( pageModel, command ) =
                    pageLocationToInitPlayerPageModel player waitUserData.pageLocation
            in
            ( PlayerLogIn
                { accessToken = waitUserData.accessToken
                , player = player
                , pageModel = pageModel
                }
            , [ "選手としてログイン成功!" ]
            , command
            )


updateNoSelectedRole : Message -> NotSelectedRoleData -> ( MainModel, List String, Cmd Message )
updateNoSelectedRole message notSelectedRoleData =
    case ( message, notSelectedRoleData ) of
        ( SelectRole role, NotSelectedRoleData record ) ->
            case role of
                Api.Enum.Role.Manager ->
                    ( NotSelectedRole
                        (NotSelectedRoleManager
                            { accessToken = record.accessToken
                            , userData = record.userData
                            , teamName = ""
                            , creating = False
                            }
                        )
                    , []
                    , Cmd.none
                    )

                Api.Enum.Role.Player ->
                    ( NotSelectedRole
                        (NotSelectedRolePlayer
                            { accessToken = record.accessToken
                            , userData = record.userData
                            , teamList = Nothing
                            , joining = False
                            }
                        )
                    , []
                    , Graphql.Http.queryRequest Data.apiUrl Data.getAllTeam
                        |> Graphql.Http.send AllTeamResponse
                    )

        ( InputTeamName teamName, NotSelectedRoleManager record ) ->
            ( NotSelectedRole (NotSelectedRoleManager { record | teamName = teamName })
            , []
            , Cmd.none
            )

        ( CreateTeam, NotSelectedRoleManager record ) ->
            ( NotSelectedRole (NotSelectedRoleManager { record | creating = True })
            , [ record.teamName ++ "を作成中……" ]
            , if Data.validateTeamName record.teamName then
                Graphql.Http.mutationRequest
                    Data.apiUrl
                    (Data.createTeamAndSetManagerRole record.accessToken record.teamName)
                    |> Graphql.Http.send CreateTeamResponse

              else
                Cmd.none
            )

        ( CreateTeamResponse response, NotSelectedRoleManager record ) ->
            case response of
                Ok newUserData ->
                    case newUserData of
                        Data.RoleManager managerUser ->
                            let
                                ( pageModel, newDataAndCommand ) =
                                    pageLocationToInitManagerPageModel managerUser PageLocation.Top
                                        |> Tuple.mapSecond subCommandToNewDataAndCommand
                            in
                            ( ManagerLogIn
                                { accessToken = record.accessToken
                                , manager =
                                    newDataAndCommand.newUser
                                        |> Maybe.andThen Data.userGetManager
                                        |> Maybe.withDefault managerUser
                                , pageModel = pageModel
                                }
                            , case newDataAndCommand.newNotification of
                                Just notification ->
                                    [ notification ]

                                Nothing ->
                                    []
                            , newDataAndCommand.command
                            )

                        _ ->
                            ( NotSelectedRole (NotSelectedRoleManager { record | creating = False })
                            , [ "チーム作成時にユーザ情報の変更に失敗しました" ]
                            , Cmd.none
                            )

                Err _ ->
                    ( NotSelectedRole (NotSelectedRoleManager { record | creating = False })
                    , [ "チームの作成に失敗しました" ]
                    , Cmd.none
                    )

        ( AllTeamResponse allTeamResult, NotSelectedRolePlayer record ) ->
            case allTeamResult of
                Ok allTeam ->
                    ( NotSelectedRole (NotSelectedRolePlayer { record | teamList = Just allTeam })
                    , []
                    , Cmd.none
                    )

                Err _ ->
                    ( NotSelectedRole (NotSelectedRolePlayer record)
                    , [ "チームの一覧取得に失敗" ]
                    , Cmd.none
                    )

        ( SelectTeam teamId, NotSelectedRolePlayer record ) ->
            ( NotSelectedRole (NotSelectedRolePlayer { record | joining = True })
            , [ "チームに参加中……" ]
            , Graphql.Http.mutationRequest
                Data.apiUrl
                (Data.joinTeamAndSetPlayerRole record.accessToken teamId)
                |> Graphql.Http.send JoinTeamResponse
            )

        ( JoinTeamResponse response, NotSelectedRolePlayer record ) ->
            case response of
                Ok newUserData ->
                    case newUserData of
                        Data.RolePlayer player ->
                            let
                                ( pageModel, command ) =
                                    pageLocationToInitPlayerPageModel player PageLocation.Top
                            in
                            ( PlayerLogIn
                                { accessToken = record.accessToken
                                , player = player
                                , pageModel = pageModel
                                }
                            , []
                            , command
                            )

                        _ ->
                            ( NotSelectedRole (NotSelectedRolePlayer { record | joining = False })
                            , [ "チーム参加時にユーザ情報の変更に失敗しました" ]
                            , Cmd.none
                            )

                Err _ ->
                    ( NotSelectedRole (NotSelectedRolePlayer { record | joining = False })
                    , [ "チームの参加に失敗しました" ]
                    , Cmd.none
                    )

        ( _, _ ) ->
            ( NotSelectedRole notSelectedRoleData
            , []
            , Cmd.none
            )


updateManager :
    Message
    -> ManagerLogInData
    -> ( ManagerLogInData, List String, Cmd Message )
updateManager message logInData =
    case ( message, logInData.pageModel ) of
        ( UrlChange url, _ ) ->
            let
                ( pageModel, newDataAndCommand ) =
                    pageLocationToInitManagerPageModel logInData.manager
                        (PageLocation.fromUrl url)
                        |> Tuple.mapSecond subCommandToNewDataAndCommand
            in
            ( { logInData
                | pageModel = pageModel
                , manager =
                    newDataAndCommand.newUser
                        |> Maybe.andThen Data.userGetManager
                        |> Maybe.withDefault logInData.manager
              }
            , case newDataAndCommand.newNotification of
                Just notification ->
                    [ notification ]

                Nothing ->
                    []
            , newDataAndCommand.command
            )

        ( MessageManagerMyPage pageMessage, PageManagerMyPage pageModel ) ->
            let
                ( newMyPageModel, newDataAndCommand ) =
                    ManagerPage.MyPage.update logInData.manager pageMessage pageModel
                        |> Tuple.mapSecond
                            (SubCommand.map MessageManagerMyPage
                                >> subCommandToNewDataAndCommand
                            )
            in
            ( { logInData
                | pageModel = PageManagerMyPage newMyPageModel
                , manager =
                    newDataAndCommand.newUser
                        |> Maybe.andThen Data.userGetManager
                        |> Maybe.withDefault logInData.manager
              }
            , []
            , newDataAndCommand.command
            )

        ( MessageManagerTeam pageMessage, PageManagerTeam pageModel ) ->
            let
                ( newPageModel, newDataAndCommand ) =
                    ManagerPage.Team.update pageMessage pageModel
                        |> Tuple.mapSecond
                            (SubCommand.map MessageManagerTeam
                                >> subCommandToNewDataAndCommand
                            )
            in
            ( { logInData | pageModel = PageManagerTeam newPageModel }
            , []
            , newDataAndCommand.command
            )

        ( _, _ ) ->
            ( logInData
            , []
            , Cmd.none
            )


updatePlayer : Message -> PlayerLogInData -> ( PlayerLogInData, List String, Cmd Message )
updatePlayer message logInData =
    case ( message, logInData.pageModel ) of
        ( UrlChange url, _ ) ->
            let
                ( pageModel, command ) =
                    pageLocationToInitPlayerPageModel logInData.player
                        (PageLocation.fromUrl url)
            in
            ( { logInData | pageModel = pageModel }
            , []
            , command
            )

        ( MessagePlayerMyPage pageMessage, PagePlayerMyPage pageModel ) ->
            let
                ( newMyPageModel, command ) =
                    PlayerPage.MyPage.update pageMessage pageModel
            in
            ( { logInData | pageModel = PagePlayerMyPage newMyPageModel }
            , []
            , command |> Cmd.map MessagePlayerMyPage
            )

        ( MessagePlayerNote pageMessage, PagePlayerNote pageModel ) ->
            let
                ( newPageModel, command ) =
                    PlayerPage.Note.update pageMessage pageModel
            in
            ( { logInData | pageModel = PagePlayerNote newPageModel }
            , []
            , command |> Cmd.map MessagePlayerNote
            )

        ( MessagePlayerTeam pageMessage, PagePlayerTeam pageModel ) ->
            let
                ( newPageModel, command ) =
                    PlayerPage.Team.update pageMessage pageModel
            in
            ( { logInData | pageModel = PagePlayerTeam newPageModel }
            , []
            , command |> Cmd.map MessagePlayerTeam
            )

        ( _, _ ) ->
            ( logInData, [], Cmd.none )


subCommandToNewDataAndCommand :
    SubCommand.SubCommand Message
    ->
        { newNotification : Maybe String
        , newTeam : Maybe Data.TeamData
        , newUser : Maybe Data.UserData
        , command : Cmd Message
        }
subCommandToNewDataAndCommand subCommand =
    case subCommand of
        SubCommand.AddNotification notification ->
            { newNotification = Just notification
            , newTeam = Nothing
            , newUser = Nothing
            , command = Cmd.none
            }

        SubCommand.ChangeInputText idAndText ->
            { newNotification = Nothing
            , newTeam = Nothing
            , newUser = Nothing
            , command = setText idAndText
            }

        SubCommand.Command command ->
            { newNotification = Nothing
            , newTeam = Nothing
            , newUser = Nothing
            , command = command
            }

        SubCommand.UpdateUser user ->
            { newNotification = Nothing
            , newTeam = Nothing
            , newUser = Just user
            , command = Cmd.none
            }

        SubCommand.UpdateTeam teamData ->
            { newNotification = Nothing
            , newTeam = Just teamData
            , newUser = Nothing
            , command = Cmd.none
            }

        SubCommand.None ->
            { newNotification = Nothing
            , newTeam = Nothing
            , newUser = Nothing
            , command = Cmd.none
            }


view : Model -> Browser.Document Message
view (Model record) =
    { title = "TEAMe"
    , body =
        [ case record.mainModel of
            NoLogIn noLogInRecord ->
                logInView noLogInRecord.logInViewModel

            WaitUser _ ->
                Html.Styled.text "ユーザーの情報を取得中…"

            NotSelectedRole notSelectedRoleData ->
                notSelectedRoleDataView notSelectedRoleData

            ManagerLogIn managerLogInData ->
                managerLogInView managerLogInData

            PlayerLogIn playerLogInData ->
                playerLogInView playerLogInData
        , notificationView record.notificationList
        ]
            |> List.map Html.Styled.toUnstyled
    }


logInView : LogInViewModel -> Html.Styled.Html Message
logInView model =
    Html.Styled.div
        []
        ([ Html.Styled.div
            []
            [ Html.Styled.text "ログイン画面" ]
         ]
            ++ (case model of
                    DisplayedLogInButton ->
                        [ Html.Styled.div
                            []
                            [ Html.Styled.text "TEAMeを使うにはログインが必要です" ]
                        , lineLogInButton
                        , Style.normalButton
                            (LogInSampleUser (Data.accessTokenFromString "862631c80456b3045b67e3c5031ca1e7f50140c9b26e662c"))
                            "Aさん でログイン"
                        , Style.normalButton
                            (LogInSampleUser (Data.accessTokenFromString "53b657df6b6d178a1c77d964c3c6d91e68c6bce10f38ead0"))
                            "Bさん でログイン"
                        , Style.normalButton
                            (LogInSampleUser (Data.accessTokenFromString "754f8dc48151667dd5cb80ec092aade54fc32deb93ddad70"))
                            "Cさん でログイン"
                        , Style.normalButton
                            (LogInSampleUser (Data.accessTokenFromString "a893a1988368e272453be3640d8b868aa5095ca4f2d7de789"))
                            "Dさん でログイン"
                        ]

                    WaitLogInUrl ->
                        [ Html.Styled.div
                            []
                            [ Html.Styled.text "ログインへのURLを取得中" ]
                        ]

                    ErrorLogIn errorMessage ->
                        [ Html.Styled.div
                            []
                            [ Html.Styled.text ("ログインに失敗しました。" ++ errorMessage) ]
                        ]
               )
        )


lineLogInButton : Html.Styled.Html Message
lineLogInButton =
    Html.Styled.button
        [ Html.Styled.Attributes.css
            [ Css.backgroundColor (Css.rgb 0 195 0)
            , Css.border2 Css.zero Css.none
            , Css.borderRadius (Css.px 8)
            , Css.padding Css.zero
            , Css.width (Css.pct 100)
            , Css.cursor Css.pointer
            ]
        , Html.Styled.Events.onClick RequestLineLogInUrl
        ]
        [ Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.property "display" "grid"
                , Css.property "grid-auto-flow" "column"
                , Css.property "grid-template-columns" "max-content 1fr"
                , Css.alignItems Css.center
                ]
            ]
            [ Html.Styled.img
                [ Html.Styled.Attributes.src "/assets/line-icon.png"
                , Html.Styled.Attributes.alt "LINEのロゴ"
                , Html.Styled.Attributes.css
                    [ Css.width (Css.px 97)
                    , Css.height (Css.px 96)
                    , Css.boxSizing Css.borderBox
                    , Css.borderRight3 (Css.px 1) Css.solid (Css.rgb 0 179 0)
                    ]
                ]
                []
            , Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.color (Css.rgb 255 255 255)
                    , Css.padding (Css.px 8)
                    , Css.fontSize (Css.rem 1.5)
                    ]
                ]
                [ Html.Styled.text "LINEでログイン" ]
            ]
        ]


notSelectedRoleDataView : NotSelectedRoleData -> Html.Styled.Html Message
notSelectedRoleDataView notSelectedRoleData =
    case notSelectedRoleData of
        NotSelectedRoleData record ->
            Html.Styled.div
                []
                [ Style.userImage
                    { name = record.userData.name
                    , imageFileHash = record.userData.imageFileHash
                    }
                , Html.Styled.text ("はじまして、" ++ record.userData.name ++ "さん。監督か 選手かを選んでください")
                , Style.normalButton (SelectRole Api.Enum.Role.Manager) "監督"
                , Style.normalButton (SelectRole Api.Enum.Role.Player) "選手"
                ]

        NotSelectedRoleManager record ->
            createTeamView record.creating record.teamName

        NotSelectedRolePlayer record ->
            joinTeamView record.userData record.teamList record.joining


createTeamView : Bool -> String -> Html.Styled.Html Message
createTeamView creating teamName =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.property "display" "grid"
            , Css.width (Css.pct 100)
            , Css.property "gap" "1rem"
            , Css.property "align-content" "center"
            , Css.height (Css.pct 100)
            ]
        ]
        (if creating then
            [ Html.Styled.div [] [ Html.Styled.text "チームを作成中……" ] ]

         else
            [ Html.Styled.div [] [ Html.Styled.text "監督はまず、チームをつくります。チーム名は?" ]
            , Style.inputText "team-name" "team-name" InputTeamName
            , Style.conditionButton
                (if Data.validateTeamName teamName then
                    Just CreateTeam

                 else
                    Nothing
                )
                "作成"
            ]
        )


joinTeamView : Data.NoRoleUser -> Maybe (List Data.TeamData) -> Bool -> Html.Styled.Html Message
joinTeamView noRoleUser teamListMaybe joining =
    Html.Styled.div
        []
        (if joining then
            [ Html.Styled.text "チームに参加中" ]

         else
            [ Html.Styled.text "選手は所属するチームを選んでもらいます"
            , Html.Styled.div
                []
                (case teamListMaybe of
                    Just teamList ->
                        teamList
                            |> List.map
                                (\team ->
                                    Style.normalButton
                                        (SelectTeam team.id)
                                        team.name
                                )

                    Nothing ->
                        [ Html.Styled.text "チーム一覧を取得中……" ]
                )
            ]
        )


managerLogInView : ManagerLogInData -> Html.Styled.Html Message
managerLogInView logInData =
    case logInData.pageModel of
        PageManagerMyPage model ->
            ManagerPage.MyPage.view logInData.manager model
                |> Html.Styled.map MessageManagerMyPage

        PageManagerTeam model ->
            ManagerPage.Team.view model
                |> Html.Styled.map MessageManagerTeam


playerLogInView : PlayerLogInData -> Html.Styled.Html Message
playerLogInView logInData =
    case logInData.pageModel of
        PagePlayerMyPage model ->
            PlayerPage.MyPage.view model
                |> Html.Styled.map MessagePlayerMyPage

        PagePlayerNote model ->
            PlayerPage.Note.view model
                |> Html.Styled.map MessagePlayerNote

        PagePlayerTeam model ->
            PlayerPage.Team.view model
                |> Html.Styled.map MessagePlayerTeam


notificationView : List String -> Html.Styled.Html Message
notificationView notificationList =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Style.displayGrid
            , Style.gridCell { x = 0, y = 0, width = 1, height = 1 }
            , Css.justifyContent Css.end
            , Style.alignContentEnd
            , Css.paddingBottom (Css.rem 5)
            ]
        ]
        (notificationList
            |> List.map
                (\notification ->
                    Html.Styled.div
                        [ Html.Styled.Attributes.css
                            [ Css.backgroundColor Style.themeColor
                            , Css.padding (Css.rem 0.2)
                            , Css.animationName
                                (Css.Animations.keyframes
                                    [ ( 0
                                      , [ Css.Animations.backgroundColor Style.themeColor
                                        , Css.Animations.property "color" "#000"
                                        ]
                                      )
                                    , ( 100
                                      , [ Css.Animations.backgroundColor (Css.rgba 0 0 0 0)
                                        , Css.Animations.property "color" "#0000"
                                        ]
                                      )
                                    ]
                                )
                            , Css.animationDelay (Css.ms 0)
                            , Css.animationIterationCount (Css.int 1)
                            , Css.animationDuration (Css.ms 5000)
                            , Style.animationFillModeForwards
                            ]
                        ]
                        [ Html.Styled.text notification ]
                )
        )
