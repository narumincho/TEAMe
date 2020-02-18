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
import Task
import Time
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
        , timeZone : Maybe Time.Zone
        }


type MainModel
    = NoLogIn NoLogInData
    | WaitUser WaitUserData
    | NotSelectedRole NotSelectedRoleData
    | ManagerWaitTeamData
        { accessToken : Data.AccessToken
        , manager : Data.Manager
        , pageLocation : PageLocation.PageLocation
        }
    | PlayerLogInWaitTeamData
        { accessToken : Data.AccessToken
        , player : Data.Player
        , pageLocation : PageLocation.PageLocation
        }
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
    , team : Data.TeamData
    }


type alias PlayerLogInData =
    { accessToken : Data.AccessToken
    , player : Data.Player
    , pageModel : PlayerPageModel
    , team : Data.TeamData
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
    | TeamResponse (Result (Graphql.Http.Error Data.TeamData) Data.TeamData)
    | ResponseTimeZone Time.Zone


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
                            , timeZone = Nothing
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
                            , timeZone = Nothing
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
                , timeZone = Nothing
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
                    , Time.here |> Task.perform ResponseTimeZone
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


pageLocationToInitPlayerPageModel : Data.TeamData -> Data.Player -> PageLocation.PageLocation -> ( PlayerPageModel, SubCommand.SubCommand Message )
pageLocationToInitPlayerPageModel teamData player pageLocation =
    case pageLocation of
        PageLocation.Top ->
            PlayerPage.MyPage.init player
                |> Tuple.mapBoth PagePlayerMyPage (SubCommand.map MessagePlayerMyPage)

        PageLocation.PlayerNote ->
            PlayerPage.Note.init player
                |> Tuple.mapBoth PagePlayerNote (SubCommand.map MessagePlayerNote)

        PageLocation.Team ->
            PlayerPage.Team.init teamData
                |> Tuple.mapBoth PagePlayerTeam (SubCommand.map MessagePlayerTeam)

        _ ->
            PlayerPage.MyPage.init player
                |> Tuple.mapBoth PagePlayerMyPage (SubCommand.map MessagePlayerMyPage)


pageLocationToInitManagerPageModel :
    Data.TeamData
    -> Data.Manager
    -> PageLocation.PageLocation
    -> ( ManagerPageModel, SubCommand.SubCommand Message )
pageLocationToInitManagerPageModel teamData manager pageLocation =
    case pageLocation of
        PageLocation.Top ->
            ManagerPage.MyPage.init manager
                |> Tuple.mapBoth PageManagerMyPage (SubCommand.map MessageManagerMyPage)

        PageLocation.Team ->
            ManagerPage.Team.init teamData
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

                ManagerWaitTeamData waitTeamData ->
                    case message of
                        TeamResponse (Ok teamData) ->
                            let
                                ( pageModel, subCommand ) =
                                    pageLocationToInitManagerPageModel teamData waitTeamData.manager waitTeamData.pageLocation
                            in
                            ( Model
                                { rec
                                    | mainModel =
                                        ManagerLogIn
                                            { accessToken = waitTeamData.accessToken
                                            , manager =
                                                subCommand
                                                    |> SubCommand.getUser
                                                    |> Maybe.andThen Data.userGetManager
                                                    |> Maybe.withDefault waitTeamData.manager
                                            , pageModel = pageModel
                                            , team =
                                                subCommand
                                                    |> SubCommand.getTeam
                                                    |> Maybe.withDefault teamData
                                            }
                                    , notificationList = "ログインに成功!" :: SubCommand.getNotificationList subCommand
                                }
                            , subCommandToCommandWithSetTextCommand subCommand
                            )

                        TeamResponse (Err _) ->
                            ( Model
                                { rec | notificationList = rec.notificationList ++ [ "チームの情報取得に失敗しました" ] }
                            , Cmd.none
                            )

                        _ ->
                            ( Model rec
                            , Cmd.none
                            )

                PlayerLogInWaitTeamData waitTeamData ->
                    case message of
                        TeamResponse (Ok teamData) ->
                            let
                                ( pageModel, subCommand ) =
                                    pageLocationToInitPlayerPageModel teamData waitTeamData.player waitTeamData.pageLocation
                            in
                            ( Model
                                { rec
                                    | mainModel =
                                        PlayerLogIn
                                            { accessToken = waitTeamData.accessToken
                                            , player =
                                                subCommand
                                                    |> SubCommand.getUser
                                                    |> Maybe.andThen Data.userGetPlayer
                                                    |> Maybe.withDefault waitTeamData.player
                                            , pageModel = pageModel
                                            , team =
                                                subCommand
                                                    |> SubCommand.getTeam
                                                    |> Maybe.withDefault teamData
                                            }
                                    , notificationList = "ログインに成功!" :: SubCommand.getNotificationList subCommand
                                }
                            , subCommandToCommandWithSetTextCommand subCommand
                            )

                        TeamResponse (Err _) ->
                            ( Model
                                { rec | notificationList = rec.notificationList ++ [ "チームの情報取得に失敗しました" ] }
                            , Cmd.none
                            )

                        _ ->
                            ( Model rec
                            , Cmd.none
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
            ( ManagerWaitTeamData
                { accessToken = waitUserData.accessToken
                , manager = manager
                , pageLocation = waitUserData.pageLocation
                }
            , [ "監督としてログイン成功!" ]
            , Graphql.Http.queryRequest Data.apiUrl (Data.getTeam (Data.managerGetTeamId manager))
                |> Graphql.Http.send TeamResponse
            )

        Data.RolePlayer player ->
            ( PlayerLogInWaitTeamData
                { accessToken = waitUserData.accessToken
                , player = player
                , pageLocation = waitUserData.pageLocation
                }
            , [ "選手としてログイン成功!" ]
            , Graphql.Http.queryRequest Data.apiUrl (Data.getTeam (Data.playerGetTeamId player))
                |> Graphql.Http.send TeamResponse
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
                    responseUserData
                        { accessToken = record.accessToken
                        , pageLocation = PageLocation.Top
                        }
                        newUserData

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
                    responseUserData
                        { accessToken = record.accessToken
                        , pageLocation = PageLocation.Top
                        }
                        newUserData

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
            pageLocationToInitManagerPageModel logInData.team
                logInData.manager
                (PageLocation.fromUrl url)
                |> managerUpdateLogInDataAndCommandFromSubCommand logInData

        ( MessageManagerMyPage pageMessage, PageManagerMyPage pageModel ) ->
            ManagerPage.MyPage.update
                logInData.accessToken
                logInData.manager
                pageMessage
                pageModel
                |> Tuple.mapBoth PageManagerMyPage (SubCommand.map MessageManagerMyPage)
                |> managerUpdateLogInDataAndCommandFromSubCommand logInData

        ( MessageManagerTeam pageMessage, PageManagerTeam pageModel ) ->
            ManagerPage.Team.update logInData.accessToken logInData.team pageMessage pageModel
                |> Tuple.mapBoth
                    PageManagerTeam
                    (SubCommand.map MessageManagerTeam)
                |> managerUpdateLogInDataAndCommandFromSubCommand logInData

        ( _, _ ) ->
            ( logInData
            , []
            , Cmd.none
            )


managerUpdateLogInDataAndCommandFromSubCommand : ManagerLogInData -> ( ManagerPageModel, SubCommand.SubCommand Message ) -> ( ManagerLogInData, List String, Cmd Message )
managerUpdateLogInDataAndCommandFromSubCommand logInData ( newPageModel, subCommand ) =
    ( { logInData
        | pageModel = newPageModel
        , manager =
            subCommand
                |> SubCommand.getUser
                |> Maybe.andThen Data.userGetManager
                |> Maybe.withDefault logInData.manager
        , team =
            subCommand
                |> SubCommand.getTeam
                |> Maybe.withDefault logInData.team
      }
    , SubCommand.getNotificationList subCommand
    , subCommandToCommandWithSetTextCommand subCommand
    )


updatePlayer : Message -> PlayerLogInData -> ( PlayerLogInData, List String, Cmd Message )
updatePlayer message logInData =
    case ( message, logInData.pageModel ) of
        ( UrlChange url, _ ) ->
            pageLocationToInitPlayerPageModel logInData.team
                logInData.player
                (PageLocation.fromUrl url)
                |> playerUpdateLogInDataAndCommandFromSubCommand logInData

        ( MessagePlayerMyPage pageMessage, PagePlayerMyPage pageModel ) ->
            PlayerPage.MyPage.update
                logInData.accessToken
                logInData.player
                pageMessage
                pageModel
                |> Tuple.mapBoth PagePlayerMyPage (SubCommand.map MessagePlayerMyPage)
                |> playerUpdateLogInDataAndCommandFromSubCommand logInData

        ( MessagePlayerNote pageMessage, PagePlayerNote pageModel ) ->
            PlayerPage.Note.update logInData.accessToken logInData.player pageMessage pageModel
                |> Tuple.mapBoth PagePlayerNote (SubCommand.map MessagePlayerNote)
                |> playerUpdateLogInDataAndCommandFromSubCommand logInData

        ( MessagePlayerTeam pageMessage, PagePlayerTeam pageModel ) ->
            PlayerPage.Team.update logInData.accessToken logInData.team pageMessage pageModel
                |> Tuple.mapBoth PagePlayerTeam (SubCommand.map MessagePlayerTeam)
                |> playerUpdateLogInDataAndCommandFromSubCommand logInData

        ( _, _ ) ->
            ( logInData, [], Cmd.none )


playerUpdateLogInDataAndCommandFromSubCommand : PlayerLogInData -> ( PlayerPageModel, SubCommand.SubCommand Message ) -> ( PlayerLogInData, List String, Cmd Message )
playerUpdateLogInDataAndCommandFromSubCommand logInData ( newPageModel, subCommand ) =
    ( { logInData
        | pageModel = newPageModel
        , player =
            subCommand
                |> SubCommand.getUser
                |> Maybe.andThen Data.userGetPlayer
                |> Maybe.withDefault logInData.player
        , team =
            subCommand
                |> SubCommand.getTeam
                |> Maybe.withDefault logInData.team
      }
    , SubCommand.getNotificationList subCommand
    , subCommandToCommandWithSetTextCommand subCommand
    )


subCommandToCommandWithSetTextCommand : SubCommand.SubCommand Message -> Cmd Message
subCommandToCommandWithSetTextCommand subCommand =
    Cmd.batch
        (SubCommand.getCommand subCommand
            :: (SubCommand.getChangeInputTextList subCommand
                    |> List.map setText
               )
        )


view : Model -> Browser.Document Message
view (Model record) =
    { title = "TEAMe"
    , body =
        [ case record.mainModel of
            NoLogIn noLogInRecord ->
                logInView noLogInRecord.logInViewModel

            WaitUser _ ->
                Html.Styled.text "ユーザーの情報を取得中…"

            ManagerWaitTeamData _ ->
                Html.Styled.div
                    []
                    [ Html.Styled.text "チーム情報を取得中……"
                    , Style.loading
                    ]

            PlayerLogInWaitTeamData _ ->
                Html.Styled.div
                    []
                    [ Html.Styled.text "チーム情報を取得中……"
                    , Style.loading
                    ]

            NotSelectedRole notSelectedRoleData ->
                notSelectedRoleDataView notSelectedRoleData

            ManagerLogIn managerLogInData ->
                managerLogInView (record.timeZone |> Maybe.withDefault Time.utc) managerLogInData

            PlayerLogIn playerLogInData ->
                playerLogInView (record.timeZone |> Maybe.withDefault Time.utc) playerLogInData
        , notificationView record.notificationList
        ]
            |> List.map Html.Styled.toUnstyled
    }


logInView : LogInViewModel -> Html.Styled.Html Message
logInView model =
    Style.pageContainer
        (case model of
            DisplayedLogInButton ->
                [ Style.header Nothing
                , Html.Styled.div
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
            [ Html.Styled.div [] [ Html.Styled.text "チームを作成中……", Style.loading ] ]

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
                        [ Html.Styled.text "チーム一覧を取得中……", Style.loading ]
                )
            ]
        )


managerLogInView : Time.Zone -> ManagerLogInData -> Html.Styled.Html Message
managerLogInView timeZone logInData =
    case logInData.pageModel of
        PageManagerMyPage model ->
            ManagerPage.MyPage.view logInData.manager model
                |> Html.Styled.map MessageManagerMyPage

        PageManagerTeam model ->
            ManagerPage.Team.view timeZone logInData.manager logInData.team model
                |> Html.Styled.map MessageManagerTeam


playerLogInView : Time.Zone -> PlayerLogInData -> Html.Styled.Html Message
playerLogInView timeZone logInData =
    case logInData.pageModel of
        PagePlayerMyPage model ->
            PlayerPage.MyPage.view logInData.player model
                |> Html.Styled.map MessagePlayerMyPage

        PagePlayerNote model ->
            PlayerPage.Note.view timeZone logInData.player model
                |> Html.Styled.map MessagePlayerNote

        PagePlayerTeam model ->
            PlayerPage.Team.view timeZone logInData.player logInData.team model
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
            , Css.property "pointer-events" "none"
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
