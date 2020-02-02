module Main exposing (main)

import Api.Enum.Role
import Api.Mutation
import Browser
import Browser.Navigation
import Css
import Data
import Graphql.Http
import Graphql.SelectionSet
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import Json.Encode
import Page.MyPage
import Page.Note
import Page.Team
import PageLocation
import Style
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


apiUrl : String
apiUrl =
    "https://us-central1-teame-c1a32.cloudfunctions.net/api"


type Model
    = Model
        { logInState : LogInState
        , navigationKey : Browser.Navigation.Key
        , messageList : List String
        }


type LogInState
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
    | NotSelectedRoleDataSelectManager
        { accessToken : Data.AccessToken
        , userData : Data.NoRoleUser
        }
    | NotSelectedRoleDataSelectPlayer
        { accessToken : Data.AccessToken
        , userData : Data.NoRoleUser
        , teamList : Maybe (List Data.TeamData)
        }


type alias ManagerLogInData =
    { accessToken : Data.AccessToken
    , userData : Data.Manager
    , pageModel : PageModel
    }


type alias PlayerLogInData =
    { accessToken : Data.AccessToken
    , userData : Data.Player
    , pageModel : PageModel
    }


type LogInViewModel
    = DisplayedLogInButton
    | WaitLogInUrl
    | ErrorLogIn String


type PageModel
    = PageMyPage Page.MyPage.Model
    | PageNote Page.Note.Model
    | PageTeam Page.Team.Model


type Message
    = UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | MessageMyPage Page.MyPage.Message
    | MessageNote Page.Note.Message
    | MessageTeam Page.Team.Message
    | RequestLineLogInUrl
    | ResponseLineLogInUrl (Result (Graphql.Http.Error String) String)
    | ResponseUserData (Result (Graphql.Http.Error Data.UserData) Data.UserData)
    | SelectRole Api.Enum.Role.Role
    | AllTeamResponse (Result (Graphql.Http.Error (List Data.TeamData)) (List Data.TeamData))
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
                            { logInState = logInState
                            , navigationKey = key
                            , messageList = []
                            }
                    )

        ( Just accessToken, Nothing ) ->
            waitUserModelAndCommand pageLocation (Data.accessTokenFromString accessToken)
                |> Tuple.mapFirst
                    (\logInState ->
                        Model
                            { logInState = logInState
                            , navigationKey = key
                            , messageList = []
                            }
                    )

        ( Nothing, Nothing ) ->
            ( Model
                { logInState =
                    NoLogIn
                        { logInViewModel = DisplayedLogInButton
                        , pageLocation = pageLocation
                        }
                , navigationKey = key
                , messageList = []
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


waitUserModelAndCommand : PageLocation.PageLocation -> Data.AccessToken -> ( LogInState, Cmd Message )
waitUserModelAndCommand pageLocation accessToken =
    ( WaitUser
        { accessToken = accessToken
        , pageLocation = pageLocation
        }
    , Graphql.Http.queryRequest apiUrl (Data.getUserData accessToken)
        |> Graphql.Http.send ResponseUserData
    )


pageLocationToInitPageModel : PageLocation.PageLocation -> ( PageModel, Cmd Message )
pageLocationToInitPageModel pageLocation =
    case pageLocation of
        PageLocation.MyPage ->
            Page.MyPage.init
                |> Tuple.mapBoth PageMyPage (always Cmd.none)

        PageLocation.Note ->
            Page.Note.init
                |> Tuple.mapBoth PageNote (always Cmd.none)

        PageLocation.Team ->
            Page.Team.init
                |> Tuple.mapBoth PageTeam (always Cmd.none)


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
            case rec.logInState of
                NoLogIn noLogInRecord ->
                    updateNoLogIn message noLogInRecord
                        |> Tuple.mapFirst
                            (\newNoLogInRecord ->
                                Model
                                    { rec | logInState = newNoLogInRecord }
                            )

                WaitUser waitUserDataRecord ->
                    let
                        ( newLogInState, newMessageList, cmd ) =
                            updateWaitUserData message waitUserDataRecord
                    in
                    ( Model
                        { rec
                            | logInState = newLogInState
                            , messageList = rec.messageList ++ newMessageList
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
                            | logInState = newLogInState
                            , messageList = rec.messageList ++ newMessageList
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
                            | logInState = ManagerLogIn newManagerLogInData
                            , messageList = rec.messageList ++ newMessageList
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
                            | logInState = PlayerLogIn newPlayerData
                            , messageList = rec.messageList ++ newMessageList
                        }
                    , cmd
                    )


updateNoLogIn :
    Message
    -> NoLogInData
    -> ( LogInState, Cmd Message )
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
            , Graphql.Http.mutationRequest apiUrl
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


updateWaitUserData : Message -> WaitUserData -> ( LogInState, List String, Cmd Message )
updateWaitUserData message waitUserData =
    case message of
        ResponseUserData userDataResult ->
            case userDataResult of
                Ok userData ->
                    case userData of
                        Data.NoRole noRoleUser ->
                            ( NotSelectedRole
                                (NotSelectedRoleData
                                    { accessToken = waitUserData.accessToken
                                    , userData = noRoleUser
                                    }
                                )
                            , []
                            , Cmd.none
                            )

                        Data.RoleManager manager ->
                            let
                                ( pageModel, command ) =
                                    pageLocationToInitPageModel waitUserData.pageLocation
                            in
                            ( ManagerLogIn
                                { accessToken = waitUserData.accessToken
                                , userData = manager
                                , pageModel = pageModel
                                }
                            , [ "監督としてログイン成功!" ]
                            , command
                            )

                        Data.RolePlayer player ->
                            let
                                ( pageModel, command ) =
                                    pageLocationToInitPageModel waitUserData.pageLocation
                            in
                            ( PlayerLogIn
                                { accessToken = waitUserData.accessToken
                                , userData = player
                                , pageModel = pageModel
                                }
                            , [ "選手としてログイン成功!" ]
                            , command
                            )

                Err _ ->
                    ( WaitUser waitUserData, [ "ユーザーの情報取得に失敗" ], Cmd.none )

        _ ->
            ( WaitUser waitUserData, [], Cmd.none )


updateNoSelectedRole : Message -> NotSelectedRoleData -> ( LogInState, List String, Cmd Message )
updateNoSelectedRole message notSelectedRoleData =
    case ( message, notSelectedRoleData ) of
        ( SelectRole role, NotSelectedRoleData record ) ->
            case role of
                Api.Enum.Role.Manager ->
                    ( NotSelectedRole (NotSelectedRoleDataSelectManager record)
                    , []
                    , Cmd.none
                    )

                Api.Enum.Role.Player ->
                    ( NotSelectedRole
                        (NotSelectedRoleDataSelectPlayer
                            { accessToken = record.accessToken
                            , userData = record.userData
                            , teamList = Nothing
                            }
                        )
                    , []
                    , Graphql.Http.queryRequest apiUrl Data.getAllTeam
                        |> Graphql.Http.send AllTeamResponse
                    )

        ( AllTeamResponse allTeamResult, NotSelectedRoleDataSelectPlayer record ) ->
            case allTeamResult of
                Ok allTeam ->
                    ( NotSelectedRole (NotSelectedRoleDataSelectPlayer { record | teamList = Just allTeam })
                    , []
                    , Cmd.none
                    )

                Err _ ->
                    ( NotSelectedRole (NotSelectedRoleDataSelectPlayer record)
                    , [ "チームの一覧取得に失敗" ]
                    , Cmd.none
                    )

        ( _, _ ) ->
            ( NotSelectedRole notSelectedRoleData
            , [ "チームの一覧取得に失敗" ]
            , Cmd.none
            )


updateManager :
    Message
    -> ManagerLogInData
    -> ( ManagerLogInData, List String, Cmd Message )
updateManager message logInRecord =
    case ( message, logInRecord.pageModel ) of
        ( UrlChange url, _ ) ->
            let
                ( pageModel, command ) =
                    pageLocationToInitPageModel (PageLocation.fromUrl url)
            in
            ( { logInRecord | pageModel = pageModel }
            , []
            , command
            )

        ( MessageMyPage pageMessage, PageMyPage pageModel ) ->
            let
                ( newMyPageModel, command ) =
                    Page.MyPage.update pageMessage pageModel
            in
            ( { logInRecord | pageModel = PageMyPage newMyPageModel }
            , []
            , command |> Maybe.map myPageCommandToCommand |> Maybe.withDefault Cmd.none
            )

        ( MessageNote pageMessage, PageNote pageModel ) ->
            let
                ( newPageModel, command ) =
                    Page.Note.update pageMessage pageModel
            in
            ( { logInRecord | pageModel = PageNote newPageModel }
            , []
            , command |> Maybe.map noteCommandToCommand |> Maybe.withDefault Cmd.none
            )

        ( MessageTeam pageMessage, PageTeam pageModel ) ->
            let
                ( newPageModel, command ) =
                    Page.Team.update pageMessage pageModel
            in
            ( { logInRecord | pageModel = PageTeam newPageModel }
            , []
            , command |> Maybe.map teamCommandToCommand |> Maybe.withDefault Cmd.none
            )

        ( _, _ ) ->
            ( logInRecord
            , []
            , Cmd.none
            )


updatePlayer : Message -> PlayerLogInData -> ( PlayerLogInData, List String, Cmd Message )
updatePlayer message playerLogInData =
    case message of
        _ ->
            ( playerLogInData, [], Cmd.none )


myPageCommandToCommand : Page.MyPage.Command -> Cmd Message
myPageCommandToCommand _ =
    Cmd.none


noteCommandToCommand : Page.Note.Command -> Cmd Message
noteCommandToCommand _ =
    Cmd.none


teamCommandToCommand : Page.Team.Command -> Cmd Message
teamCommandToCommand _ =
    Cmd.none


view : Model -> Browser.Document Message
view (Model record) =
    { title = "TEAMe"
    , body =
        ([ case record.logInState of
            NoLogIn noLogInRecord ->
                logInView noLogInRecord.logInViewModel

            WaitUser waitUserData ->
                Html.Styled.text "ユーザーの情報を取得中…"

            NotSelectedRole notSelectedRoleData ->
                notSelectedRoleDataView notSelectedRoleData

            ManagerLogIn managerLogInData ->
                Html.Styled.text "監督の画面"

            PlayerLogIn playerLogInData ->
                Html.Styled.text "選手の画面"
         ]
            ++ (record.messageList
                    |> List.map
                        (\message -> Html.Styled.div [] [ Html.Styled.text message ])
               )
        )
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
                    record.userData.name
                    record.userData.imageFileHash
                , Html.Styled.text ("はじまして、" ++ record.userData.name ++ "さん。監督か 選手かを選んでください")
                , Html.Styled.div
                    []
                    [ Html.Styled.text (Data.accessTokenToString record.accessToken) ]
                , Style.normalButton (SelectRole Api.Enum.Role.Manager) "監督"
                , Style.normalButton (SelectRole Api.Enum.Role.Player) "選手"
                ]

        NotSelectedRoleDataSelectManager record ->
            Html.Styled.div
                []
                [ Html.Styled.text "監督はます、チームをつくります。チーム名は?"
                , Html.Styled.input
                    [ Html.Styled.Attributes.type_ "text"
                    , Html.Styled.Attributes.property "autocomplete" (Json.Encode.string "team-name")
                    ]
                    []
                ]

        NotSelectedRoleDataSelectPlayer record ->
            Html.Styled.div
                []
                [ Html.Styled.text "選手は所属するチームを選んでもらいます"
                , Html.Styled.div
                    []
                    [ Html.Styled.text
                        (case record.teamList of
                            Just _ ->
                                "チームを取得した"

                            Nothing ->
                                "チームを取得中……"
                        )
                    ]
                ]
