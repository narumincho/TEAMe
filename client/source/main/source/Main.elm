module Main exposing (main)

import Api
import Browser
import Browser.Navigation
import Css
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import Page.MyPage
import Page.Note
import Page.Team
import PageLocation
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


type Model
    = Model
        { logInState : LogInState
        , navigationKey : Browser.Navigation.Key
        }


type LogInState
    = NoLogIn
        { logInViewModel : LogInViewModel
        , pageLocation : PageLocation.PageLocation
        }
    | LogIn
        { accessToken : Api.AccessToken
        , pageModel : PageModel
        }


type LogInViewModel
    = DisplayedLogInButton
    | WaitLogInUrl


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
    | ResponseLineLogInUrl (Result String Url.Url)


main : Program () Model Message
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Message )
init () url key =
    let
        ( accessTokenMaybe, pageLocation ) =
            PageLocation.initFromUrl url

        ( pageModel, pageCommand ) =
            pageLocationToInitPageModel pageLocation
    in
    ( Model
        { logInState =
            case accessTokenMaybe of
                Just accessToken ->
                    LogIn
                        { accessToken = accessToken
                        , pageModel = pageModel
                        }

                Nothing ->
                    NoLogIn
                        { logInViewModel = DisplayedLogInButton
                        , pageLocation = pageLocation
                        }
        , navigationKey = key
        }
    , Cmd.batch
        [ Browser.Navigation.replaceUrl key
            (PageLocation.toUrlAsString pageLocation)
        , pageCommand
        ]
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
                                    { rec | logInState = NoLogIn newNoLogInRecord }
                            )

                LogIn logInRecord ->
                    updateLogIn message logInRecord
                        |> Tuple.mapFirst
                            (\newLogInRecord ->
                                Model
                                    { rec | logInState = LogIn newLogInRecord }
                            )


updateNoLogIn : Message -> { logInViewModel : LogInViewModel, pageLocation : PageLocation.PageLocation } -> ( { logInViewModel : LogInViewModel, pageLocation : PageLocation.PageLocation }, Cmd Message )
updateNoLogIn message noLogInRecord =
    case message of
        UrlChange url ->
            ( { noLogInRecord | pageLocation = PageLocation.fromUrl url }
            , Cmd.none
            )

        RequestLineLogInUrl ->
            ( { noLogInRecord
                | logInViewModel = WaitLogInUrl
              }
            , Api.getLineLogInUrl ResponseLineLogInUrl
            )

        _ ->
            ( noLogInRecord
            , Cmd.none
            )


updateLogIn : Message -> { accessToken : Api.AccessToken, pageModel : PageModel } -> ( { accessToken : Api.AccessToken, pageModel : PageModel }, Cmd Message )
updateLogIn message logInRecord =
    case ( message, logInRecord.pageModel ) of
        ( UrlChange url, _ ) ->
            let
                ( pageModel, command ) =
                    pageLocationToInitPageModel (PageLocation.fromUrl url)
            in
            ( { logInRecord | pageModel = pageModel }
            , command
            )

        ( MessageMyPage pageMessage, PageMyPage pageModel ) ->
            let
                ( newMyPageModel, command ) =
                    Page.MyPage.update pageMessage pageModel
            in
            ( { logInRecord | pageModel = PageMyPage newMyPageModel }
            , command |> Maybe.map myPageCommandToCommand |> Maybe.withDefault Cmd.none
            )

        ( MessageNote pageMessage, PageNote pageModel ) ->
            let
                ( newPageModel, command ) =
                    Page.Note.update pageMessage pageModel
            in
            ( { logInRecord | pageModel = PageNote newPageModel }
            , command |> Maybe.map noteCommandToCommand |> Maybe.withDefault Cmd.none
            )

        ( MessageTeam pageMessage, PageTeam pageModel ) ->
            let
                ( newPageModel, command ) =
                    Page.Team.update pageMessage pageModel
            in
            ( { logInRecord | pageModel = PageTeam newPageModel }
            , command |> Maybe.map teamCommandToCommand |> Maybe.withDefault Cmd.none
            )

        ( _, _ ) ->
            ( logInRecord
            , Cmd.none
            )


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
        [ case record.logInState of
            NoLogIn noLogInRecord ->
                logInView noLogInRecord.logInViewModel

            LogIn logInRecord ->
                case logInRecord.pageModel of
                    PageMyPage model ->
                        Page.MyPage.view model
                            |> Html.Styled.map MessageMyPage

                    PageNote model ->
                        Page.Note.view model
                            |> Html.Styled.map MessageNote

                    PageTeam model ->
                        Page.Team.view model
                            |> Html.Styled.map MessageTeam
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
                        ]

                    WaitLogInUrl ->
                        [ Html.Styled.div
                            []
                            [ Html.Styled.text "ログインへのURLを取得中" ]
                        ]
               )
        )


lineLogInButton : Html.Styled.Html Message
lineLogInButton =
    Html.Styled.button
        [ Html.Styled.Attributes.css
            [ Css.backgroundColor (Css.rgb 0 195 0) ]
        , Html.Styled.Events.onClick RequestLineLogInUrl
        ]
        [ Html.Styled.img
            [ Html.Styled.Attributes.src "/assets/line-icon.png"
            , Html.Styled.Attributes.href "LINEのロゴ"
            , Html.Styled.Attributes.css
                [ Css.width (Css.px 48)
                , Css.height (Css.px 48)
                , Css.boxSizing Css.borderBox
                ]
            ]
            []
        , Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.borderLeft3 (Css.px 1) Css.solid (Css.rgb 0 179 0)
                , Css.color (Css.rgb 255 255 255)
                ]
            ]
            [ Html.Styled.text "LINEでログイン" ]
        ]
