module Main exposing (main)

import Api
import Browser
import Browser.Navigation
import Html.Styled
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
        { accessToken : Maybe Api.AccessToken
        , pageModel : PageModel
        , navigationKey : Browser.Navigation.Key
        }


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
        { accessToken = accessTokenMaybe
        , pageModel = pageModel
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


view : Model -> Browser.Document Message
view (Model { pageModel }) =
    { title = "TEAMe"
    , body =
        [ case pageModel of
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


update : Message -> Model -> ( Model, Cmd Message )
update message (Model rec) =
    case ( message, rec.pageModel ) of
        ( UrlChange url, _ ) ->
            let
                ( pageModel, command ) =
                    pageLocationToInitPageModel (PageLocation.fromUrl url)
            in
            ( Model
                { rec
                    | pageModel = pageModel
                }
            , command
            )

        ( UrlRequest urlRequest, _ ) ->
            ( Model rec
            , case urlRequest of
                Browser.Internal url ->
                    Browser.Navigation.pushUrl rec.navigationKey (Url.toString url)

                Browser.External urlString ->
                    Browser.Navigation.load urlString
            )

        ( MessageMyPage pageMessage, PageMyPage pageModel ) ->
            let
                ( newMyPageModel, command ) =
                    Page.MyPage.update pageMessage pageModel
            in
            ( Model
                { rec | pageModel = PageMyPage newMyPageModel }
            , command |> Maybe.map myPageCommandToCommand |> Maybe.withDefault Cmd.none
            )

        ( MessageNote pageMessage, PageNote pageModel ) ->
            let
                ( newPageModel, command ) =
                    Page.Note.update pageMessage pageModel
            in
            ( Model
                { rec | pageModel = PageNote newPageModel }
            , command |> Maybe.map noteCommandToCommand |> Maybe.withDefault Cmd.none
            )

        ( MessageTeam pageMessage, PageTeam pageModel ) ->
            let
                ( newPageModel, command ) =
                    Page.Team.update pageMessage pageModel
            in
            ( Model
                { rec | pageModel = PageTeam newPageModel }
            , command |> Maybe.map teamCommandToCommand |> Maybe.withDefault Cmd.none
            )

        ( _, _ ) ->
            ( Model rec
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
