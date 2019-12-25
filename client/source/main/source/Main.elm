module Main exposing (main)

import Api
import Browser
import Browser.Navigation
import Html
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
        , page : PageLocation.InitPageLocation
        , navigationKey : Browser.Navigation.Key
        }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    let
        ( accessTokenMaybe, initPageLocationMaybe ) =
            PageLocation.initFromUrl url
    in
    case initPageLocationMaybe of
        Just initPageLocation ->
            ( Model
                { accessToken = accessTokenMaybe
                , page = initPageLocation
                , navigationKey = key
                }
            , Browser.Navigation.replaceUrl key
                (PageLocation.initToUrlAsString initPageLocation)
            )

        Nothing ->
            ( Model
                { accessToken = accessTokenMaybe
                , page = PageLocation.InitMyPage
                , navigationKey = key
                }
            , Browser.Navigation.replaceUrl key (PageLocation.initToUrlAsString PageLocation.InitMyPage)
            )


view : Model -> Browser.Document Msg
view _ =
    { title = "TEAMe"
    , body = [ Html.text "TEAMe" ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )
