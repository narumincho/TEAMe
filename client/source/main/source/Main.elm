module Main exposing (main)

import Browser
import Browser.Navigation
import Html
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
init () _ _ =
    ( Model, Cmd.none )


view : Model -> Browser.Document Msg
view _ =
    { title = "TEAMe"
    , body = [ Html.text "TEAMe" ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )
