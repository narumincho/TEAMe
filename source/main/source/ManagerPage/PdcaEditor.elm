module ManagerPage.PdcaEditor exposing (Message(..), Model, init, view)

import Html.Styled as S


type Model
    = Model


type Message
    = Message


init : ( Model, Cmd Message )
init =
    ( Model
    , Cmd.none
    )


view : Model -> S.Html Message
view _ =
    S.div
        []
        [ S.text "PDCAサイクルの質問作成" ]
