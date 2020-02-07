module ManagerPage.PdcaEditor exposing (Message, Model, init, update, view)

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


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    ( model, Cmd.none )


view : Model -> S.Html Message
view _ =
    S.div
        []
        [ S.text "PDCAサイクルの質問作成" ]
