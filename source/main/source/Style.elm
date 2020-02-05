module Style exposing (conditionButton, inputText, normalButton, userImage)

import Css
import Css.Animations
import Css.Transitions
import Data
import Html.Styled as S
import Html.Styled.Attributes as A
import Html.Styled.Events as E


normalButton : message -> String -> S.Html message
normalButton message text =
    S.button
        [ A.css
            [ Css.padding (Css.rem 1)
            , Css.border2 Css.zero Css.none
            , Css.cursor Css.pointer
            , Css.backgroundColor (Css.rgb 220 220 220)
            , Css.borderRadius (Css.rem 0.5)
            , Css.hover
                [ Css.backgroundColor (Css.rgb 180 180 180) ]
            ]
        , E.onClick message
        ]
        [ S.text text ]


conditionButton : Maybe message -> String -> S.Html message
conditionButton messageMaybe text =
    case messageMaybe of
        Just message ->
            normalButton message text

        Nothing ->
            S.button
                [ A.css
                    [ Css.padding (Css.rem 1)
                    , Css.border2 Css.zero Css.none
                    , Css.borderRadius (Css.rem 0.5)
                    , Css.cursor Css.notAllowed
                    ]
                , A.disabled True
                ]
                [ S.text text ]


userImage : String -> Data.FileHash -> S.Html message
userImage userName imageFileHash =
    S.img
        [ A.css
            [ Css.width (Css.rem 2)
            , Css.height (Css.rem 2)
            , Css.property "object-fit" "cover"
            , Css.borderRadius (Css.pct 50)
            ]
        , A.alt (userName ++ "のプロフィール画像")
        , A.src (Data.fileHashToUrlAsString imageFileHash)
        ]
        []


inputText : String -> (String -> message) -> S.Html message
inputText name messageFunction =
    S.input
        [ A.type_ "text"
        , A.name name
        , E.onInput messageFunction
        , A.css
            [ Css.borderRadius (Css.rem 0.5)
            , Css.border3 (Css.px 1) Css.solid (Css.rgb 204 204 204)
            , Css.fontSize (Css.rem 1.5)
            , Css.padding (Css.rem 0.5)
            , Css.boxShadow5
                Css.inset
                Css.zero
                (Css.px 1)
                (Css.px 1)
                (Css.rgba 0 0 0 0.75)
            , Css.outline Css.none
            , Css.focus
                [ Css.property "box-shadow" "inset 0 1px 1px rgba(0, 0, 0, 0.075), 0 0 8px rgba(102, 175, 233, 0.6)"
                , Css.border3 (Css.px 1) Css.solid (Css.rgb 102 175 233)
                ]
            , Css.Transitions.transition
                [ Css.Transitions.borderColor3 150 0 Css.Transitions.easeInOut
                , Css.Transitions.boxShadow3 150 0 Css.Transitions.easeInOut
                ]
            ]
        ]
        []
