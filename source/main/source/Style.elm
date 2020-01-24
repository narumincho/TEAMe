module Style exposing (normalButton)

import Css
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
            ]
        , E.onClick message
        ]
        [ S.text text ]
