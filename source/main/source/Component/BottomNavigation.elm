module Component.BottomNavigation exposing (..)

import Css
import Html.Styled
import Html.Styled.Attributes


view : Html.Styled.Html msg
view =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.height (Css.px 48) ]
        ]
        [ Html.Styled.text "下に表示するメニュー" ]
