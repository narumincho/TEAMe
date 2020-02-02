module Style exposing (normalButton, userImage)

import Css
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
            ]
        , E.onClick message
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
