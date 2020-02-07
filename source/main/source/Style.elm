module Style exposing (alignContentEnd, alignContentStart, animationFillModeForwards, conditionButton, displayGrid, gridAutoFlowColumn, gridCell, header, inputText, managerBottomNavigation, normalButton, pageContainer, pageMainViewContainer, playerBottomNavigation, themeColor, userImage)

import Css
import Css.Transitions
import Data
import Html.Styled as S
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import PageLocation


themeColor : Css.Color
themeColor =
    Css.rgb 167 216 110


pageContainer : List (S.Html message) -> S.Html message
pageContainer =
    S.div
        [ A.css
            [ displayGrid
            , Css.height (Css.pct 100)
            , gridCellHeightList [ "max-content", "1fr", "max-content" ]
            , gridCell { x = 0, y = 0, width = 1, height = 1 }
            ]
        ]


header : Maybe Data.UserData -> S.Html message
header userMaybe =
    S.div
        [ A.css
            [ displayGrid
            , gridAutoFlowColumn
            , Css.backgroundColor themeColor
            , gridCell { x = 0, y = 0, width = 1, height = 1 }
            , gridCellWidthList [ "1fr", "max-content" ]
            , Css.padding (Css.rem 0.2)
            ]
        ]
        ([ S.div
            [ A.css
                [ Css.color (Css.rgb 255 255 255)
                , Css.fontWeight Css.bold
                , Css.fontSize (Css.rem 1.5)
                ]
            ]
            [ S.text "TEAMe" ]
         ]
            ++ (case userMaybe of
                    Just user ->
                        [ Data.getUserNameAndImageFileHash user |> userImage ]

                    Nothing ->
                        []
               )
        )


pageMainViewContainer : List (S.Html message) -> S.Html message
pageMainViewContainer =
    S.div
        [ A.css
            [ gridCell { x = 0, y = 1, width = 1, height = 1 }
            , displayGrid
            , alignContentStart
            ]
        ]


playerBottomNavigation : PageLocation.PageLocation -> S.Html message
playerBottomNavigation selected =
    S.div
        [ A.css [ bottomNavigationStyle, gridCellWidthList [ "1fr", "1fr", "1fr" ] ]
        ]
        [ navigationItem selected PageLocation.Top "マイページ"
        , navigationItem selected PageLocation.PlayerNote "ノート"
        , navigationItem selected PageLocation.Team "チーム"
        ]


managerBottomNavigation : PageLocation.PageLocation -> S.Html message
managerBottomNavigation selected =
    S.div
        [ A.css [ bottomNavigationStyle, gridCellWidthList [ "1fr", "1fr" ] ]
        ]
        [ navigationItem selected PageLocation.Top "マイページ"
        , navigationItem selected PageLocation.Team "チーム"
        ]


bottomNavigationStyle : Css.Style
bottomNavigationStyle =
    Css.batch
        [ gridCell { x = 0, y = 2, width = 1, height = 1 }
        , displayGrid
        , gridAutoFlowColumn
        , Css.height (Css.rem 3)
        ]


navigationItem : PageLocation.PageLocation -> PageLocation.PageLocation -> String -> S.Html message
navigationItem selected pageLocation text =
    if selected == pageLocation then
        S.div
            [ A.css
                [ displayGrid
                , Css.textDecoration Css.none
                , Css.color (Css.rgb 255 255 255)
                , Css.backgroundColor themeColor
                , justifyItemsCenter
                , Css.alignItems Css.center
                ]
            ]
            [ S.text text ]

    else
        S.a
            [ A.href (PageLocation.toUrlAsString pageLocation)
            , A.css
                [ displayGrid
                , Css.textDecoration Css.none
                , Css.color (Css.rgb 0 0 0)
                , Css.backgroundColor themeColor
                , justifyItemsCenter
                , Css.alignItems Css.center
                ]
            ]
            [ S.text text ]


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


userImage : { name : String, imageFileHash : Data.FileHash } -> S.Html message
userImage { name, imageFileHash } =
    S.img
        [ A.css
            [ Css.width (Css.rem 2)
            , Css.height (Css.rem 2)
            , Css.property "object-fit" "cover"
            , Css.borderRadius (Css.pct 50)
            ]
        , A.alt (name ++ "のプロフィール画像")
        , A.src (Data.fileHashToUrlAsString imageFileHash)
        ]
        []


inputText : String -> String -> (String -> message) -> S.Html message
inputText id name messageFunction =
    S.input
        [ A.type_ "text"
        , A.id id
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


displayGrid : Css.Style
displayGrid =
    Css.property "display" "grid"


gridAutoFlowColumn : Css.Style
gridAutoFlowColumn =
    Css.property "grid-auto-flow" "column"


gap : Int -> Css.Style
gap number =
    Css.property "gap" (String.fromInt number ++ "px")


gridCellHeightList : List String -> Css.Style
gridCellHeightList list =
    Css.property "grid-template-rows" (list |> String.join " ")


gridCellWidthList : List String -> Css.Style
gridCellWidthList list =
    Css.property "grid-template-columns" (list |> String.join " ")


gridCell : { x : Int, y : Int, width : Int, height : Int } -> Css.Style
gridCell { x, y, width, height } =
    Css.batch
        [ Css.property "grid-column"
            (String.fromInt (1 + x) ++ " / " ++ String.fromInt (1 + x + width))
        , Css.property "grid-row"
            (String.fromInt (1 + y) ++ " / " ++ String.fromInt (1 + y + height))
        ]


alignContentEnd : Css.Style
alignContentEnd =
    Css.property "align-content" "end"


alignContentStart : Css.Style
alignContentStart =
    Css.property "align-content" "start"


justifyItemsCenter : Css.Style
justifyItemsCenter =
    Css.property "justify-items" "center"


userSelectNone : Css.Style
userSelectNone =
    Css.property "user-select" "none"


objectFixContain : Css.Style
objectFixContain =
    Css.property "object-fit" "contain"


animationFillModeForwards : Css.Style
animationFillModeForwards =
    Css.property "animation-fill-mode" "forwards"
