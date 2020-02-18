module Style exposing (alignContentEnd, alignContentStart, animationFillModeForwards, conditionButton, displayGrid, gap, goalTitle, gridAutoFlowColumn, gridCell, header, inputText, loading, managerBottomNavigation, multiLineTextBox, normalButton, pageContainer, pageMainViewContainer, playerBottomNavigation, teamPlayerListView, themeColor, timeToString, userImage)

import Css
import Css.Animations
import Css.Transitions
import Data
import Html.Styled as S
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import PageLocation
import Time


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
            , Css.overflow Css.auto
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
            , Css.padding (Css.rem 0.4)
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
            , Css.backgroundColor (Css.rgb 246 252 240)
            , gap 0.5
            , Css.overflow Css.auto
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
                , userSelectNone
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
                , userSelectNone
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
            , Css.border3 (Css.px 3) Css.solid themeColor
            , Css.fontSize (Css.rem 1.5)
            , Css.padding (Css.rem 0.5)
            , Css.outline Css.none
            , Css.boxSizing Css.borderBox
            , Css.width (Css.pct 100)
            , Css.focus
                [ Css.property "box-shadow" "inset 0 1px 1px rgba(0, 0, 0, 0.075), 0 0 8px rgba(167, 216, 110, 0.6)"
                , Css.border3 (Css.px 3) Css.solid themeColor
                ]
            , Css.Transitions.transition
                [ Css.Transitions.borderColor3 150 0 Css.Transitions.easeInOut
                , Css.Transitions.boxShadow3 150 0 Css.Transitions.easeInOut
                ]
            ]
        ]
        []


multiLineTextBox : String -> String -> (String -> message) -> S.Html message
multiLineTextBox id name messageFunction =
    S.textarea
        [ A.id id
        , A.name name
        , E.onInput messageFunction
        , A.css
            [ Css.width (Css.pct 100)
            , Css.boxSizing Css.borderBox
            , Css.height (Css.rem 8)
            , Css.fontSize (Css.rem 1)
            ]
        ]
        []


goalTitle : String -> S.Html message
goalTitle text =
    S.div
        [ A.css
            [ Css.color themeColor
            , displayGrid
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.fontSize (Css.rem 1.2)
            , Css.padding (Css.rem 0.5)
            ]
        ]
        [ S.text text ]


loading : S.Html msg
loading =
    S.div
        [ A.css
            [ Css.borderRadius (Css.pct 50)
            , Css.width (Css.px 32)
            , Css.height (Css.px 32)
            , Css.border3 (Css.px 3) Css.solid (Css.rgb 0 0 0)
            , Css.borderRightColor Css.transparent
            , Css.animationName
                (Css.Animations.keyframes
                    [ ( 100, [ Css.Animations.transform [ Css.rotate (Css.turn 1) ] ] ) ]
                )
            , Css.animationIterationCount infinite
            , Css.animationDuration (Css.sec 0.6)
            , Css.property "animation-timing-function" "linear"
            ]
        ]
        []


infinite =
    let
        a =
            Css.int 0
    in
    { a | value = "infinite" }


displayGrid : Css.Style
displayGrid =
    Css.property "display" "grid"


gridAutoFlowColumn : Css.Style
gridAutoFlowColumn =
    Css.property "grid-auto-flow" "column"


gap : Float -> Css.Style
gap number =
    Css.property "gap" (String.fromFloat number ++ "rem")


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


teamPlayerListView : Time.Zone -> Maybe (List Data.Player) -> S.Html message
teamPlayerListView timeZone playerListMaybe =
    S.div
        []
        (case playerListMaybe of
            Just playerList ->
                playerList
                    |> List.map (teamPlayerView timeZone)

            Nothing ->
                [ S.div [] [ S.text "選手の情報を読込中" ], loading ]
        )


teamPlayerView : Time.Zone -> Data.Player -> S.Html message
teamPlayerView timeZone player =
    S.div []
        ([ S.div
            [ A.css
                [ displayGrid
                , gridAutoFlowColumn
                , Css.alignItems Css.center
                , Css.justifyContent Css.start
                ]
            ]
            [ userImage
                { name = Data.playerGetName player
                , imageFileHash = Data.playerGetImageFileHash player
                }
            , S.div [] [ S.text (Data.playerGetName player) ]
            ]
         ]
            ++ (player
                    |> Data.playerGetCycleDataList
                    |> List.map
                        (\cycle ->
                            S.div
                                [ A.css
                                    [ Css.padding (Css.rem 1) ]
                                ]
                                [ pdcaItem "P" cycle.plan
                                , pdcaItem "D" cycle.do
                                , pdcaItem "C" cycle.check
                                , pdcaItem "A" cycle.act
                                , S.div [] [ S.text (timeToString timeZone cycle.createdAt) ]
                                ]
                        )
               )
        )


timeToString : Time.Zone -> Time.Posix -> String
timeToString zone posix =
    String.fromInt (Time.toYear zone posix)
        ++ "/"
        ++ String.fromInt (monthToInt (Time.toMonth zone posix))
        ++ "/"
        ++ String.fromInt (Time.toDay zone posix)
        ++ " "
        ++ String.padLeft 2 '0' (String.fromInt (Time.toHour zone posix))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute zone posix))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute zone posix))


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


pdcaItem : String -> String -> S.Html message
pdcaItem title body =
    S.div
        [ A.css
            [ displayGrid
            , gridAutoFlowColumn
            , Css.alignItems Css.baseline
            ]
        ]
        [ S.div [ A.css [ Css.fontSize (Css.rem 2) ] ] [ S.text title ]
        , S.div [] [ S.text body ]
        ]
