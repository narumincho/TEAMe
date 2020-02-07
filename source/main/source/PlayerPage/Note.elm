module PlayerPage.Note exposing (Message, Model, init, update, view)

import Css
import Data
import Html.Styled as S
import Html.Styled.Attributes
import PageLocation
import Style
import SubCommand


type Model
    = Model { player : Data.Player }


type Message
    = CreateNewPdca
    | CreateNewCycle


update : Message -> Model -> ( Model, SubCommand.SubCommand Message )
update message model =
    ( model, SubCommand.none )


init : Data.Player -> ( Model, SubCommand.SubCommand Message )
init player =
    ( Model { player = player }, SubCommand.none )


view : Model -> S.Html Message
view (Model record) =
    Style.pageContainer
        [ Style.header (Just (Data.RolePlayer record.player))
        , mainView (Model record)
        , Style.playerBottomNavigation PageLocation.PlayerNote
        ]


mainView : Model -> S.Html Message
mainView model =
    Style.pageMainViewContainer
        [ S.div
            [ Html.Styled.Attributes.css
                [ Css.padding (Css.rem 0.5)
                , Style.displayGrid
                ]
            ]
            [ S.table
                [ Html.Styled.Attributes.css
                    [ Css.borderCollapse Css.collapse
                    , Css.border3 (Css.rem 0.2) Css.solid cycleBorderColor
                    , Css.borderRadius (Css.rem 0.5)
                    , Css.backgroundColor (Css.rgb 255 255 255)
                    ]
                ]
                [ pdcaTr "P" "仮説" []
                , pdcaTr "D" "検証" [ S.text "Dについて" ]
                , pdcaTr "C" "気づき" [ S.text "Cについて" ]
                , pdcaTr "A" "改善" [ S.text "Aについて" ]
                ]
            ]
        , Style.normalButton CreateNewPdca "新しいPDCAを作成"
        , Style.normalButton CreateNewCycle "新しいCycleを作成"
        ]


pdcaTr : String -> String -> List (S.Html Message) -> S.Html Message
pdcaTr alpha subText body =
    S.tr
        [ Html.Styled.Attributes.css
            [ Css.border3 (Css.rem 0.2) Css.solid cycleBorderColor
            , Css.height (Css.rem 6)
            ]
        ]
        [ S.td
            [ Html.Styled.Attributes.css
                [ Css.width (Css.rem 6)
                , Css.padding (Css.rem 0.5)
                , Css.color cycleBorderColor
                ]
            ]
            [ S.div
                [ Html.Styled.Attributes.css [ Css.fontSize (Css.rem 2) ] ]
                [ S.text alpha ]
            , S.div
                [ Html.Styled.Attributes.css [ Css.fontSize (Css.rem 1) ] ]
                [ S.text subText ]
            ]
        , S.td [] body
        ]


cycleBorderColor : Css.Color
cycleBorderColor =
    Css.rgb 168 165 188
