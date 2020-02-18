module PlayerPage.Note exposing (Message, Model, init, update, view)

import Css
import Data
import Graphql.Http
import Html.Styled as S
import Html.Styled.Attributes
import PageLocation
import Style
import SubCommand


type Model
    = View
    | NewCycle { updating : Bool, plan : String, do : String, check : String, act : String }


type Message
    = CreateNewCycle
    | InputPlan String
    | InputDo String
    | InputCheck String
    | InputAct String
    | SentCreateNewCycle
    | SentCreateNewCycleResponse (Result (Graphql.Http.Error Data.CycleData) Data.CycleData)


update : Data.AccessToken -> Data.Player -> Message -> Model -> ( Model, SubCommand.SubCommand Message )
update accessToken player message model =
    case ( message, model ) of
        ( CreateNewCycle, View ) ->
            ( NewCycle { updating = False, plan = "", do = "", check = "", act = "" }
            , SubCommand.none
            )

        ( InputPlan plan, NewCycle record ) ->
            ( NewCycle { record | plan = plan }
            , SubCommand.none
            )

        ( InputDo do, NewCycle record ) ->
            ( NewCycle { record | do = do }
            , SubCommand.none
            )

        ( InputCheck check, NewCycle record ) ->
            ( NewCycle { record | check = check }
            , SubCommand.none
            )

        ( InputAct act, NewCycle record ) ->
            ( NewCycle { record | act = act }
            , SubCommand.none
            )

        ( SentCreateNewCycle, NewCycle record ) ->
            ( NewCycle { record | updating = True }
            , Graphql.Http.mutationRequest Data.apiUrl
                (Data.createCycle accessToken
                    { plan = record.plan
                    , do = record.do
                    , check = record.check
                    , act = record.act
                    }
                )
                |> Graphql.Http.send SentCreateNewCycleResponse
                |> SubCommand.addCommand
            )

        ( SentCreateNewCycleResponse response, NewCycle record ) ->
            case response of
                Ok cycleData ->
                    ( View
                    , SubCommand.updateUser
                        (player |> Data.playerAddCycle cycleData |> Data.RolePlayer)
                    )

                Err _ ->
                    ( NewCycle { record | updating = False }
                    , SubCommand.addNotification "Cycleの作成に失敗しました"
                    )

        ( _, _ ) ->
            ( model, SubCommand.none )


init : Data.Player -> ( Model, SubCommand.SubCommand Message )
init player =
    ( View
    , SubCommand.none
    )


view : Data.Player -> Model -> S.Html Message
view player model =
    Style.pageContainer
        [ Style.header (Just (Data.RolePlayer player))
        , mainView player model
        , Style.playerBottomNavigation PageLocation.PlayerNote
        ]


mainView : Data.Player -> Model -> S.Html Message
mainView player model =
    case model of
        View ->
            Style.pageMainViewContainer
                ((player |> Data.playerGetCycleDataList |> List.map cycleView)
                    ++ [ Style.normalButton CreateNewCycle "新しいCycleを作成"
                       ]
                )

        NewCycle cycleData ->
            cycleEditor cycleData


cycleView : Data.CycleData -> S.Html Message
cycleView cycleData =
    S.div
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
            [ pdcaTr "P" "仮説" [ S.text cycleData.plan ]
            , pdcaTr "D" "検証" [ S.text cycleData.do ]
            , pdcaTr "C" "気づき" [ S.text cycleData.check ]
            , pdcaTr "A" "改善" [ S.text cycleData.act ]
            ]
        ]


cycleEditor : { updating : Bool, plan : String, do : String, check : String, act : String } -> S.Html Message
cycleEditor record =
    Style.pageMainViewContainer
        (if record.updating then
            [ S.div [] [ S.text "作成中……", Style.loading ] ]

         else
            [ S.div
                []
                [ S.div
                    []
                    [ pdcaEditorTitle "P" "lan" "仮説"
                    , S.div [] [ S.text "練習のタイプ、課題、解決方法、未来" ]
                    , Style.multiLineTextBox "plan" "plan" InputPlan
                    ]
                , S.div
                    []
                    [ pdcaEditorTitle "D" "o" "検証"
                    , S.div []
                        [ S.span [] [ S.text "仮説と現実の" ]
                        , S.span
                            [ Html.Styled.Attributes.css
                                [ Css.textDecoration Css.underline ]
                            ]
                            [ S.text "ギャップ" ]
                        , S.span [] [ S.text "について書く" ]
                        ]
                    , Style.multiLineTextBox "do" "do" InputDo
                    ]
                , S.div
                    []
                    [ pdcaEditorTitle "C" "heck" "気づき"
                    , S.div []
                        [ S.span [] [ S.text "検証に対する" ]
                        , S.span
                            [ Html.Styled.Attributes.css
                                [ Css.textDecoration Css.underline ]
                            ]
                            [ S.text "感じたこと" ]
                        , S.span [] [ S.text "や" ]
                        , S.span
                            [ Html.Styled.Attributes.css
                                [ Css.textDecoration Css.underline ]
                            ]
                            [ S.text "考えたこと" ]
                        , S.span [] [ S.text "を書く" ]
                        ]
                    , Style.multiLineTextBox "check" "check" InputCheck
                    ]
                , S.div
                    []
                    [ pdcaEditorTitle "A" "ction" "改善"
                    , S.div [] [ S.text "次のPDCAに向けての改善したい点を書く" ]
                    , Style.multiLineTextBox "act" "act" InputAct
                    ]
                ]
            , Style.normalButton SentCreateNewCycle "Cycleを作成"
            ]
        )


pdcaEditorTitle : String -> String -> String -> S.Html Message
pdcaEditorTitle firstChar otherChars text =
    S.div
        [ Html.Styled.Attributes.css
            [ Style.displayGrid
            , Style.gridAutoFlowColumn
            , Css.justifyContent Css.center
            , Css.alignItems Css.baseline
            , Style.gap 1
            ]
        ]
        [ S.div
            []
            [ S.span [ Html.Styled.Attributes.css [ Css.fontSize (Css.rem 3) ] ] [ S.text firstChar ]
            , S.span [ Html.Styled.Attributes.css [ Css.fontSize (Css.rem 1.5) ] ] [ S.text otherChars ]
            ]
        , S.span [ Html.Styled.Attributes.css [ Css.fontSize (Css.rem 1.5) ] ] [ S.text text ]
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
