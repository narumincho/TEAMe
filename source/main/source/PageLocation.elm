module PageLocation exposing
    ( PageLocation(..)
    , fromUrl
    , initFromUrl
    , toUrlAsString
    )

import Data
import Dict
import Erl
import Url
import Url.Builder


type PageLocation
    = Top
    | PlayerNote
    | PlayerPdcaForm
    | Team
    | ManagerPdcaEditor


initFromUrl : Url.Url -> ( Maybe Data.AccessToken, PageLocation )
initFromUrl url =
    let
        { path, hash } =
            url
                |> Url.toString
                |> Erl.parse

        fragmentDict =
            (Erl.parse ("?" ++ hash)).query
                |> Dict.fromList
    in
    ( fragmentDict
        |> Dict.get "accessToken"
        |> Maybe.map Data.accessTokenFromString
    , fromUrl url
    )


fromUrl : Url.Url -> PageLocation
fromUrl url =
    let
        { path, hash } =
            url
                |> Url.toString
                |> Erl.parse
    in
    [ topParser |> parserMap (always Top)
    , playerNoteParser |> parserMap (always PlayerNote)
    , playerPdcaFormParser |> parserMap (always PlayerPdcaForm)
    , playerTeamParser |> parserMap (always Team)
    , managerPdcaEditorParser |> parserMap (always ManagerPdcaEditor)
    ]
        |> List.map (\f -> f path)
        |> oneOf
        |> Maybe.withDefault Top


parserMap : (a -> b) -> (List String -> Maybe a) -> (List String -> Maybe b)
parserMap f parser path =
    parser path |> Maybe.map f


oneOf : List (Maybe a) -> Maybe a
oneOf list =
    case list of
        (Just x) :: _ ->
            Just x

        Nothing :: xs ->
            oneOf xs

        [] ->
            Nothing


{-| ページの場所から、文字列で表現したURLに変換する。一方通行のページの場合、ホームのURLを返す
-}
toUrlAsString : PageLocation -> String
toUrlAsString location =
    (case location of
        Top ->
            ( topPath, [] )

        PlayerNote ->
            ( playerNotePath, [] )

        PlayerPdcaForm ->
            ( playerPdcaFormPath, [] )

        Team ->
            ( playerTeamPath, [] )

        ManagerPdcaEditor ->
            ( managerPdcaEditorPath, [] )
    )
        |> (\( path, query ) -> Url.Builder.absolute path query)


topParser : List String -> Maybe ()
topParser path =
    if path == topPath then
        Just ()

    else
        Nothing


topPath : List String
topPath =
    []


playerNoteParser : List String -> Maybe ()
playerNoteParser path =
    if path == playerNotePath then
        Just ()

    else
        Nothing


playerNotePath : List String
playerNotePath =
    [ "note" ]


playerPdcaFormParser : List String -> Maybe ()
playerPdcaFormParser path =
    if path == playerPdcaFormPath then
        Just ()

    else
        Nothing


playerPdcaFormPath : List String
playerPdcaFormPath =
    [ "pdca-form" ]


playerTeamParser : List String -> Maybe ()
playerTeamParser path =
    if path == playerTeamPath then
        Just ()

    else
        Nothing


playerTeamPath : List String
playerTeamPath =
    [ "team" ]


managerPdcaEditorParser : List String -> Maybe ()
managerPdcaEditorParser path =
    if path == managerPdcaEditorPath then
        Just ()

    else
        Nothing


managerPdcaEditorPath : List String
managerPdcaEditorPath =
    [ "pdca-editor" ]
