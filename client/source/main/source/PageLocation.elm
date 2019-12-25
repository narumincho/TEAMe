module PageLocation exposing (InitPageLocation(..), initFromUrl, initToUrlAsString)

import Api
import Dict
import Erl
import Url
import Url.Builder


type InitPageLocation
    = InitMyPage
    | InitNote
    | InitTeam


initFromUrl : Url.Url -> ( Maybe Api.AccessToken, Maybe InitPageLocation )
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
        |> Maybe.map Api.AccessToken
    , [ myPageParser |> parserMap (always InitMyPage) ]
        |> List.map (\f -> f path)
        |> oneOf
    )


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
initToUrlAsString : InitPageLocation -> String
initToUrlAsString location =
    case location of
        InitMyPage ->
            Url.Builder.absolute myPagePath []

        InitNote ->
            Url.Builder.absolute notePath []

        InitTeam ->
            Url.Builder.absolute teamPath []


myPageParser : List String -> Maybe ()
myPageParser path =
    if path == myPagePath then
        Just ()

    else
        Nothing


myPagePath : List String
myPagePath =
    []


noteParser : List String -> Maybe ()
noteParser path =
    if path == notePath then
        Just ()

    else
        Nothing


notePath : List String
notePath =
    [ "note" ]


teamParser : List String -> Maybe ()
teamParser path =
    if path == teamPath then
        Just ()

    else
        Nothing


teamPath : List String
teamPath =
    [ "team" ]
