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
    = MyPage
    | Note
    | Team


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
    , [ myPageParser |> parserMap (always MyPage)
      , noteParser |> parserMap (always Note)
      , teamParser |> parserMap (always Team)
      ]
        |> List.map (\f -> f path)
        |> oneOf
        |> Maybe.withDefault MyPage
    )


fromUrl : Url.Url -> PageLocation
fromUrl url =
    let
        { path, hash } =
            url
                |> Url.toString
                |> Erl.parse
    in
    [ myPageParser |> parserMap (always MyPage)
    , noteParser |> parserMap (always Note)
    , teamParser |> parserMap (always Team)
    ]
        |> List.map (\f -> f path)
        |> oneOf
        |> Maybe.withDefault MyPage


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
    case location of
        MyPage ->
            Url.Builder.absolute myPagePath []

        Note ->
            Url.Builder.absolute notePath []

        Team ->
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
