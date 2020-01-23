module Data exposing
    ( AccessToken
    , FileHash
    , accessTokenFromString
    , fileHashFromGraphQLScalaValue
    , fileHashToUrlAsString
    , timePosixFromGraphQLScalaValue
    , urlAsStringFromGraphQLScalaValue
    )

import Api.Scalar
import Graphql.Codec
import Json.Decode
import Json.Encode
import Time
import Url


type AccessToken
    = AccessToken String


type FileHash
    = FileHash String


accessTokenFromString : String -> AccessToken
accessTokenFromString =
    AccessToken


fileHashToUrlAsString : FileHash -> String
fileHashToUrlAsString (FileHash path) =
    "https://us-central1-teame-c1a32.cloudfunctions.net/file/" ++ path


timePosixFromGraphQLScalaValue : Api.Scalar.DateTime -> Time.Posix
timePosixFromGraphQLScalaValue (Api.Scalar.DateTime value) =
    value |> String.toInt |> Maybe.withDefault 0 |> Time.millisToPosix


fileHashFromGraphQLScalaValue : Api.Scalar.FileHash -> FileHash
fileHashFromGraphQLScalaValue (Api.Scalar.FileHash hash) =
    FileHash hash


urlAsStringFromGraphQLScalaValue : Api.Scalar.Url -> String
urlAsStringFromGraphQLScalaValue (Api.Scalar.Url url) =
    url
