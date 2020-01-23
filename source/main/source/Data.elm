module Data exposing (AccessToken, FileHash, codecForGraphQlScalaValue, fileHashToUrlAsString)

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


fileHashToUrlAsString : FileHash -> String
fileHashToUrlAsString (FileHash path) =
    "https://us-central1-teame-c1a32.cloudfunctions.net/file/" ++ path


codecForGraphQlScalaValue : Api.Scalar.Codecs Time.Posix FileHash Url.Url
codecForGraphQlScalaValue =
    Api.Scalar.defineCodecs
        { codecDateTime =
            { encoder = \timePosix -> Json.Encode.int (Time.posixToMillis timePosix)
            , decoder = Json.Decode.int |> Json.Decode.map Time.millisToPosix
            }
        , codecFileHash =
            { encoder = \(FileHash path) -> Json.Encode.string path
            , decoder = Json.Decode.string |> Json.Decode.map FileHash
            }
        , codecUrl =
            { encoder = \url -> Json.Encode.string (Url.toString url)
            , decoder =
                Json.Decode.string
                    |> Json.Decode.andThen
                        (\urlAsString ->
                            case Url.fromString urlAsString of
                                Just url ->
                                    Json.Decode.succeed url

                                Nothing ->
                                    Json.Decode.fail ("url (" ++ urlAsString ++ ") is invalid.")
                        )
            }
        }
