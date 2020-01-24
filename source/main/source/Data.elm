module Data exposing
    ( AccessToken
    , FileHash
    , Manager
    , NoRoleUser
    , Player
    , TeamData
    , TeamId
    , UserData(..)
    , UserId
    , accessTokenFromString
    , accessTokenToString
    , fileHashFromGraphQLScalaValue
    , fileHashToUrlAsString
    , getAllTeam
    , getUserData
    , timePosixFromGraphQLScalaValue
    , urlAsStringFromGraphQLScalaValue
    )

import Api.Enum.Role
import Api.Object.Team
import Api.Object.UserData
import Api.Query
import Api.Scalar
import Graphql.Operation
import Graphql.SelectionSet
import Time


type AccessToken
    = AccessToken String


type FileHash
    = FileHash String


accessTokenFromString : String -> AccessToken
accessTokenFromString =
    AccessToken


accessTokenToString : AccessToken -> String
accessTokenToString (AccessToken string) =
    string


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


type UserId
    = UserId String


type UserData
    = NoRole NoRoleUser
    | RoleManager Manager
    | RolePlayer Player


type alias NoRoleUser =
    { id : UserId
    , name : String
    , imageFileHash : FileHash
    , createdAt : Time.Posix
    }


type Manager
    = Manager
        { id : UserId
        , name : String
        , imageFileHash : FileHash
        , createdAt : Time.Posix
        }


type Player
    = Player
        { id : UserId
        , name : String
        , imageFileHash : FileHash
        , createdAt : Time.Posix
        }


type TeamId
    = TeamId String


type alias TeamData =
    { id : TeamId
    , name : String
    , managerId : UserId
    , playerIdList : List UserId
    , createdAt : Time.Posix
    }


getUserData : AccessToken -> Graphql.SelectionSet.SelectionSet UserData Graphql.Operation.RootQuery
getUserData accessToken =
    Api.Query.userPrivate { accessToken = accessTokenToString accessToken }
        (Graphql.SelectionSet.map5
            (\id name imageFileHash createdAt roleMaybe ->
                case roleMaybe of
                    Just Api.Enum.Role.Manager ->
                        RoleManager
                            (Manager
                                { id = UserId id
                                , name = name
                                , imageFileHash = fileHashFromGraphQLScalaValue imageFileHash
                                , createdAt = timePosixFromGraphQLScalaValue createdAt
                                }
                            )

                    Just Api.Enum.Role.Player ->
                        RolePlayer
                            (Player
                                { id = UserId id
                                , name = name
                                , imageFileHash = fileHashFromGraphQLScalaValue imageFileHash
                                , createdAt = timePosixFromGraphQLScalaValue createdAt
                                }
                            )

                    Nothing ->
                        NoRole
                            { id = UserId id
                            , name = name
                            , imageFileHash = fileHashFromGraphQLScalaValue imageFileHash
                            , createdAt = timePosixFromGraphQLScalaValue createdAt
                            }
            )
            Api.Object.UserData.id
            Api.Object.UserData.name
            Api.Object.UserData.imageFileHash
            Api.Object.UserData.createdAt
            Api.Object.UserData.role
        )


getAllTeam : Graphql.SelectionSet.SelectionSet (List TeamData) Graphql.Operation.RootQuery
getAllTeam =
    Api.Query.allTeam
        (Graphql.SelectionSet.map5
            (\id name managerId playerIdList createdAt ->
                { id = TeamId id
                , name = name
                , managerId = UserId managerId
                , playerIdList =
                    -- TODO playerListがnullableになってしまっている?
                    playerIdList
                        |> List.map
                            (\playerId ->
                                playerId |> List.map (Maybe.withDefault "???") |> String.join "," |> UserId
                            )
                , createdAt = timePosixFromGraphQLScalaValue createdAt
                }
            )
            Api.Object.Team.id
            Api.Object.Team.name
            (Api.Object.Team.manager Api.Object.UserData.id)
            (Api.Object.Team.playerList Api.Object.UserData.id)
            Api.Object.Team.createdAt
        )
