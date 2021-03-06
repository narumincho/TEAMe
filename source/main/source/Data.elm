module Data exposing
    ( AccessToken
    , CycleData
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
    , apiUrl
    , cloudFunctionsOrigin
    , createCycle
    , createTeamAndSetManagerRole
    , fileHashFromGraphQLScalaValue
    , fileHashToUrlAsString
    , getAllTeam
    , getTeam
    , getUserByUserIdList
    , getUserNameAndImageFileHash
    , getUserPrivateData
    , joinTeamAndSetPlayerRole
    , managerGetGoal
    , managerGetImageFileHash
    , managerGetName
    , managerGetTeamId
    , playerAddCycle
    , playerGetCycleDataList
    , playerGetGoal
    , playerGetImageFileHash
    , playerGetName
    , playerGetTeamId
    , timePosixFromGraphQLScalaValue
    , updatePersonalGoal
    , updateTeamGoal
    , updateTeamInformation
    , urlAsStringFromGraphQLScalaValue
    , userGetGoal
    , userGetManager
    , userGetPlayer
    , validateTeamName
    )

import Api.Enum.Role
import Api.Mutation
import Api.Object
import Api.Object.Cycle
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


cloudFunctionsOrigin : String
cloudFunctionsOrigin =
    "https://us-central1-teame-c1a32.cloudfunctions.net"


apiUrl : String
apiUrl =
    cloudFunctionsOrigin ++ "/api"


accessTokenFromString : String -> AccessToken
accessTokenFromString =
    AccessToken


accessTokenToString : AccessToken -> String
accessTokenToString (AccessToken string) =
    string


fileHashToUrlAsString : FileHash -> String
fileHashToUrlAsString (FileHash path) =
    cloudFunctionsOrigin ++ "/file/" ++ path


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


userGetManager : UserData -> Maybe Manager
userGetManager userData =
    case userData of
        RoleManager manager ->
            Just manager

        _ ->
            Nothing


userGetPlayer : UserData -> Maybe Player
userGetPlayer user =
    case user of
        RolePlayer player ->
            Just player

        _ ->
            Nothing


userGetGoal : UserData -> Maybe String
userGetGoal user =
    case user of
        NoRole _ ->
            Nothing

        RoleManager manager ->
            Just (managerGetGoal manager)

        RolePlayer player ->
            Just (playerGetGoal player)


type Manager
    = Manager
        { id : UserId
        , name : String
        , imageFileHash : FileHash
        , goal : String
        , teamId : TeamId
        , createdAt : Time.Posix
        }


managerGetName : Manager -> String
managerGetName (Manager { name }) =
    name


managerGetImageFileHash : Manager -> FileHash
managerGetImageFileHash (Manager { imageFileHash }) =
    imageFileHash


managerGetGoal : Manager -> String
managerGetGoal (Manager { goal }) =
    goal


managerGetTeamId : Manager -> TeamId
managerGetTeamId (Manager { teamId }) =
    teamId


type Player
    = Player
        { id : UserId
        , name : String
        , imageFileHash : FileHash
        , goal : String
        , teamId : TeamId
        , createdAt : Time.Posix
        , cycleList : List CycleData
        }


type CycleId
    = CycleId String


type alias CycleData =
    { id : CycleId
    , plan : String
    , do : String
    , check : String
    , act : String
    , createdAt : Time.Posix
    }


playerAddCycle : CycleData -> Player -> Player
playerAddCycle cycleData (Player record) =
    Player
        { record | cycleList = record.cycleList ++ [ cycleData ] }


playerGetName : Player -> String
playerGetName (Player { name }) =
    name


playerGetImageFileHash : Player -> FileHash
playerGetImageFileHash (Player { imageFileHash }) =
    imageFileHash


playerGetGoal : Player -> String
playerGetGoal (Player { goal }) =
    goal


playerGetTeamId : Player -> TeamId
playerGetTeamId (Player { teamId }) =
    teamId


playerGetCycleDataList : Player -> List CycleData
playerGetCycleDataList (Player { cycleList }) =
    cycleList


type TeamId
    = TeamId String


type alias TeamData =
    { id : TeamId
    , name : String
    , goal : String
    , information : String
    , managerId : UserId
    , playerIdList : List UserId
    , createdAt : Time.Posix
    }


getUserNameAndImageFileHash : UserData -> { name : String, imageFileHash : FileHash }
getUserNameAndImageFileHash userData =
    case userData of
        NoRole record ->
            { name = record.name
            , imageFileHash = record.imageFileHash
            }

        RolePlayer (Player record) ->
            { name = record.name
            , imageFileHash = record.imageFileHash
            }

        RoleManager (Manager record) ->
            { name = record.name
            , imageFileHash = record.imageFileHash
            }


validateTeamName : String -> Bool
validateTeamName teamName =
    let
        timedTeamName =
            String.trim teamName
    in
    0 < String.length timedTeamName


getUserPrivateData : AccessToken -> Graphql.SelectionSet.SelectionSet UserData Graphql.Operation.RootQuery
getUserPrivateData accessToken =
    Api.Query.userPrivate { accessToken = accessTokenToString accessToken } userDataQuery


getUserByUserIdList : List UserId -> Graphql.SelectionSet.SelectionSet (List UserData) Graphql.Operation.RootQuery
getUserByUserIdList userIdList =
    Graphql.SelectionSet.list
        (userIdList
            |> List.map
                (\(UserId userIdAsString) ->
                    Api.Query.user { userId = userIdAsString } userDataQuery
                )
        )


getTeam : TeamId -> Graphql.SelectionSet.SelectionSet TeamData Graphql.Operation.RootQuery
getTeam (TeamId teamIdAsString) =
    Api.Query.team { id = teamIdAsString }
        teamDataQuery


getAllTeam : Graphql.SelectionSet.SelectionSet (List TeamData) Graphql.Operation.RootQuery
getAllTeam =
    Api.Query.allTeam
        teamDataQuery


createTeamAndSetManagerRole : AccessToken -> String -> Graphql.SelectionSet.SelectionSet UserData Graphql.Operation.RootMutation
createTeamAndSetManagerRole accessToken teamName =
    Api.Mutation.createTeamAndSetManagerRole
        { accessToken = accessTokenToString accessToken
        , teamName = teamName
        }
        userDataQuery


joinTeamAndSetPlayerRole : AccessToken -> TeamId -> Graphql.SelectionSet.SelectionSet UserData Graphql.Operation.RootMutation
joinTeamAndSetPlayerRole accessToken (TeamId teamIdAsString) =
    Api.Mutation.joinTeamAndSetPlayerRole
        { accessToken = accessTokenToString accessToken
        , teamId = teamIdAsString
        }
        userDataQuery


updatePersonalGoal : AccessToken -> String -> Graphql.SelectionSet.SelectionSet UserData Graphql.Operation.RootMutation
updatePersonalGoal accessToken goal =
    Api.Mutation.updatePersonalGoal
        { accessToken = accessTokenToString accessToken
        , goal = goal
        }
        userDataQuery


updateTeamGoal : AccessToken -> String -> Graphql.SelectionSet.SelectionSet TeamData Graphql.Operation.RootMutation
updateTeamGoal accessToken goal =
    Api.Mutation.updateTeamGoal
        { accessToken = accessTokenToString accessToken
        , goal = goal
        }
        teamDataQuery


updateTeamInformation : AccessToken -> String -> Graphql.SelectionSet.SelectionSet TeamData Graphql.Operation.RootMutation
updateTeamInformation accessToken information =
    Api.Mutation.updateTeamInformation
        { accessToken = accessTokenToString accessToken
        , information = information
        }
        teamDataQuery


createCycle : AccessToken -> { plan : String, do : String, check : String, act : String } -> Graphql.SelectionSet.SelectionSet CycleData Graphql.Operation.RootMutation
createCycle accessToken cycleData =
    Api.Mutation.createCycle
        { accessToken = accessTokenToString accessToken
        , plan = cycleData.plan
        , do = cycleData.do
        , check = cycleData.check
        , act = cycleData.act
        }
        cycleQuery


teamDataQuery : Graphql.SelectionSet.SelectionSet TeamData Api.Object.Team
teamDataQuery =
    Graphql.SelectionSet.map7
        (\id name goal information managerId playerIdList createdAt ->
            { id = TeamId id
            , name = name
            , goal = goal
            , information = information
            , managerId = UserId managerId
            , playerIdList = playerIdList |> List.map UserId
            , createdAt = timePosixFromGraphQLScalaValue createdAt
            }
        )
        Api.Object.Team.id
        Api.Object.Team.name
        Api.Object.Team.goal
        Api.Object.Team.information
        (Api.Object.Team.manager Api.Object.UserData.id)
        (Api.Object.Team.playerList Api.Object.UserData.id)
        Api.Object.Team.createdAt


userDataQuery : Graphql.SelectionSet.SelectionSet UserData Api.Object.UserData
userDataQuery =
    Graphql.SelectionSet.map8
        (\id name imageFileHash goal teamIdMaybe createdAt roleMaybe cycleList ->
            case ( roleMaybe, teamIdMaybe ) of
                ( Just Api.Enum.Role.Manager, Just teamId ) ->
                    RoleManager
                        (Manager
                            { id = UserId id
                            , name = name
                            , imageFileHash = fileHashFromGraphQLScalaValue imageFileHash
                            , goal = goal
                            , teamId = TeamId teamId
                            , createdAt = timePosixFromGraphQLScalaValue createdAt
                            }
                        )

                ( Just Api.Enum.Role.Player, Just teamId ) ->
                    RolePlayer
                        (Player
                            { id = UserId id
                            , name = name
                            , imageFileHash = fileHashFromGraphQLScalaValue imageFileHash
                            , goal = goal
                            , teamId = TeamId teamId
                            , createdAt = timePosixFromGraphQLScalaValue createdAt
                            , cycleList = cycleList
                            }
                        )

                ( _, _ ) ->
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
        Api.Object.UserData.goal
        (Api.Object.UserData.team
            Api.Object.Team.id
        )
        Api.Object.UserData.createdAt
        Api.Object.UserData.role
        (Api.Object.UserData.cycleList cycleQuery)


cycleQuery : Graphql.SelectionSet.SelectionSet CycleData Api.Object.Cycle
cycleQuery =
    Graphql.SelectionSet.map6
        (\id plan do check act createdAt ->
            { id = CycleId id
            , plan = plan
            , do = do
            , check = check
            , act = act
            , createdAt = timePosixFromGraphQLScalaValue createdAt
            }
        )
        Api.Object.Cycle.id
        Api.Object.Cycle.plan
        Api.Object.Cycle.do
        Api.Object.Cycle.check
        Api.Object.Cycle.act
        Api.Object.Cycle.createdAt
