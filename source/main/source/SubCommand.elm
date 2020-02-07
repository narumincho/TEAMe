module SubCommand exposing
    ( SubCommand
    , addCommand
    , addNotification
    , batch
    , setInputText
    , getChangeInputTextList
    , getCommand
    , getNotificationList
    , getTeam
    , getUser
    , map
    , none
    , updateTeam
    , updateUser
    )

import Data
import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet
import Maybe.Extra


{-| 各ページからメインのところへのコマンド
-}
type SubCommand message
    = SubCommand
        { notificationList : List String
        , changeInputTextList : List { id : String, text : String }
        , newTeam : Maybe Data.TeamData
        , newUser : Maybe Data.UserData
        , command : Cmd message
        }


none : SubCommand message
none =
    SubCommand
        { notificationList = []
        , changeInputTextList = []
        , newTeam = Nothing
        , newUser = Nothing
        , command = Cmd.none
        }


addNotification : String -> SubCommand message
addNotification notification =
    SubCommand
        { notificationList = [ notification ]
        , changeInputTextList = []
        , newTeam = Nothing
        , newUser = Nothing
        , command = Cmd.none
        }


setInputText : { id : String, text : String } -> SubCommand message
setInputText idAndText =
    SubCommand
        { notificationList = []
        , changeInputTextList = [ idAndText ]
        , newTeam = Nothing
        , newUser = Nothing
        , command = Cmd.none
        }


updateTeam : Data.TeamData -> SubCommand message
updateTeam teamData =
    SubCommand
        { notificationList = []
        , changeInputTextList = []
        , newTeam = Just teamData
        , newUser = Nothing
        , command = Cmd.none
        }


updateUser : Data.UserData -> SubCommand message
updateUser userData =
    SubCommand
        { notificationList = []
        , changeInputTextList = []
        , newTeam = Nothing
        , newUser = Just userData
        , command = Cmd.none
        }


addCommand : Cmd message -> SubCommand message
addCommand command =
    SubCommand
        { notificationList = []
        , changeInputTextList = []
        , newTeam = Nothing
        , newUser = Nothing
        , command = command
        }


batch : List (SubCommand message) -> SubCommand message
batch subCommandList =
    case subCommandList of
        x :: xs ->
            SubCommand
                { notificationList = getNotificationList x ++ getNotificationList (batch xs)
                , changeInputTextList = getChangeInputTextList x ++ getChangeInputTextList (batch xs)
                , newTeam = getTeam x |> Maybe.Extra.or (getTeam (batch xs))
                , newUser = getUser x |> Maybe.Extra.or (getUser (batch xs))
                , command = Cmd.batch [ getCommand x, getCommand (batch xs) ]
                }

        [] ->
            none


getNotificationList : SubCommand message -> List String
getNotificationList (SubCommand { notificationList }) =
    notificationList


getChangeInputTextList : SubCommand message -> List { id : String, text : String }
getChangeInputTextList (SubCommand { changeInputTextList }) =
    changeInputTextList


getTeam : SubCommand message -> Maybe Data.TeamData
getTeam (SubCommand { newTeam }) =
    newTeam


getUser : SubCommand message -> Maybe Data.UserData
getUser (SubCommand { newUser }) =
    newUser


getCommand : SubCommand message -> Cmd message
getCommand (SubCommand { command }) =
    command


map : (input -> output) -> SubCommand input -> SubCommand output
map func (SubCommand record) =
    SubCommand
        { notificationList = record.notificationList
        , changeInputTextList = record.changeInputTextList
        , newTeam = record.newTeam
        , newUser = record.newUser
        , command = record.command |> Cmd.map func
        }
