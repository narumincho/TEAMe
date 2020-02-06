module SubCommand exposing (SubCommand(..), map)

import Data
import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet


{-| 各ページからメインのところへのコマンド
-}
type SubCommand message
    = AddNotification String
    | ChangeInputText { id : String, text : String }
    | Command (Cmd message)
    | UpdateUser Data.UserData
    | UpdateTeam Data.TeamData
    | None


map : (input -> output) -> SubCommand input -> SubCommand output
map func subCommand =
    case subCommand of
        AddNotification notification ->
            AddNotification notification

        ChangeInputText idAndText ->
            ChangeInputText idAndText

        Command command ->
            Command (command |> Cmd.map func)

        UpdateUser user ->
            UpdateUser user

        UpdateTeam team ->
            UpdateTeam team

        None ->
            None
