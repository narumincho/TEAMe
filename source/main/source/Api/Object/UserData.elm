-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.UserData exposing (..)

import Api.Enum.Role
import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| ユーザー識別するためのID
-}
id : SelectionSet String Api.Object.UserData
id =
    Object.selectionForField "String" "id" [] Decode.string


{-| ユーザー名
-}
name : SelectionSet String Api.Object.UserData
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| ユーザーのプロフィール画像のファイルハッシュ
-}
imageFileHash : SelectionSet Api.ScalarCodecs.FileHash Api.Object.UserData
imageFileHash =
    Object.selectionForField "ScalarCodecs.FileHash" "imageFileHash" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecFileHash |> .decoder)


{-| ユーザーの役割
-}
role : SelectionSet (Maybe Api.Enum.Role.Role) Api.Object.UserData
role =
    Object.selectionForField "(Maybe Enum.Role.Role)" "role" [] (Api.Enum.Role.decoder |> Decode.nullable)


{-| ユーザーが作られた日時
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.UserData
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| 所属しているチーム
-}
team : SelectionSet decodesTo Api.Object.Team -> SelectionSet decodesTo Api.Object.UserData
team object_ =
    Object.selectionForCompositeField "team" [] object_ identity
