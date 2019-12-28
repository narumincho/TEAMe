module Api exposing (AccessToken(..), getLineLogInUrl)

import Http
import Json.Decode as Jd
import Json.Encode as Je
import Url


type AccessToken
    = AccessToken String


getLineLogInUrl : String -> (Result String Url.Url -> msg) -> Cmd msg
getLineLogInUrl path =
    graphQlApiRequest
        (Mutation
            [ Field
                { name = "getLineLogInUrl"
                , args =
                    [ ( "path", GraphQLString path )
                    ]
                , return = []
                }
            ]
        )
        (Jd.field "getLineLogInUrl" Jd.string
            |> Jd.andThen
                (\urlString ->
                    case Url.fromString urlString of
                        Just url ->
                            Jd.succeed url

                        Nothing ->
                            Jd.fail "url format error"
                )
        )



{- ==============================================================================
                              Graph QL Api Request
   ==============================================================================
-}


type Query
    = Mutation (List Field)
    | Query (List Field)


type Field
    = Field
        { name : String
        , args : List ( String, GraphQLValue )
        , return : List Field
        }


type GraphQLValue
    = GraphQLString String
    | GraphQLEnum String
    | GraphQLInt Int
    | GraphQLFloat Float
    | GraphQLObject (List ( String, GraphQLValue ))
    | GraphQLList (List GraphQLValue)
    | GraphQLNull


graphQlApiRequest : Query -> Jd.Decoder a -> (Result String a -> msg) -> Cmd.Cmd msg
graphQlApiRequest query responseDecoder callBack =
    Http.post
        { url = "https://us-central1-teame-c1a32.cloudfunctions.net/api"
        , body = graphQlRequestBody (queryToString query)
        , expect = Http.expectStringResponse callBack (graphQlResponseDecoder responseDecoder)
        }


queryToString : Query -> String
queryToString query =
    case query of
        Mutation fieldList ->
            "mutation {\n" ++ (fieldList |> List.map fieldToString |> String.join "\n") ++ "}"

        Query fieldList ->
            "{\n" ++ (fieldList |> List.map fieldToString |> String.join "\n") ++ "}"


fieldToString : Field -> String
fieldToString (Field { name, args, return }) =
    name
        ++ (if args == [] then
                ""

            else
                "("
                    ++ (args
                            |> List.map (\( argsName, argsValue ) -> argsName ++ ": " ++ graphQLValueToString argsValue)
                            |> String.join ", "
                       )
                    ++ ")"
           )
        ++ (if return == [] then
                ""

            else
                " {\n" ++ (return |> List.map fieldToString |> String.join "\n") ++ "\n}"
           )


graphQLValueToString : GraphQLValue -> String
graphQLValueToString graphQLValue =
    case graphQLValue of
        GraphQLString string ->
            string |> Je.string |> Je.encode 0

        GraphQLEnum string ->
            string

        GraphQLInt int ->
            String.fromInt int

        GraphQLFloat float ->
            String.fromFloat float

        GraphQLObject object ->
            "{"
                ++ (object
                        |> List.map (\( argsName, argsValue ) -> argsName ++ ": " ++ graphQLValueToString argsValue)
                        |> String.join ", "
                   )
                ++ "}"

        GraphQLList list ->
            "["
                ++ (list |> List.map graphQLValueToString |> String.join ", ")
                ++ "]"

        GraphQLNull ->
            "null"


nullableGraphQLValue : (a -> GraphQLValue) -> Maybe a -> GraphQLValue
nullableGraphQLValue func maybe =
    case maybe of
        Just a ->
            func a

        Nothing ->
            GraphQLNull


graphQlRequestBody : String -> Http.Body
graphQlRequestBody queryOrMutation =
    Http.jsonBody
        (Je.object
            [ ( "query"
              , Je.string queryOrMutation
              )
            ]
        )


graphQlResponseDecoder : Jd.Decoder a -> Http.Response String -> Result String a
graphQlResponseDecoder decoder response =
    case response of
        Http.BadUrl_ _ ->
            Err "BadURL"

        Http.Timeout_ ->
            Err "Timeout"

        Http.NetworkError_ ->
            Err "NetworkError"

        Http.BadStatus_ _ body ->
            case body |> Jd.decodeString graphQLErrorResponseDecoder of
                Ok message ->
                    Err message

                Err decodeError ->
                    Err (Jd.errorToString decodeError)

        Http.GoodStatus_ _ body ->
            body
                |> Jd.decodeString
                    (Jd.field "data"
                        decoder
                    )
                |> Result.mapError Jd.errorToString


graphQLErrorResponseDecoder : Jd.Decoder String
graphQLErrorResponseDecoder =
    Jd.field "errors"
        (Jd.list
            (Jd.field "message" Jd.string)
        )
        |> Jd.map (String.join ", ")
