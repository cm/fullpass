module Rest exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Http exposing (jsonBody, Error, Header, header)


headers : List Header
headers =
    []


get : String -> Decoder a -> (Result Error a -> msg) -> Cmd msg
get url decoder msg =
    Http.request
        { method = "GET"
        , headers = headers
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg


post : String -> Http.Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
post url body decoder msg =
    Http.request
        { method = "POST"
        , headers = headers
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg


put : String -> Http.Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
put url body decoder msg =
    Http.request
        { method = "PUT"
        , headers = headers
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg


delete : String -> Decoder a -> (Result Error a -> msg) -> Cmd msg
delete url decoder msg =
    Http.request
        { method = "DELETE"
        , headers = headers
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg
