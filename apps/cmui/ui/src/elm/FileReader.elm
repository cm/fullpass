module FileReader
    exposing
        ( Error(..)
        , FileContentArrayBuffer
        , FileContentDataUrl
        , FileRef
        , NativeFile
        , filePart
        , onFileChange
        , parseDroppedFiles
        , parseSelectedFiles
        , prettyPrint
        , rawBody
        , readAsArrayBuffer
        , readAsDataUrl
        , readAsTextFile
        )

import Html exposing (Attribute)
import Html.Events exposing (on)
import Http exposing (Body, Part)
import Json.Decode as Json exposing (Decoder, Value)
import MimeType
import Native.FileReader
import Task exposing (Task, fail)


type alias FileRef =
    Value


type alias FileContentArrayBuffer =
    Value


type alias FileContentDataUrl =
    Value


type Error
    = NoValidBlob
    | ReadFail
    | NotTextFile


readAsTextFile : FileRef -> Task Error String
readAsTextFile fileRef =
    if isTextFile fileRef then
        Native.FileReader.readAsTextFile fileRef
    else
        fail NotTextFile


readAsArrayBuffer : FileRef -> Task Error FileContentArrayBuffer
readAsArrayBuffer fileRef =
    Native.FileReader.readAsArrayBuffer fileRef


readAsDataUrl : FileRef -> Task Error FileContentDataUrl
readAsDataUrl fileRef =
    Native.FileReader.readAsDataUrl fileRef


filePart : String -> NativeFile -> Part
filePart name nf =
    Native.FileReader.filePart name nf.blob


rawBody : String -> NativeFile -> Body
rawBody mimeType nf =
    Native.FileReader.rawBody mimeType nf.blob


prettyPrint : Error -> String
prettyPrint err =
    case err of
        ReadFail ->
            "File reading error"

        NoValidBlob ->
            "Blob was not valid"

        NotTextFile ->
            "Not a text file"


type alias NativeFile =
    { name : String
    , size : Int
    , mimeType : Maybe MimeType.MimeType
    , blob : FileRef
    }


onFileChange : (List NativeFile -> msg) -> Attribute msg
onFileChange msg =
    on "change" (Json.map msg parseSelectedFiles)


parseSelectedFiles : Decoder (List NativeFile)
parseSelectedFiles =
    fileParser "target"


parseDroppedFiles : Decoder (List NativeFile)
parseDroppedFiles =
    fileParser "dataTransfer"


isTextFile : FileRef -> Bool
isTextFile fileRef =
    case Json.decodeValue mtypeDecoder fileRef of
        Ok (Just (MimeType.Text text)) ->
            True

        Ok Nothing ->
            True

        _ ->
            False


fileParser : String -> Decoder (List NativeFile)
fileParser fieldName =
    Json.field fieldName <|
        Json.field "files" <|
            fileListDecoder nativeFileDecoder


fileListDecoder : Decoder a -> Decoder (List a)
fileListDecoder decoder =
    let
        decodeFileValues indexes =
            indexes
                |> List.map (\index -> Json.field (toString index) decoder)
                |> List.foldr (Json.map2 (::)) (Json.succeed [])
    in
    Json.field "length" Json.int
        |> Json.map (\i -> List.range 0 (i - 1))
        |> Json.andThen decodeFileValues


mtypeDecoder : Decoder (Maybe MimeType.MimeType)
mtypeDecoder =
    Json.map MimeType.parseMimeType (Json.field "type" Json.string)


nativeFileDecoder : Decoder NativeFile
nativeFileDecoder =
    Json.map4 NativeFile
        (Json.field "name" Json.string)
        (Json.field "size" Json.int)
        mtypeDecoder
        Json.value
