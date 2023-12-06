module MyBase64 exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Bytes exposing (Bytes)
import Bytes.Encode
import Bytes.Decode
import Base64

decode : Decode.Decoder Bytes
decode = Decode.string |> Decode.andThen (\b64Str ->
  case Base64.toBytes b64Str of
    Just bytes -> Decode.succeed bytes
    Nothing -> Decode.fail "Invalid base64 string")

encode : Bytes -> Encode.Value
encode bytes = case Base64.fromBytes bytes of 
  Just str -> Encode.string str
  Nothing -> 
    Encode.string 
    """
    I was lied to. Here is the proof: 
    https://package.elm-lang.org/packages/danfishgold/base64-bytes/latest/Base64#fromBytes
    """