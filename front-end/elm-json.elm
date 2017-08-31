module Main exposing (..)

import Json.Decode exposing (field)


type alias ComplexType =
    { artist : String
    , title : String
    , time : String
    , timeStamp : String
    }


decodeComplexType : Json.Decode.Decoder ComplexType
decodeComplexType =
    Json.Decode.map4 ComplexType
        (field "artist" Json.Decode.string)
        (field "title" Json.Decode.string)
        (field "time" Json.Decode.string)
        (field "timeStamp" Json.Decode.string)



{-
   If successful, create a record of the type Something
   It must have a field called `artist` with the type String
   It must have a field called `title` with the type String
   It must have a field called `time` with the type String
   It must have a field called `timeStamp` with the type String
-}


type alias Something =
    { latestFive : List ComplexType
    }


decodeSomething : Json.Decode.Decoder Something
decodeSomething =
    Json.Decode.map Something
        (field "latestFive" (Json.Decode.list decodeComplexType))



{-
   If successful, create a record of the type Something
   It must have a field called `latestFive` with the type List ComplexType
-}
