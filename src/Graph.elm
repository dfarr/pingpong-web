module Graph where

import Http
import String

type Graph
  = Graph (List Field)

type Field 
  = Field String (List Param) (List Field)

type Param
  = Param String Value
  
type Value
  = ValueNumber Int
  | ValueString String


graphToBody : Graph -> Http.Body
graphToBody graph
  = Http.string (graphToString graph)


graphToString : Graph -> String
graphToString graph
  = case graph of
      Graph listOfFields
        -> fieldListToString listOfFields


fieldListToString : List (Field) -> String
fieldListToString listOfFields
  = case listOfFields of
      []
        -> ""
      _
        -> "{" ++ String.join "," (List.map fieldToString listOfFields) ++ "}"


fieldToString : Field -> String
fieldToString field
  = case field of
      Field name listOfParams listOfFields
        -> name ++ (paramListToString listOfParams) ++ (fieldListToString listOfFields)


paramListToString : List (Param) -> String
paramListToString listOfParams
  = case listOfParams of
      []
        -> ""
      _
        -> "(" ++ String.join "," (List.map paramToString listOfParams) ++ ")"


paramToString : Param -> String
paramToString param
  = case param of
      Param label value
      -> label ++ ": " ++ (valueToString value)


valueToString : Value -> String
valueToString value
  = case value of
      ValueNumber number
        -> toString number
      ValueString string 
        -> "\"" ++ string ++ "\""
