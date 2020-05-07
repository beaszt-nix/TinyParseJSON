module JSON (
        module JSON,
        pretty
            ) where

import TinyParse
import qualified Data.Map.Strict as Map
import System.IO
import Data.Maybe(fromMaybe)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

-- Pretty Printing Instance for Map
instance (Pretty k, Pretty v, Ord k) => Pretty (Map.Map k v) where
        pretty a = vsep ([pretty '{'] ++ (list f t) ++ [pretty '}']) 
                where
                   t        = Map.toList a
                   f (k,v)  = 
                        indent 2 $ ((fillBreak 10 $ 
                                 (pretty '\"'<> pretty k <> pretty '\"')) 
                             <+> hsep [pretty ':', pretty v , pretty ','])
                   f' (k,v)  = 
                        indent 2 $ ((fillBreak 10 $ 
                                (pretty '\"'<> pretty k <> pretty '\"')) 
                             <+> hsep [pretty ':', pretty v])
                   list f t = let n = init t
                                  l = last t in (map f n) ++ [f' l]

-- Pretty Printing instance for JsonVal
instance Pretty JsonVal where
        pretty (JsonNull)      = pretty "null"
        pretty (JsonBool a)    = pretty $ if a then "true" else "false"
        pretty (JsonStr a)     = pretty '\"' <> pretty a <> pretty '\"'
        pretty (JsonNum a)     = pretty a
        pretty (JsonObj m)     = if Map.empty == m then pretty "{}" else pretty m
        pretty (JsonArr a)     = prettyList a 

data JsonVal = JsonNull 
             | JsonBool Bool 
             | JsonStr String
             | JsonNum Double
             | JsonObj (Map.Map (String) JsonVal) 
             | JsonArr [JsonVal]
             deriving (Show,Eq)

jsonNull :: Parser JsonVal
jsonNull = JsonNull <$ string "null"

jsonBool :: Parser JsonVal 
jsonBool = true <|> false
           where  
                   true  = JsonBool True  <$ string "true"
                   false = JsonBool False <$ string "false"

jsonStr :: Parser JsonVal
jsonStr =  JsonStr <$> (char '"' *> stringLiteral <* char '"')

jsonNum :: Parser JsonVal
jsonNum = (JsonNum ) <$> number

jsonArr :: Parser JsonVal
jsonArr = JsonArr <$> (char '[' *> sepBy com element <* char ']')
          where
                  com = char ','

jsonObj :: Parser JsonVal
jsonObj =  f <$> (char '{' *> sepBy com member <* char '}')
          where
                  com = char ','
                  f :: [(JsonVal,JsonVal)] -> JsonVal
                  f x = JsonObj <$> Map.fromList $ 
                          map (\(JsonStr a, b) -> (a,b)) x 

element :: Parser JsonVal
element = (whiteSpace *> jsonVal <* whiteSpace)

member :: Parser (JsonVal,JsonVal)
member = comb <$> (whiteSpace *> jsonStr <* whiteSpace) <*> char ':' <*> element
         where
                comb :: JsonVal -> Char -> JsonVal -> (JsonVal,JsonVal)
                comb key _ val = (key,val) 

jsonVal :: Parser JsonVal
jsonVal = jsonNull <|> jsonBool <|> jsonStr <|> jsonArr <|> jsonObj <|> jsonNum

parsePrintFile   :: String -> (IO (Doc ann))
parsePrintFile inp = do
                   tok  <- loadJsonFromFile inp 
                   return $ pretty tok

loadJsonFromFile :: String -> (IO JsonVal)
loadJsonFromFile inp = do
                  str <- readFile inp
                  let 
                    tok = getJsonVal str
                  return tok

getJsonVal  :: String -> JsonVal
getJsonVal json = fst $ fromMaybe (JsonNull, "") $ parse jsonVal json

writeJsonToFile :: FilePath -> JsonVal -> IO ()
writeJsonToFile path json = do
                    writeFile path ""
                    handle <- openFile path WriteMode
                    renderIO handle (f json)
                    hClose handle
                    where
                      f input= unAnnotateS $ layoutSmart 
                                        (LayoutOptions (AvailablePerLine 60 1)) 
                                                    (pretty input) 
