
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Data.Int (Int64)


data ParseState = ParseState {
  string :: L.ByteString
, offset :: Int64
} deriving (Show)

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
}

identity :: a -> Parse a
identity a = Parse (\s -> Right (a,s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
 = case runParse parser (ParseState initState 0) of
     Left err            -> Left err
     Right (result, _)   -> Right result

