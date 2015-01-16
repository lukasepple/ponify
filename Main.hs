import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Tuple
import Control.Applicative
import Options

ponify :: T.Text -> [(T.Text, T.Text)] -> T.Text
ponify input rules
  | null rules = input
  | otherwise  = ponify (uncurry T.replace (head rules) input) (tail rules)

deponify :: T.Text -> [(T.Text, T.Text)] -> T.Text
deponify input rules = ponify input (map swap rules)

parseRules :: String -> [(T.Text, T.Text)]
parseRules = map (composeRule . words) . dropComments . lines
  where dropComments = filter (\x -> not (null x) && head x /= '#')
        composeRule list = mapTuple (T.pack . unwords) (take dotsIndex list, drop (dotsIndex + 1) list)
          where mapTuple f t = (f (fst t), f (snd t))
                dotsIndex = fromJust (elemIndex "::" list)

data MainOptions = MainOptions
  { optPonify :: Bool,
    optDeponify :: Bool,
    optRules :: String
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "ponify" True
      "Wether to ponify"
    <*> simpleOption "deponify" False
      "Wether to deponify"
    <*> simpleOption "rules" "rules.txt"
      "Which rules file to use"

main :: IO ()
main = runCommand $ \opts args -> do
  -- get Stdin
  -- load rules
  contents <- getContents
  rules <- readFile (optRules opts)
  -- process the input
  let action = if (optDeponify opts) then deponify else ponify in
    putStr (T.unpack (action (T.pack contents) (parseRules rules)))
