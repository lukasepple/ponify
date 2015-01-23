import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Tuple
import Control.Applicative
import Options

ponify :: T.Text -> [(T.Text, T.Text)] -> T.Text
ponify input rules
  | null rules = input
  | otherwise  = ponify (uncurry globalReplace (head rules) input) (tail rules)

deponify :: T.Text -> [(T.Text, T.Text)] -> T.Text
deponify input rules = ponify input (map swap rules)

-- rules are just full text replaces with some extra glue
-- the markup of the rules file looks like this:
-- search :: replace
-- the word search gets replaced by replace

parseRules :: String -> [(T.Text, T.Text)]
parseRules = map (composeRule . words) . dropComments . lines
  where dropComments = filter (\x -> not (null x) && head x /= '#')
        composeRule list = mapTuple (T.pack . unwords) (take dotsIndex list, drop (dotsIndex + 1) list)
          where mapTuple f t = (f (fst t), f (snd t))
                dotsIndex = fromJust (elemIndex "::" list)

globalReplace :: T.Text -> T.Text -> T.Text -> T.Text
globalReplace needle repl haystack = if needle `T.isInfixOf` haystack
                                     then
                                      T.unlines $ map (T.unwords . replaceLine . T.words) (T.lines haystack)
                                     else
                                      haystack
  where replaceLine line = map (\word -> replaceWord word) line
        replaceWord word
          | word == needle = repl
          | needle `T.isInfixOf` word &&
            onlyPunctuation (foldl1 T.append (T.splitOn needle word)) = T.replace needle repl word
          | otherwise      = word
          where onlyPunctuation str = T.foldl (\acc c -> c `elem` ".?!,\"\' " && acc) True str

data MainOptions = MainOptions
  {
    optPonify :: Bool,
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
    putStr $ T.unpack $ action (T.pack contents) (parseRules rules)
