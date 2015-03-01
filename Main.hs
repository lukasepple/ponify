import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import Data.Tuple
import Control.Applicative
import Options

ponify :: T.Text -> [(T.Text, T.Text)] -> T.Text
ponify input rules
  | null rules = input
  | otherwise  = ponify (uncurry replaceAll (head rules) input) (tail rules)

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

replaceAll :: T.Text -> T.Text -> T.Text -> T.Text
replaceAll needle replace haystack
  | isNothing nextMatch = haystack
  | otherwise           = let replaced = T.take (fst (fromJust nextMatch)) haystack `T.append`
                                replaceExpanded `T.append`
                                T.drop ((fst (fromJust nextMatch)) + (snd (fromJust nextMatch))) haystack in
                                    T.head replaced `T.cons` replaceAll needle replace (T.tail replaced)
  where nextMatch = match needle haystack 0
        replaceExpanded = if T.last replace == '*' then T.init replace else replace

match :: (Integral a, Num a) => T.Text -> T.Text -> a -> Maybe (a, a)
match pattern haystack currentIndex
  | T.null haystack  = Nothing
  | couldMatch       = Just (currentIndex, matchLength)
  | otherwise        = match pattern (T.tail haystack) (1 + currentIndex)
    where couldMatch = if T.last pattern == '*'
                        then T.take ((T.length pattern) - 1) haystack == T.take ((T.length pattern) - 1) pattern
                        else let afterMatch = T.drop (T.length pattern) haystack in
                               T.take (T.length pattern) haystack == pattern && (T.null afterMatch || (isSpace (T.head afterMatch)) || (isPunctuation (T.head afterMatch)))
          matchLength = if T.last pattern == '*'
                        then (fromIntegral (T.length pattern) - 1)
                        else (fromIntegral (T.length pattern))

data MainOptions = MainOptions
  {
    optPonify :: Bool,
    optDeponify :: Bool,
    optRules :: String
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "ponify" True
      "Whether to ponify"
    <*> simpleOption "deponify" False
      "Whether to deponify"
    <*> simpleOption "rules" "rules"
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
