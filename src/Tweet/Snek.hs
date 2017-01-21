module Tweet.Snek
    ( exec'
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Char
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Text.Madlibs
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Random.MWC
import Web.Tweet

-- | main executable
exec' :: IO ()
exec' = snekspeare >>= prompt (flip snekTweet ".credws")

prompt :: (TL.Text -> IO ()) -> TL.Text -> IO ()
prompt f input = do
    putStrLn . show $ ((text . TL.unpack) input) <> yellow "\n\n   Proceed? (y/N)"
    c <- getChar
    case (toLower c) of
        'y' -> f input
        'n' -> pure ()

-- | Given filepath to credentials and a `TL.Text`, tweet it
snekTweet :: TL.Text -> FilePath -> IO ()
snekTweet txt = thread (TL.unpack txt) mempty Nothing tweets
    where tweets = fromIntegral $ (TL.length txt) `div` 140 + 1

-- | A line from the snard (snail bard)
snekspeare :: IO TL.Text
snekspeare = ((pure . (`TL.append` "\n  -William Snakespeare")) <=< sReplace <=< pickLine <=< (pure . sepLines)) =<< readSnekspeare

-- | replace every s with a random number of them
sReplace :: TL.Text -> IO TL.Text
sReplace = (fmap (TL.pack . concat)) . (mapM (\c -> if c /= 's' && c /= 'S' then pure [c] else ((:) c) . T.unpack <$> runFile "madsrc/slither.mad")) . TL.unpack

-- | pick which line of shakespeare we want today
pickLine :: [TL.Text] -> IO TL.Text
pickLine lines = do
    i <- randNumRange (0, length lines)
    pure $ TL.dropWhile (==' ') $ TL.filter (\a -> and $ map (a/=) "[]\r\n") $ lines !! i

-- | separate into sentence
sepLines :: TL.Text -> [TL.Text]
sepLines = (fmap replacements) . filterLower . (filter (/="")) . (TL.split splitWhen)
    where splitWhen    = \a -> or $ map (a==) ".?!" 
          replacements = foldr ((.) . (flip TL.replace " ")) id (map (flip TL.replicate " ") [2..4])
          filterLower  = (map TL.pack) . (filter (any isLower)) . (map TL.unpack)

-- | read the file as a `TL.Text`
readSnekspeare :: IO TL.Text
readSnekspeare = TLIO.readFile "text/complete-works.txt"

-- | Generate random numbers from a given range.
randNumRange :: (Int, Int) -> IO Int
randNumRange ran = do
    let ran' = over both (fromIntegral) ran
    vs <- (withSystemRandom . asGenST $ \gen -> uniformR ran' gen) :: IO Double
    pure $ floor vs
