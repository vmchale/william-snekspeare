{-# LANGUAGE TypeOperators #-}

module Tweet.Snek
    ( exec'
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Options.Generic
import Text.Madlibs
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Random.MWC
import Web.Tweet hiding (text)

data Program = Program { noprompt :: Bool           <?> "whether to proceed before tweeting (avoid shitposts)"
                       , cred     :: Maybe FilePath <?> "filepath to credentials file"
                       } deriving (Generic)

instance ParseRecord Program

-- | main executable
exec' :: IO ()
exec' = do
    x <- getRecord "William SnakeSpeare"
    let file = (fromMaybe ".credws") . unHelpful . cred $ x
    if (unHelpful . noprompt) x then
        snekspeare >>= flip snekTweet file
    else
        snekspeare >>= proceed (flip snekTweet file)

-- | Utility function to ask before tweeting
proceed :: (TL.Text -> IO ()) -> TL.Text -> IO ()
proceed f input = do
    print $ ((text . TL.unpack) input) <> dullyellow "\n\n   Proceed? (y/N)"
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
sReplace = (fmap (TL.pack . concat)) . (mapM (\c -> if c /= 's' && c /= 'S' then pure [c] else ((:) c) . T.unpack <$> runFile [] "madsrc/slither.mad")) . TL.unpack

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
