module Tweet.Snek
    ( exec'
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Text.Madlibs
import System.Random.MWC
import Web.Tweet

snekTweet :: TL.Text -> FilePath -> IO ()
snekTweet txt = thread (TL.unpack txt) mempty Nothing tweets
    where tweets = fromIntegral $ (TL.length txt) `div` 140 + 1

exec' :: IO ()
exec' = snekspeare >>= TLIO.putStrLn

snekspeare :: IO TL.Text
snekspeare = (sReplace <=< pickLine <=< (pure . sepLines)) =<< readSnekspeare

sReplace :: TL.Text -> IO TL.Text
sReplace = (fmap (TL.pack . concat)) . (mapM (\c -> if c /= 's' then pure [c] else T.unpack <$> runFile "madsrc/slither.mad")) . TL.unpack

pickLine :: [TL.Text] -> IO TL.Text
pickLine lines = do
    i <- randNumRange (0, length lines)
    pure $ TL.dropWhile (==' ') $ TL.filter (\a -> and $ map (a/=) "[]\r\n") $ lines !! i

sepLines :: TL.Text -> [TL.Text]
sepLines = (fmap replacements) . (filter (/="")) . (TL.split splitWhen)
    where splitWhen    = \a -> or $ map (a==) ".?!" 
          replacements = foldr ((.) . (flip TL.replace " ")) id (map (flip TL.replicate " ") [2..4])

readSnekspeare :: IO TL.Text
readSnekspeare = TLIO.readFile "text/complete-works.txt"

-- | Generate random numbers from a given range.
randNumRange :: (Int, Int) -> IO Int
randNumRange ran = do
    let ran' = over both (fromIntegral) ran
    vs <- (withSystemRandom . asGenST $ \gen -> uniformR ran' gen) :: IO Double
    pure $ floor vs
