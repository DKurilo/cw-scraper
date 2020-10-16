{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module WikiScrapeLib
    (  mostfrequentwordonpage
    ) where

import           Control.Exception               (IOException, catch)
import           Control.Monad                   ((>=>))
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as BC
import           Data.Char                       (isAlpha, isNumber,
                                                  isPunctuation, isSpace,
                                                  toLower)
import           Data.List                       (sortOn)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromJust, isJust, listToMaybe)
import qualified Data.Trie                       as T
import           Network.HTTP.Client             (HttpException)
import           Text.HTML.Scalpel
import           Text.ParserCombinators.ReadP    as TPR
import qualified Text.ParserCombinators.ReadPrec as TP
import           Text.Read                       (Read (..), readMaybe)

stopwords :: IO (T.Trie Bool)
stopwords = T.fromList  . map (, True) . BC.words <$> catch (BS.readFile "./stopwords.txt")
                                                            (return . const "" :: IOException -> IO BS.ByteString )

newtype GoodWord = W BS.ByteString

instance Read GoodWord where
    readPrec = TP.lift parser
        where parser :: TPR.ReadP GoodWord
              parser = ( do
                           TPR.munch1 isPunctuation
                           parser
                       )
                       <++ ( do
                           TPR.munch1 isSpace
                           parser
                       )
                       <++ ( do
                           word <- W . BC.pack . map toLower <$> TPR.munch1 isAlpha
                           afterWord
                           return word
                       )
              afterWord :: TPR.ReadP String
              afterWord = ( do
                             satisfy isPunctuation
                             TPR.munch (const True)
                          )
                          <++ TPR.munch (not . isNumber)

instance Show GoodWord where
    show (W w) = BC.unpack w

fromGoodWord :: GoodWord -> BS.ByteString
fromGoodWord (W w) = w

cleanUpWord :: Title -> T.Trie a -> GoodWord -> Maybe GoodWord
cleanUpWord title sw gw
  | startsWitTitle = Nothing
  | isStopWord = Nothing
  | BS.length w <= 1 = Nothing
  | otherwise = Just gw
  where w = fromGoodWord gw
        titl = BC.map toLower . BS.take 4 . BC.pack $ title
        startsWitTitle = titl `BS.isPrefixOf` w
        isStopWord = w `T.member` sw

type Title = String
type FullText = String
data Article = Article Title FullText deriving (Show)

getArticle :: URL -> IO (Maybe Article)
getArticle url =  scrapeURL url $
    do
        title <- text ("h1" @: ["id" @= "firstHeading"])
        body <- text ("div" @: ["id" @= "bodyContent"])
        return (Article title body)

getWords :: T.Trie a -> Article -> [BS.ByteString]
getWords sw (Article title body) = map (fromGoodWord . fromJust)
                                 . filter isJust
                                 . map (readMaybe >=> cleanUpWord title sw)
                                 . words
                                 $ body

type Histogram = M.Map BS.ByteString Int

buildHistogram :: [BS.ByteString] -> Histogram
buildHistogram = foldl (\h w -> M.insertWith (+) w 1 h) M.empty

getMostFrequent :: Histogram -> Maybe BS.ByteString
getMostFrequent = fmap fst . listToMaybe . sortOn (\(_, n) -> -n) . M.toAscList

mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage page = do
    mbArticle <- catch (getArticle page) (return . const Nothing :: HttpException -> IO (Maybe Article))
    sw <- stopwords
    return $ BC.unpack <$> (getMostFrequent . buildHistogram . getWords sw =<< mbArticle)
