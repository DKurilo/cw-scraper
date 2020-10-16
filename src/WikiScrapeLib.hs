{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module WikiScrapeLib
    (  mostfrequentwordonpage
    ) where

import           Control.Exception               (IOException, catch)
import           Control.Monad                   ((>=>), join)
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
import           Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as TP
import           Text.Read                       (Read (..), readMaybe)

stopwords :: IO (T.Trie Bool)
stopwords = T.fromList  . map (, True) . BC.words <$> catch (BS.readFile "./stopwords.txt")
                                                            (return . const "" :: IOException -> IO BS.ByteString )

newtype GoodWord = W BS.ByteString deriving (Eq, Ord)

instance Semigroup GoodWord where
    (W w1) <> (W w2) = W $ w1 <> w2

instance Read GoodWord where
    readPrec = TP.lift parser
        where parser :: ReadP GoodWord
              parser = ( do
                           munch1 isPunctuation
                           parser
                       )
                       <++ ( do
                           munch1 isSpace
                           parser
                       )
                       <++ ( do
                           c <- W . BC.singleton . toLower <$> satisfy isAlpha
                           word <- W . BC.pack . map toLower <$> munch1 isAlpha
                           afterWord
                           return $ c <> word
                       )
              afterWord :: ReadP String
              afterWord = ( do
                             satisfy isPunctuation
                             munch (const True)
                          )
                          <++ munch (not . isNumber)

instance Show GoodWord where
    show (W w) = BC.unpack w

cleanUpWord :: Title -> T.Trie a -> GoodWord -> Maybe GoodWord
cleanUpWord title sw gw@(W w)
  | startsWitTitle = Nothing
  | isStopWord = Nothing
  | otherwise = Just gw
  where titl = BC.map toLower . BS.take 4 . BC.pack $ title
        startsWitTitle = titl `BS.isPrefixOf` w
        isStopWord = w `T.member` sw

type Title = String
type FullText = String
data Article = Article Title FullText

getArticle :: URL -> IO (Maybe Article)
getArticle url =  scrapeURL url $
    do
        title <- text ("h1" @: ["id" @= "firstHeading"])
        body <- text ("div" @: ["id" @= "bodyContent"])
        return (Article title body)

getWords :: T.Trie a -> Article -> [GoodWord]
getWords sw (Article title body) = map fromJust
                                 . filter isJust
                                 . map (readMaybe >=> cleanUpWord title sw)
                                 . words
                                 $ body

type Histogram = M.Map GoodWord Int

buildHistogram :: [GoodWord] -> Histogram
buildHistogram = foldl (\h gw -> M.insertWith (+) gw 1 h) M.empty

getSortedWords :: Histogram -> [(GoodWord, Int)]
getSortedWords = sortOn (\(_, n) -> -n) . M.toAscList

tryGetFirst :: [(a, b)] -> Maybe a
tryGetFirst = fmap fst . listToMaybe

buildTagCloud :: Maybe [(GoodWord, Int)] -> IO ()
buildTagCloud Nothing = return ()
buildTagCloud _ = return ()

mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage page = do
    mbArticle <- catch (getArticle page) (return . const Nothing :: HttpException -> IO (Maybe Article))
    sw <- stopwords
    let sortedWords = getSortedWords . buildHistogram . getWords sw <$> mbArticle
    buildTagCloud sortedWords
    return $ show <$> join (tryGetFirst <$> sortedWords)
