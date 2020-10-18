{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module WikiScrapeLib
    (  mostfrequentwordonpage
    ) where

import           Control.Exception               (IOException, catch)
import           Control.Lens
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
import qualified Graphics.GD                     as G
import           Network.HTTP.Client             (HttpException)
import           System.Random                   (randomRIO)
import           Text.HTML.Scalpel
import           Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as TP
import           Text.Read                       (Read (..), readMaybe)

-- some settings
font = "./fonts/Underdog-Regular.ttf"
borders = 40
paddings = 10
wordsInCloud = 50

-- stop words initialization
stopwords :: IO (T.Trie Bool)
stopwords = T.fromList  . map (, True) . BC.words <$> catch (BS.readFile "./stopwords.txt")
                                                            (return . const "" :: IOException -> IO BS.ByteString )

-- most frequent word
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
        startsWitTitle = titl `BS.isPrefixOf` w || w `BS.isPrefixOf` titl
        isStopWord = w `T.member` sw

type Title = String
type Body = String
data Article = Article { aTitle :: Title
                       , aBody  :: Body
                       }

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

-- Word cloud building
data DrawnWord = DW { _dwWord     :: GoodWord
                    , _dwFontSize :: Double
                    , _dwColor    :: G.Color
                    , _dwSize     :: G.Size
                    , _dwCenter   :: G.Point
                    , _dwOrigin   :: G.Point
                    }
makeLenses ''DrawnWord

stringSize :: DrawnWord -> IO DrawnWord
stringSize w = do
    (p1, p2, p3, p4) <- G.measureString font (w ^. dwFontSize) 0 (0, 0) (w ^. dwWord . to show) (w ^. dwColor)
    let xs = map fst [p1, p2, p3, p4]
        ys = map snd [p1, p2, p3, p4]
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
    return $ w & dwSize .~ (maxX - minX, maxY - minY)

initCenter :: DrawnWord -> IO DrawnWord
initCenter w = do
    x <- randomRIO (-300, 300)
    return $ w & dwCenter .~ (x, 0)

setCenters :: [DrawnWord] -> [DrawnWord]
setCenters ws = go ws []
    where go [] ws'' = ws''
          go (w:ws') ws''
            | null newWs = go ws' ws''
            | otherwise = go ws' (head newWs : ws'')
            where x0 = w ^. dwCenter . _1 . to fromIntegral
                  y0 = w ^. dwCenter . _2 . to fromIntegral
                  r = sqrt (x0 * x0 + y0 * y0)
                  steps = 360
                  getXY (r', z) = (round (r' * cos angle), round (- r' * sin angle * 2 / 3))
                    where angle = z * 2 * pi / steps
                  getBox w' = ((minX, minY), (maxX, maxY))
                    where minX = (w' ^. dwCenter . _1) - ((w' ^. dwSize . _1) `div` 2)
                          maxX = (w' ^. dwCenter . _1) + ((w' ^. dwSize . _1) `div` 2)
                          minY = (w' ^. dwCenter . _2) - ((w' ^. dwSize . _2) `div` 2)
                          maxY = (w' ^. dwCenter . _2) + ((w' ^. dwSize . _2) `div` 2)
                  are1DOvelapped (b1minX, b1maxX) (b2minX, b2maxX) = b1maxX >= b2minX - paddings && b2maxX >= b1minX - paddings
                  are2DOverlappeded w1 w2 = are1DOvelapped (w1minX, w1maxX) (w2minX, w2maxX)
                                          && are1DOvelapped (w1minY, w1maxY) (w2minY, w2maxY)
                      where ((w1minX, w1minY), (w1maxX, w1maxY)) = getBox w1
                            ((w2minX, w2minY), (w2maxX, w2maxY)) = getBox w2
                  isOverlappedWithAny w' = any (are2DOverlappeded w') ws''
                  newWs = filter (not . isOverlappedWithAny)
                          . map (\polar -> w & dwCenter .~ getXY polar)
                          $ [(r', angle) | r' <- [r, r + 8..], angle <- [0..steps]]

calculateOrigins :: [DrawnWord] -> ([DrawnWord], G.Size)
calculateOrigins ws = (map setOrigin ws, (maxX + dx + borders, maxY + dy + borders))
    where minX = minimum . map (\w -> (w ^. dwCenter . _1) - ((w ^. dwSize . _1) `div` 2)) $ ws
          maxX = maximum . map (\w -> (w ^. dwCenter . _1) + ((w ^. dwSize . _1) `div` 2)) $ ws
          minY = minimum . map (\w -> (w ^. dwCenter . _2) - ((w ^. dwSize . _2) `div` 2)) $ ws
          maxY = maximum . map (\w -> (w ^. dwCenter . _2) + ((w ^. dwSize . _2) `div` 2)) $ ws
          dx = borders - minX
          dy = borders - minY
          setOrigin w = w & dwOrigin .~ (x, y)
              where x = dx + (w ^. dwCenter . _1) - ((w ^. dwSize . _1) `div` 2)
                    y = dy + (w ^. dwCenter . _2) + ((w ^. dwSize . _2) `div` 2)

drawWord :: G.Image -> DrawnWord -> IO (G.Point, G.Point, G.Point, G.Point)
drawWord im w = G.drawString font (w ^. dwFontSize) 0 (w ^. dwOrigin) (w ^. dwWord . to show) (w ^. dwColor) im

htmlColor :: String -> G.Color
htmlColor [r1, r2, g1, g2, b1, b2] = G.rgb (getInt r1 r2) (getInt g1 g2) (getInt b1 b2)
    where getInt c1 c2 = read ("0x" <> [c1, c2])
htmlColor _ = G.rgb 0 0 0

buildWordCloud :: String -> Maybe [(GoodWord, Int)] -> IO ()
buildWordCloud _ Nothing = return ()
buildWordCloud fn (Just h) = do
    let firstWords = take wordsInCloud h
        minSize = 40
        maxSize = 100
        minN = (fromIntegral . snd . last) firstWords
        maxN = (fromIntegral . snd . head) firstWords
        getPt n
          | minN == maxN = maxSize
          | otherwise = ((maxSize - minSize) * n' + minSize * maxN - maxSize * minN) / (maxN - minN)
          where n' = fromIntegral n
        color pt
          | pt >= 88 = htmlColor "201b1b"
          | pt >= 76 = htmlColor "380606"
          | pt >= 64 = htmlColor "5a0b0b"
          | pt >= 52 = htmlColor "7a0e0e"
          | otherwise = htmlColor "d3af8e"
    words <- mapM ((initCenter >=> stringSize) . (\(gw, n) -> let pt = getPt n in  DW gw pt (color pt) (0, 0) (0, 0) (0, 0))) firstWords
    let (words', isize) = (calculateOrigins . setCenters) words
    image <- G.newImage isize
    G.fillImage (G.rgb 255 255 255) image
    mapM_ (drawWord image) words'
    G.savePngFile ("./generated-images/" <> fn <> ".png") image

-- main doer
mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage page = do
    mbArticle <- catch (getArticle page) (return . const Nothing :: HttpException -> IO (Maybe Article))
    sw <- stopwords
    let sortedWords = getSortedWords . buildHistogram . getWords sw <$> mbArticle
    let wordCloudFileName = maybe "unknown" aTitle mbArticle
    buildWordCloud wordCloudFileName sortedWords
    return $ show <$> (tryGetFirst =<< sortedWords)
