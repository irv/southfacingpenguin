{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.List (isSuffixOf, intercalate, sortBy, group)
import Data.List.Split
import Data.Time
import System.FilePath
import Control.Monad (forM, forM_, liftM)
import Data.Monoid ((<>))
import Control.Arrow ((&&&))
import Data.Maybe
import qualified Data.Map as M
import Network.HTTP.Base (urlEncode)
import Data.Char(toLower)
import qualified Data.Text as T
import Summarizer (wordFreq, sentences, pickSentences, popularWords)
import Summarizer.StopWords (englishStopWords)

hostname :: String
hostname = "https://southfacingpenguin.review"

compressorCompiler :: String -> Item String -> Compiler (Item String)
compressorCompiler t = withItemBody(unixFilter "yui-compressor" ["--type", t])

defaultTeaser :: String
defaultTeaser = "My name is Jane Wallace, and the idea behind this website is really simple: read a book and write about it."

main :: IO ()
main = do
  time <- getCurrentTime
  hakyll $ do
    reviewMatches <- getMatches "reviews/*"
    let reviewIds = fromList $ filterReviews (utctDay time) reviewMatches
    -- ghetto way of only publishing when it's time

    archives <- buildArchive reviewIds
    tags <- buildTagsWith (liftM (map slugify) . getTags) reviewIds (fromCapture "tags/*/index.html")
    paginate <- buildPaginateWith grouper reviewIds makeId

    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "robots.txt" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*.css" $ do
      route   idRoute
      compile compressCssCompiler

    match "css/*.scss" $ do
      route $ setExtension "css"
      compile $ getResourceString
        >>= withItemBody(unixFilter "sass" ["-s", "--scss"])
        >>= compressorCompiler "css"

    match (fromGlob "*.md") $ do
      route $ customRoute (\i -> takeBaseName (toFilePath i) </> "index.html")
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" (constField "teaser" defaultTeaser <> field "archives" (\_ -> renderArchives archives) <> reviewCtx)
        >>= cleanIndexUrls


    match reviewIds $ do
      route $ customRoute (slugify . takeBaseName . toFilePath) `composeRoutes` niceRoute "reviews/" `composeRoutes` dateFolders
      --  niceRoute`composeRoutes` dateFolders
      compile $ do
          body <- getResourceBody
          let b = itemBody body
          let revCtx = field "teaser" (\_ -> return $ T.unpack $ makeSummary $ T.pack b) <> tagsCtx tags
          pandocCompiler
            >>= loadAndApplyTemplate "templates/review.html" (tagsCtx tags)
            >>= saveSnapshot "fullreview"
            >>= loadAndApplyTemplate "templates/default.html" (field "archives" (\_ -> renderArchives archives) <> revCtx)
            >>= cleanIndexUrls

    tagsRules tags $ \tag pattern -> do
      let title = "Reviews tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        reviews <- recentFirst =<< loadAllSnapshots pattern "fullreview"
        let ctx = constField "title" title
                  <> listField "reviews" reviewCtx (return reviews)
                  <> field "archives" (\_ -> renderArchives archives) <> constField "teaser" defaultTeaser
                  <> constField "hostname" hostname
                  <> defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/review-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= cleanIndexUrls

    forM_ archives $ \(d,_) ->
      create [mkArchivePath d] $ do
        route idRoute
        compile $ do
          reviews <- recentFirst =<< filterArchive d =<< loadAllSnapshots (fromGlob "reviews/*") "fullreview"
          let ctx = listField "reviews" reviewCtx (return reviews)
                      <> field "archives" (\_ -> renderArchives archives)
                      <> constField "title" ("Reviews in " ++ show d)
                      <> constField "teaser" defaultTeaser <> constField "hostname" hostname

                      <> defaultContext
          makeItem ""
            >>= loadAndApplyTemplate "templates/review-list.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= cleanIndexUrls

    paginateRules paginate $ \page pattern -> do
      route idRoute
      compile $ do
        reviews <- recentFirst =<< loadAllSnapshots pattern "fullreview"
        let ctx = constField "title" ("Review Archive - Page " ++ show page) <>
                  listField "reviews" reviewCtx (return reviews) <>
                  field "archives" (\_ -> renderArchives archives) <>
                  constField "teaser" defaultTeaser <> constField "hostname" hostname <>
                  paginateContextPlus paginate page <>
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/review-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler

filterReviews :: Day -> [Identifier] -> [Identifier]
filterReviews d = filter (\a -> d >= fromMaybe d (parseDay' a))

niceRoute :: String -> Routes
niceRoute prefix = customRoute $ \ident -> prefix ++ (takeBaseName . toFilePath $ ident) ++ "/index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndex :: String -> String
cleanIndex url
  | idx `isSuffixOf` url = take (length url - length idx) url
  | otherwise            = url
    where idx = "index.html"

mkArchivePath :: Day -> Identifier
mkArchivePath d = mk $ toGregorian d
  where
    mk (y, m, _) = fromFilePath $ "archive/" ++ show y ++ "/" ++ show m ++ "/index.html"

grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper = liftM (paginateEvery 2) . sortRecentFirst

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ mkId pageNum
  where mkId 1 = "index.html"
        mkId i = "reviews/page/" ++ show i ++ "/index.html"

filterArchive :: MonadMetadata m => Day -> [Item a] -> m [Item a]
filterArchive day a = return $ filter (\x -> parseDay (itemIdentifier x) == Just day) a

buildArchive :: MonadMetadata m => Pattern -> m [(Day, Int)]
buildArchive pattern = liftM (createList . map parseDay) (getMatches pattern)
  where
    createList :: [Maybe Day] -> [(Day, Int)]
    createList l = map (head &&& length) $ group $ catMaybes l

parseDay :: Identifier -> Maybe Day
parseDay s = uncurry fromGregorianValid (p (ex s)) 1
  where
    ex s' = splitOn "-" $ takeFileName $ toFilePath s'
    p (x:y:_) = (read x :: Integer, read y :: Int)
    p [] = (1974, 1)

parseDay' :: Identifier -> Maybe Day
parseDay' s  = b $ (p (ex s))
  where
    ex :: Identifier -> [String]
    ex s' = splitOn "-" $ takeFileName $ toFilePath s'
    p :: [String] -> (Integer, Int, Int)
    p (x:y:z:_) = (read x :: Integer, read y :: Int, read z :: Int)
    p [] = (1974, 1, 1)
    b (x',y',z') = fromGregorianValid x' y' z'


renderArchives :: [(Day, Int)] -> Compiler String
renderArchives a = do
  dates <- forM(sortBy (flip compare) a) $ \(day, count) -> do
    route' <- getRoute $ mkArchivePath day
    return (day, route', count)
  return . unwords $ map makeLink dates
  where
    makeLink (day, Just r, count) = renderHtml $
      H.li $ do
        H.i ! A.class_ "fa fa-archive fa-fw" $ ""
        H.a ! A.href (toValue . toUrl $ r ) $ toHtml $ formatTime defaultTimeLocale "%B %0Y" day ++" (" ++ show count ++ ")"
    makeLink (_, Nothing, _) = ""

dateFolders :: Routes
dateFolders = gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")

makeSummary :: T.Text -> T.Text
makeSummary = go 1
  where go n t = let popWords = popularWords 100 (wordFreq englishStopWords t)
                 in foldMap id $ pickSentences n popWords (sentences t)

reviewCtx :: Context String
reviewCtx =  dateField "date" "%B %e, %Y"
             <> constField "hostname" hostname
             <> field "safetitle" (\_ -> do
               identifier <- getUnderlying
               title <- getMetadataField identifier "title"
               case title of
                 Nothing -> return ""
                 Just t -> return $ urlEncode t
                )
                <> defaultContext

paginateContextPlus :: Paginate -> PageNumber -> Context a
paginateContextPlus pag currentPage = paginateContext pag currentPage <> mconcat
    [ listField "pagesBefore" linkCtx $ wrapPages pagesBefore
    , listField "pagesAfter"  linkCtx $ wrapPages pagesAfter
    ]
    where
        linkCtx = field "pageNum" (return . fst . itemBody) <>
                  field "pageUrl" (return . snd . itemBody)
        lastPage = M.size . paginateMap $ pag
        pageInfo n = (n, paginateMakeId pag n)

        pages = [pageInfo n | n <- [1..lastPage], n /= currentPage]
        (pagesBefore, pagesAfter) = span ((< currentPage) . fst) pages

        wrapPages = mapM makeInfoItem

        makeInfoItem (n, i) = getRoute i >>= \mbR -> case mbR of
            Just r  -> makeItem (show n, toUrl r)
            Nothing -> fail $ "No URL for page: " ++ show n

tagsCtx :: Tags -> Context String
tagsCtx tags = listFieldWith "tags" tagCtx (getTagItems tags) <>
          reviewCtx

getTagItems :: Tags -> Item a -> Compiler [Item (String, FilePath)]
getTagItems tags postItem = do
        tags' <- getTags (itemIdentifier postItem)
        links <- forM tags' $ \tag -> do
          route' <- getRoute $ tagsMakeId tags $ slugify tag
          return (tag, d route')
        mapM makeItem links
          where
            d Nothing = ""
            d (Just x) = x

tagCtx :: Context (String, String)
tagCtx = mconcat
  [field "tag-name" $ \tagItem -> return (fst $ itemBody tagItem)
  ,field "tag-url" $ \tagItem -> return(snd $ itemBody tagItem)]

slugify :: String -> String
slugify = intercalate "-" . words . map (\x -> if x `elem` allowedChars then toLower x else ' ')
  where allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
