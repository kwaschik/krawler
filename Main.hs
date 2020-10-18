{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when, foldM)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.ST (RealWorld, stToIO)
import Data.BloomFilter.Easy (suggestSizing)
import Data.BloomFilter.Hash (cheapHashes)
import qualified Data.BloomFilter.Mutable as MBF 
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import qualified Data.CaseInsensitive as CI
import Data.Default (def)
import qualified Data.List as L
import Data.Sequence hiding (lookup, filter)
import Data.Text (pack)
import Data.Time (getCurrentTime)
import Network.DNS.Cache
import Network.DNS.Resolver
import Network.HTTP.Conduit.Downloader
import Network.Mime
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import Text.HTML.TagSoup
import Text.Read (readMaybe)
import Text.Regex.TDFA

import RobotsTxtParser
import URIParser

import Prelude hiding (lookup, length)

type Crawler e s a = ReaderT e (StateT s IO) a
evalCrawler :: Crawler e s a -> e -> s -> IO a
evalCrawler c e s = evalStateT (runReaderT c e) s

data CrawlState = CrawlState {
        todo      :: Seq (URI, Maybe URI),
        curHost   :: Maybe String,
        curRules  :: Maybe [(String, [String])],
        count     :: Int,
        -- bloomfilter
        -- false positives mgl., d.h. URIs als besucht gekennzeichnet,
        -- die noch nie besucht worden sind
        done      :: MBF.MBloom RealWorld String
} deriving (Show)

data CrawlEnv = CrawlEnv {
        getDNSCache   :: DNSCache,
        getDownloader :: Downloader,
        getStartHost  :: String,
        getLogPath    :: String
}

-- TODOs 
-- Ausführung concurrent/parallel 
-- DNS-Server konfigurierbar
-- Log-Dir konfigurierbar
-- Bloomfilter konfigurierbar
-- Themen, nach denen gesucht wird, konfigurierbar (Methode konfigurierbar?)
-- generelle Methode: crawler ewig laufen lassen oder auf Suchmaschinen aufsetzen?
-- Linux-Tests, kompilieren als static binary

main :: IO ()
main = do 
        (startUrl:_) <- getArgs
        case parseToURI startUrl of
                Right startUri -> do
                        let firstHost  = maybe "" (\(_,h,_) -> h) $ getAuthority startUri
                        let cacheConf  = DNSCacheConf 
                                         { resolvConfs = [defaultResolvConf { resolvInfo = RCHostName "94.247.43.254"}]
                                         , maxConcurrency = 5
                                         , minTTL = 3600
                                         , maxTTL = 86400
                                         , negativeTTL = 120
                                         }
                        let dlSettings = def
                                         { dsUserAgent = "meaning of life crawler"
                                         , dsTimeout = 10
                                         -- offenbar pro Download?
                                         , dsMaxDownloadSize = 1 * 1024 * 1024
                                         } 
                        let uris = singleton (startUri, Nothing)
                        let (filterSize, hashFuns) = suggestSizing 100000 0.0001
                        bloomFilter <- stToIO $ MBF.new (cheapHashes hashFuns) filterSize

                        withDNSCache cacheConf $ \dnsCache -> 
                                withDownloaderSettings dlSettings $ \downloader -> do
                                        let env = CrawlEnv 
                                                  { getDNSCache = dnsCache
                                                  , getDownloader = downloader
                                                  , getStartHost = firstHost
                                                  , getLogPath = "." </> "crawl_result" 
                                                  } 
                                        let state = CrawlState
                                                    { todo     = uris
                                                    , curHost  = Nothing
                                                    , curRules = Nothing
                                                    , count    = 0
                                                    , done     = bloomFilter 
                                                    }
                                        hSetBuffering stdout LineBuffering
                                        createDirectoryIfMissing True $ getLogPath env
                                        getCurrentTime >>= putStrLn . (++ " -- start: " ++ startUrl) . show
                                        evalCrawler crawl env state
                                        getCurrentTime >>= putStrLn . (++ " -- finished") . show
                Left err -> do 
                        putStrLn $ "failed to parse uri '" ++ startUrl ++ "'"
                        putStrLn $ show err

pattern :: Regex
pattern = makeRegex ("^.*(([Ss]inn)|([Ww]ert)|([Zz]weck)|([Zz]iel)).*$"::ByteString)
-- "(Ll)eben" später im Resultat filtern ...?

processContent :: FilePath -> ByteString -> IO ()
processContent path content = do
        --BSC.appendFile (path<.>"2") content
        let fltcont = BSC.unlines $ getAllTextMatches $ match pattern content
        when (BSC.length fltcont > 0) $ BSC.appendFile path fltcont
        

crawl :: Crawler CrawlEnv CrawlState ()
crawl = let maybeResolve base = maybe base (resolveRef base)
        in do
                state  <- get
                -- next target
                case todo state of
                        Empty -> return ()
                        (baseUri, mbRef) :<| rest -> do
                                put $ state { todo = rest }

                                let targetUri = maybeResolve baseUri mbRef
                                let target    = uriToStrNoFrag targetUri 
                                let scheme    = getScheme targetUri
                                let host      = maybe "" (\(_,h,_) -> h) $ getAuthority targetUri

                                liftIO $ putStr $ "=> " ++ target ++ " , "
                                -- get bloom filter for already crawled sites
                                let crawled = done state
                                visited <- liftIO $ stToIO $ MBF.elem target crawled
                                when (not visited && isSchemeAcceptable scheme) $ do
                                        result <- performDownload target host $ getPath targetUri
                                        -- performDownload kann state ändern (modify?)
                                        state  <- get
                                        case result of
                                                Right (downloadInfo, content, nextUrls) -> do
                                                        liftIO $ putStr $ "[" ++ downloadInfo ++ "] "
                                                        -- log content
                                                        env <- ask
                                                        when (BSC.length content > 0) $ 
                                                                liftIO $ processContent ((getLogPath env) </> host <.> "txt") content
                                                        -- update bloomfilter
                                                        liftIO $ stToIO $ MBF.insert crawled target
                                                        -- stricter!!
                                                        -- bloomcheck bereits hier, nur noch Uris, die gem. Filter ok sind
                                                        new_todo <- liftIO $ foldM (f crawled targetUri) rest nextUrls
                                                        --let new_todo = foldl' (\sq u -> sq |> (targetUri, u)) rest nextUrls
                                                        put $ state { todo = new_todo, done = crawled }
                                                Left (Nothing, err) -> liftIO $ putStr $ "[error: " ++ err ++ "] "
                                                Left (Just dlResult, retry) -> do
                                                        liftIO $ putStr $ "[" ++ (show dlResult) ++ "] "
                                                        case readMaybe retry of
                                                                Just s -> do
                                                                        put $ state { todo = (baseUri, mbRef) <| rest }
                                                                        liftIO $ putStr $ "[pause " ++ retry ++ " seconds] "
                                                                        liftIO $ threadDelay $ s*1000000
                                                                _ -> liftIO $ putStr "[delay?] " 
                                liftIO $ putStr $ "[count: " ++ (show $ count state) ++ "] "
                                liftIO $ putStrLn $ "[todo: " ++ (show $ length $ todo state) ++ "]"
                                -- count siehe performDownload
                                when (count state < 1000) crawl

-- als lokale funktion in crawl integrieren
f flt bu sq u = do
        let tu = maybe bu (resolveRef bu) u
        let ts = uriToStrNoFrag tu
        known <- stToIO $ MBF.elem ts flt
        if known
        then return sq
        else return $ sq |> (bu, u)

performDownload :: String -> String -> [String]
                -> Crawler CrawlEnv CrawlState (Either (Maybe DownloadResult, String) (String, ByteString, [Maybe URI]))
performDownload target host path = do
        env   <- ask
        state <- get
        addr  <- liftIO $ lookup (getDNSCache env) $ BSC.pack host
        when ((host == getStartHost env) && (curHost state /= Just host)) $ 
                if (addr /= Nothing) 
                then do
                        -- robots.txt 
                        result <- liftIO $ download (getDownloader env) ("http://"++host++"/robots.txt") addr []
                        robotsTxt <- case result of
                                DROK dat _ -> return $ BSC.unpack dat
                                DRRedirect url -> do 
                                        result <- liftIO $ download (getDownloader env) url addr []
                                        case result of
                                                DROK dat _ -> return $ BSC.unpack dat
                                                _ -> return ""
                                _ -> return ""
                        let rules = parseRobotsTxt robotsTxt
                        put $ state { curHost = Just host, curRules = rules }
                else put $ state { curHost = Just host, curRules = Nothing }
        let matches = checkRules path (curRules state) 
        liftIO $ putStr $ "[matches in robots.txt rules: " ++ show matches ++ "] "
        if (addr /= Nothing) && (host == getStartHost env) && (not $ L.isInfixOf "?" target) && (matches <= Just 0) && (isPathAcceptable path)
        then do
                -- timeout??
                --result <- liftIO $ download (getDownloader env) target addr []
                (result, rawResult) <- liftIO $ rawDownload return (getDownloader env) target addr []
                when (resultOk result) $ modify' $ \state -> state { count = (count state) + 1 }

                let (tags, downloadInfo) = case result of
                        DROK dat _     -> (parseTags dat, Right "DROK")
                        DRRedirect url -> ([TagOpen "a" [("href", BSC.pack url)], TagClose "a"], Right $ show result)
                        -- status beachten (429, 503 usw.)
                        -- ggf. Retry-After beachten (delay-s or date)
                        _              -> ([], Left $ fmap (map snd . filter ((retryHeader==).fst) . rdrHeaders) rawResult)
                -- comments?
                let content = BSC.unwords . map fromTagText $ filter isTagText tags
                let hrefs   = filter (isTagOpenName "a") tags
                case downloadInfo of
                        Right info -> return $ Right $ (info, content, map (hrefToUri . fromAttrib "href") hrefs)
                        Left (Just (retry:_)) -> return $ Left $ (Just result, BSC.unpack retry)
                        _ -> return $ Left $ (Just result, "")
        else return $ Left (Nothing, "host not resolved, host not acceptable, uri not acceptable, uri not allowed")
       where 
        retryHeader :: CI.CI ByteString
        retryHeader         = "Retry-After"
        hrefToUri           = either (const Nothing) Just . parseToURI . BSC.unpack
        resultOk (DROK _ _) = True
        resultOk _          = False
        

checkRules :: [String] -> Maybe [(String, [String])] -> Maybe Int
checkRules pathElems mbRules = do
        let path = L.intercalate "/" pathElems
        rules <- mbRules
        let disallow1 = case L.lookup "Disallow:" rules of
                Just rs -> rs
                Nothing -> []
        let disallow2 = case L.lookup "disallow:" rules of
                Just rs -> rs
                Nothing -> []
        let disallow = disallow1++disallow2
        return $ L.length $ L.filter ((`L.isPrefixOf` path).(takeWhile (/='*'))) disallow
        

isPathAcceptable :: [String] -> Bool
isPathAcceptable []    = True
isPathAcceptable path  = "text/" `BSC.isPrefixOf` (mimeType path)
        where mimeType = mimeByExt defaultMimeMap "text/plain" . pack . map toLower . last

isSchemeAcceptable :: Maybe String -> Bool
isSchemeAcceptable (Just s) | s == "https" = True
                            | s == "http"  = True
isSchemeAcceptable _                       = False
