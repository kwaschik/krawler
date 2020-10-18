module URIParser 
        ( URI(..)
        , parseToURI
        , resolveRef
        , uriToStr
        , uriToStrNoFrag
        ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Data.List (intercalate)

data URI = URI 
        { getScheme    :: Maybe String
        , getAuthority :: Maybe (Maybe String, String, Maybe String) 
        , getPath      :: [String]
        , getQuery     :: Maybe String
        , getFragment  :: Maybe String
        } deriving (Show)

pctEncoded   = c <$> char '%' <*> hexDigit <*> hexDigit
        where c x y z = x:y:z:""

reserved     = genDelims <|> subDelims
genDelims    = oneOf ":/?#[]@"
subDelims    = oneOf "!$&'()*+,;="
unreserved   = letter <|> digit <|> oneOf "-._~"

uri          = do s <- scheme
                  char ':'
                  (a, p) <- hierPart
                  q <- option Nothing (char '?' *> query)
                  f <- option Nothing (char '#' *> fragment) 
                  return $ URI s a p q f

scheme       = let c x xs = Just (x:xs) in
               c <$> letter <*> many (alphaNum <|> oneOf "+-.")

hierPart     = option (Nothing, []) $ 
                try pathWithAuthD <|> try pathAbsoluteD <|> pathRootlessD

authority    = do u <- option Nothing (try $ userinfo <* char '@')
                  h <- host
                  p <- option Nothing (char ':' >> port)
                  return $ Just (u, h, p)
        
userinfo     = Just . concat <$> 
                many (many1 unreserved <|> pctEncoded <|> many1 subDelims <|> many1 (char ':'))
-- wir nehmen nur reg-names
host         = concat <$> many (many1 unreserved <|> pctEncoded <|> many1 subDelims)
port         = Just <$> many digit


pathWithAuth = (,) <$> (string "//" *> authority) <*> pathAbEmpty
pathAbEmpty  = many (char '/' *> segment)
pathWithAuthD = do
        (a, p) <- pathWithAuth
        return (a, if length p == 0 then p else "":p)

pathAbsolute = char '/' *> (option [] pathRootless)
pathAbsoluteD = do
        p <- pathAbsolute
        return (Nothing, if length p == 0 then ["",""] else "":p)

pathNoScheme = do s <- many1 (
                        many1 unreserved <|> pctEncoded <|> many1 subDelims <|> many1 (char '@'))
                  ss <- many (char '/' >> segment)
                  return $ (concat s:ss)
pathNoSchemeD = do
        p <- pathNoScheme
        return (Nothing, p)

pathRootless = (:) <$> segmentNz <*> many (char '/' *> segment)
pathRootlessD = do
        p <- pathRootless
        return (Nothing, p)

segment      = concat <$> many pchar
segmentNz    = concat <$> many1 pchar
pchar        = many1 unreserved <|> pctEncoded <|> many1 subDelims <|> many1 (oneOf ":@")

query        = Just . concat <$> many (pchar <|> many1 (oneOf "/?"))
fragment     = Just . concat <$> many (pchar <|> many1 (oneOf "/?"))

--refOrUri     = try (relativeRef <* eof) <|> (uri <* eof)
uriRef       = try (uri <* eof) <|> (relativeRef <* eof)
relativeRef  = do (a, p) <- option (Nothing, []) $ 
                        try pathWithAuthD <|> try pathAbsoluteD <|> pathNoSchemeD
                  q <- option Nothing (char '?' *> query)
                  f <- option Nothing (char '#' *> fragment) 
                  return $ URI Nothing a p q f

-- remove dot segments
resolveRef :: URI -> URI -> URI
resolveRef base ref = URI scheme authority path query fragment
        where scheme = case (getScheme ref) of
                        Nothing -> getScheme base
                        s -> s
              defQuery Nothing q       = q
              defQuery q       _       = q
              merge [] p               = ("":p)
              merge b  p               = (init b)++p
              (authority, path, query) = authority_path_query
              authority_path_query | (getAuthority ref) /= Nothing 
                                       = (getAuthority ref, getPath ref, getQuery ref)
                                   | otherwise
                                       = case (getPath ref) of
                                                []      ->
                                                        (getAuthority base, getPath base, defQuery (getQuery ref) (getQuery base))
                                                ("":_)  ->
                                                        (getAuthority base, getPath ref, getQuery ref)
                                                p       ->
                                                        (getAuthority base, merge (getPath base) p, getQuery ref)
              fragment = getFragment ref

uriToStr' :: (Maybe String -> String) -> URI -> String
uriToStr' fragToStr u = 
             let schemeToStr = maybe "" (++":") 
                 userInfoToStr  = maybe "" (++"@") 
                 portToStr      = maybe "" (":"++)
                 authPartsToStr (u, h, p) 
                                = (userInfoToStr u) ++ h ++ (portToStr p)
                 authorityToStr = maybe "" (("//"++).authPartsToStr)
                 pathToStr      = intercalate "/" 
                 queryToStr     = maybe "" ("?"++)
                 fragmentToStr  = fragToStr --maybe "" ("#"++)
              in (schemeToStr $ getScheme u) ++
                 (authorityToStr $ getAuthority u) ++
                 (pathToStr $ getPath u) ++
                 (queryToStr $ getQuery u) ++
                 (fragmentToStr $ getFragment u) 

uriToStr :: URI -> String
uriToStr = uriToStr' (maybe "" ("#"++))
uriToStrNoFrag :: URI -> String
uriToStrNoFrag = uriToStr' (maybe "" (const ""))

parseToURI = parse uriRef "<input>"
