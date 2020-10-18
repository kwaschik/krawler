module RobotsTxtParser (parseRobotsTxt) where

import Data.List (groupBy, sortBy, splitAt)
import Text.ParserCombinators.Parsec

parseRobotsTxt :: String -> Maybe [(String, [String])]
parseRobotsTxt str = let parsed  = parse robotsTxt "<input>" str
                         content = either (const []) id parsed
                     in fmap grpByRule $ lookup "*" content

robotsTxt :: GenParser Char st [(String, [(String, String)])]
robotsTxt = (optional (char '\65279')) >> (block `manyTill` eof)

block = do
        commentsOrSpaces
        a  <- agent
        rs <- many (try rule)
        commentsOrSpaces
        return (a, rs)

-- TODO einer oder mehrere pro Gruppe
-- momentan geht nur einer
agent = do
        choice [ s' "User-Agent:"
               , s' "User-agent:"
               , s' "user-Agent:"
               , s' "user-agent:"
               ]
        skipMany (char ' ')
        anyChar `manyTill` eol

rule = do
        commentsOrSpaces
        t <- choice [ s' "Disallow:"
                    , s' "disallow:"
                    , s' "Allow:"
                    , s' "allow:"
                    , s' "Crawl-Delay:"
                    , s' "Crawl-delay:"
                    , s' "crawl-Delay:"
                    , s' "crawl-delay:"
                    , s' "Sitemap:"
                    , s' "sitemap:"
                    ]
        skipMany (char ' ')
        r <- anyChar `manyTill` (eol <|> eof) 
        return (t, r)

comment = do
        char '#'
        anyChar `manyTill` (eol <|> eof)
        return ()

commentsOrSpaces = skipMany (comment <|> (skipMany1 space))
s'  = try . string
eol = (wineol <|> newline) >> return ()
        where wineol = char '\r' >> newline

                             
-- robots.txt rules: fst is rule type 
grpByRule :: [(String, String)] -> [(String, [String])]
grpByRule = map (merge . (splitAt 1)) . groupBy eqTpl . sortBy cmpTpl
        where cmpTpl t1 t2 = fst t1 `compare` fst t2
              eqTpl t1 t2  = fst t1 == fst t2
              merge (((t, r):_), rest) = (t, r:(map snd rest))
