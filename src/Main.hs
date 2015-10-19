{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Prim
import System.Environment
import System.Exit
import System.IO

-- Main:

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go ("-h"    :_) = help
go ("--help":_) = help
go []           = getContents >>= goT "STDIN"
go fs           = mapM_ goF fs

goF :: String -> IO ()
goF f = readFile f >>= goT ("File: " ++ f)

goT :: String -> String -> IO ()
goT f t = case parse (items <* eof) f t
         of Left  err -> hPrint stderr err >> exitFailure
            Right res -> putStr $ unlines $ pps res

help :: IO ()
help = putStrLn "Usage: [STDIN |] prettybraces [-h | --help] [FILE]*"

-- Data:

type Open     = String
type Close    = String
type Brackets = (Open, Close)
data Group    = Group Brackets Items deriving Show
type Item     = Either String Group
type Items    = [Item]

-- Constants:

brackets :: [(String, String)]
brackets = [("(",")"),("{","}"),("[","]"),("<!--","-->")]

bStrings :: [String]
bStrings = brackets >>= \(l,r) -> [l,r]

-- Parser:

items :: Parsec String Items
items = many item

item :: Parsec String Item
item = choice (map (fmap Right . mkParser) brackets) <|> fmap Left nonBracket

mkParser :: (String, String) -> Parsec String Group
mkParser p@(l,r) = do
  is <- between (string l) (string r) items
  return $ Group p is

nonBracket :: Parsec String String
nonBracket = untilP $ choice $ map string bStrings

untilP :: MonadParsec s f Char => f a -> f String
untilP end = some $ notFollowedBy end *> anyChar

-- Printer:

pp' :: Int -> Either String Group -> [String]
pp' n (Left  s)                = [blank n s]
pp' n (Right (Group (l,r) is)) = [blank n l]
                              ++ pps' (succ n) is
                              ++ [blank n r]

pps' :: Int -> Items -> [String]
pps' n = concatMap (pp' n)

pps :: Items -> [String]
pps = pps' 0

blank :: Int -> String -> String
blank n t = concat (replicate n "  ") ++ cleanup t

cleanup :: String -> String
cleanup = dropWhile (`elem` "\t ") . filter (/= '\n')
