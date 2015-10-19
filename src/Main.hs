{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Megaparsec
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
         of Left  err -> hPutStrLn stderr (show err) >> exitFailure
            Right res -> putStrLn $ unlines $ pps res

help :: IO ()
help = putStrLn "Usage: [STDIN |] prettybraces [FILE]*"

-- Lib:

--               Open    Close
type Brackets = (String, String)

data Group = Group Brackets Items deriving Show

type Item = Either String Group

type Items = [Item]

brackets :: [(String, String)]
brackets = [("(",")"),("{","}"),("[","]")]

bchars :: String
bchars = concat $ brackets >>= (\(l,r) -> [l,r]) -- TODO: Allow string neg lookahead, eg, for html

items :: Parsec String Items
items = many item

item :: Parsec String Item
item = choice (map (fmap Right . mkParser) brackets) <|> fmap Left nonBracket

mkParser :: (String, String) -> Parsec String Group
mkParser p@(l,r) = do
  is <- between (string l) (string r) items
  return $ Group p is

nonBracket :: Parsec String String
nonBracket = some $ noneOf bchars

pp' :: Int -> Either String Group -> [String]
pp' n (Left  s)                = [blank n s]
pp' n (Right (Group (l,r) is)) = [blank n l]
                              ++ pps' (succ n) is
                              ++ [blank n r]

pps' :: Int -> Items -> [String]
pps' n is = concat $ map (pp' n) is

pps :: Items -> [String]
pps = pps' 0

blank :: Int -> String -> String
blank n t = concat (replicate n "  ") ++ t
