{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List.Split
import Text.Megaparsec
import Text.Megaparsec.Prim
import Control.Monad
import System.Exit
import System.IO

import Options.Applicative hiding (some)
import qualified Options.Applicative as A

-- Main:

main :: IO ()
main = join $ execParser optsInfo

-- Options:

optsInfo :: ParserInfo (IO ())
optsInfo = info (helper <*> opts)
      ( fullDesc
     <> header   "Pretty-Braces parses, and then pretty-prints strings with balanced braces."
     <> progDesc "Also accepts input on STDIN." )

opts :: Parser (IO ())
opts = go <$> bracesArg <*> filesArg

bracesArg :: Parser [Braces]
bracesArg = A.option (bracesArgParser <$> str)
   ( long    "braces"
  <> short   'b'
  <> help    "Space-separated, comma-separated pairs of braces, e.g. '(,) {,} <,>'."
  <> value   [("(",")"),("{","}"),("[","]")])

bracesArgParser :: String -> [Braces]
bracesArgParser = concatMap (pair . splitOn ",") . words
  where
  pair [l,r] = [(l,r)]
  pair _     = []

filesArg :: Parser [String]
filesArg = A.many (argument str (metavar "FILES..."))

-- Driver:

go :: [Braces] -> [String] -> IO ()
go bs [] = getContents >>= goT bs "STDIN"
go bs fs = mapM_ (goF bs) fs

goF :: [Braces] -> String -> IO ()
goF bs f = readFile f >>= goT bs ("File: " ++ f)

goT :: [Braces] -> String -> String -> IO ()
goT bs f t = case parse (items bs <* eof) f t
        of Left  err -> hPrint stderr err >> exitFailure
           Right res -> putStr $ unlines $ pps res

printHelp :: IO ()
printHelp = putStrLn "Usage: [STDIN |] prettybraces [-h | --printHelp] [FILE]*"

-- Data:

type Open   = String
type Close  = String
type Braces = (Open, Close)
data Group  = Group Braces [Item] deriving Show
type Item   = Either String Group

-- Parser:

items :: [Braces] -> Parsec String [Item]
items bs = many (item bs)

item :: [Braces] -> Parsec String Item
item bs = choice (map (fmap Right . mkParser bs) bs) <|> fmap Left (nonBracket bs)

mkParser :: [Braces] -> (String, String) -> Parsec String Group
mkParser bs p@(l,r) = do
  is <- between (string l) (string r) (items bs)
  return $ Group p is

nonBracket :: [Braces] -> Parsec String String
nonBracket bs = untilP $ choice $ map string bStrings
  where
  bStrings = bs >>= \(l,r) -> [l,r]


untilP :: MonadParsec s f Char => f a -> f String
untilP end = some $ notFollowedBy end *> anyChar

-- Printer:

pp' :: Int -> Either String Group -> [String]
pp' n (Left  s)                = [blank n s]
pp' n (Right (Group (l,r) is)) = [blank n l]
                              ++ pps' (succ n) is
                              ++ [blank n r]

pps' :: Int -> [Item] -> [String]
pps' n = concatMap (pp' n)

pps :: [Item] -> [String]
pps = pps' 0

blank :: Int -> String -> String
blank n t = concat (replicate n "  ") ++ cleanup t

cleanup :: String -> String
cleanup = dropWhile (`elem` "\t ") . filter (/= '\n')
