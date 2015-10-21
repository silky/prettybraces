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
go bs fs = case fs of
  [] -> getContents >>= goT (makeItem bs) "STDIN"
  _  -> mapM_ (goF (makeItem bs)) fs

goF :: Parsec String Item -> FilePath -> IO ()
goF item f = readFile f >>= goT item ("File: " ++ f)

goT :: Stream s t => Parsec s Item -> String -> s -> IO ()
goT item f t = case parse (many item <* eof) f t
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

makeItem :: [Braces] -> Parsec String (Either String Group)
makeItem bs = item
  where
  item       =  moreBraces <|> nonBraces
  moreBraces = choice $ map (fmap Right . mkParser item) bs
  nonBraces  = fmap Left (nonBracket bs)

mkParser :: MonadParsec s m Char => m Item -> Braces -> m Group
mkParser item p@(l,r) = do
  is <- between (string l) (string r) (many item)
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
