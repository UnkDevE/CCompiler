module Lexer (
    tokenize,
    Token
)
where

import Data.Maybe
import Data.List.Split
import Text.Read 
import qualified Data.Text as T

data Token = Key | Sep | ROp | Op | Com | Id | Int | Float | Cha | Str | None
     deriving (Show, Eq)

lexseperators :: String 
lexseperators = " =+-/&|^<>:"

normseperators :: String 
normseperators = "(){};"

seperators :: [T.Text]
seperators = map T.pack ["(", ")", "{", "}", ";"]

keywords :: [T.Text]
keywords = map T.pack ["short", "long", "signed", "int", "float", "double", 
    "unsigned", "volitile", "struct", "union", "void", "auto", "char", "enum",
    "register", "static", "return", "for", "while", "if", "do", "switch",
    "case", "break", "default", "continue", "else", ":"]

operators :: [T.Text]
operators = map T.pack ["+", "-", "/", "*", "sizeof", "++", "--", "?"]

rOperators :: [T.Text]
rOperators = map T.pack
    ["==", ">", "<", "<=", ">=", "!=", "&&", "||", "!", "<<", ">>", 
    "~", "^"] 

comments :: [T.Text] 
comments = map T.pack ["/*", "*/"]

tokenAssoc :: [(Token, [T.Text])]
tokenAssoc = [(Sep, seperators), (Key, keywords), (Op, operators), 
        (ROp, rOperators), (Com, comments)]

isInt :: String -> Bool
isInt str = isJust $ (readMaybe str :: Maybe Int)

isFloat :: String -> Bool 
isFloat str = isJust $ (readMaybe str :: Maybe Double)

isString :: String -> Bool
isString str = isJust $ (readMaybe str :: Maybe String)

isChar :: String -> Bool 
isChar str = isJust $ (readMaybe str :: Maybe Char)

literalsOrId :: String -> (Token, String)
literalsOrId str
    | isInt str = (Int, str)
    | isFloat str = (Float, str)
    | isString str = (Str, str)
    | isChar str = (Cha, str) 
    | otherwise = (Id, str)

tagTokens :: [[String]] -> [(Token, String)] 
tagTokens [] = []
tagTokens ([]:rrs) = tagTokens rrs
tagTokens ((x:rs):rrs) 
    | isGenTok == [] = if x /= " " then 
            literalsOrId x : tagTokens (rs:rrs)
        else 
            tagTokens (rs:rrs)
    | otherwise = (fst $ head isGenTok, x) : tagTokens (rs:rrs)
    where isGenTok = filter snd  
            $ map (\(t, eqs) -> (t, (T.pack x) `elem` eqs)) tokenAssoc

tokenize :: T.Text -> [(Token, String)]
tokenize str = tagTokens tokens
    where tokensBySep = split (oneOf normseperators) (T.unpack $ T.strip str) 
          tokens = map (filter (/= "") . split (oneOf lexseperators)) tokensBySep
