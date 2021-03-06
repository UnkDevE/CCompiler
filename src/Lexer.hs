module Lexer (
    tokenize,
    Token
)
where

import Data.Maybe
import Data.List.Split
import Text.Read 
import qualified Data.Text as T

data Token = As | Cast | Key | Sep | ROp | Op | Com | Id | Int | Float | Cha | Str 
     deriving (Show, Eq)

lexseperators :: String 
lexseperators = " =+-/&|^<>:"

normseperators :: String 
normseperators = "[](){};"

seperators :: [T.Text]
seperators = map T.pack [":", "[", "]", "(", ")", "{", "}", ";"]

keys :: [T.Text]
keys = map T.pack ["for", "return", "while", "if", "do", "switch", "case", "break", "default", "continue", "else", "struct", "union", "enum", "typedef"]

casts :: [T.Text]
casts = map T.pack ["short", "long", "signed", "int", "float", "double", 
    "unsigned", "volitile", "void", "auto", "char",
    "register", "static"
    ]

operators :: [T.Text]
operators = map T.pack ["+", "-", "/", "*", "sizeof", "++", "--", "?"]

rOperators :: [T.Text]
rOperators = map T.pack
    ["==", ">", "<", "<=", ">=", "!=", "&&", "||", "!", "<<", ">>", 
    "~", "^"] 

comments :: [T.Text] 
comments = map T.pack ["/*", "*/"]

asignments :: [T.Text]
asignments = map T.pack ["=", "+=", "/=", "-=", "*=", "%="]

tokenAssoc :: [(Token, [T.Text])]
tokenAssoc = [(As, asignments), (Sep, seperators), (Key, keys), (Op, operators), 
        (ROp, rOperators), (Com, comments), (Cast, casts)]

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
