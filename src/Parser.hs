module Parser (
    Bodies,
    Structures,
    AST
) where 

import Lexer

data Variable ty id a = Pointer [ty] id (Maybe a) | Reference [ty] id (Maybe a) | Var [ty] id (Maybe a)
    | Literal ty a | Function [ty] id (Maybe a) | Identifier id 
    deriving (Show, Eq)

data Tags = Definition | Call | Ret | Assignment | Operation | Enum | Typedef |
    For | If | While | Switch | DoWhile | Struct | Union | Function deriving (Show, Eq)

data AST tag a = Body a | Structure tag [AST tag a]
    deriving (Show, Eq)

tokensToAST :: [(Tokens, String)] -> AST Tags (Variables [String] String String) 
tokensToAST tokens = 

