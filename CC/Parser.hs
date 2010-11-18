{-# LANGUAGE TypeSynonymInstances #-}

module CC.Parser (parse,makeBW,ReadCC(..)) where

import Text.Parsec hiding (choice,parse)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Lex
import Text.Parsec.Language (emptyDef)

import CC.Syntax
import CC.Tree
import CC.Show


----------------------
-- Public Interface --
----------------------

parse :: ReadCC a => String -> TreeCC a
parse = either (error . show) id . runParser justExpr () "" . makeBW
  where justExpr = expr >>= \e -> eof >> return e

-- strip out any color information
makeBW :: String -> String
makeBW ('\27':s) = (makeBW . tail . dropWhile (/= 'm')) s
makeBW (c:s)     = c : makeBW s
makeBW []        = []

class TreeVal a => ReadCC a where
  readCC :: Parser a

instance ReadCC Int where
  readCC = integer >>= return . fromInteger

instance ReadCC String where
  -- this is kind of a hacky solution, but works for the most part
  -- when in doubt, parenthesize expressions and you should be good
  readCC = do s  <- manyTill anyChar (lookAhead end)
              return (read ('"' : s ++ "\""))
    where end = ignore comma      <|>
                ignore (char '{') <|> 
                ignore (char '}') <|>
                ignore (char ')') <|>
                ignore (char '>') <|> eof

-----------
-- Lexer --
-----------

lexer = Lex.makeTokenParser def
  where def = emptyDef {
                Lex.reservedNames   = ["dim","let","in"],
                Lex.reservedOpNames = ["=","$"]
              }

parens     = Lex.parens     lexer
braces     = Lex.braces     lexer
angles     = Lex.angles     lexer
comma      = Lex.comma      lexer
identifier = Lex.identifier lexer
natural    = Lex.natural    lexer
integer    = Lex.integer    lexer
operator   = Lex.operator   lexer
reserved   = Lex.reserved   lexer
reservedOp = Lex.reservedOp lexer


-------------
-- Parsers --
-------------

expr :: ReadCC a => Parser (TreeCC a)
expr = parens expr <|> bind <|> ref <|> dim <|> try choice <|> struct

ignore :: Parser a -> Parser ()
ignore p = p >> return ()

list :: Parser a -> Parser [a]
list = flip sepBy comma

var :: Parser String
var = reservedOp "$" >> identifier

struct :: ReadCC a => Parser (TreeCC a)
struct = do
    a  <- readCC
    es <- option [] (braces (list expr))
    return (node a es)

choice :: ReadCC a => Parser (TreeCC a)
choice = do
    d  <- identifier
    as <- angles (list expr)
    return (Chc d as)

dim :: ReadCC a => Parser (TreeCC a)
dim = do 
    reserved "dim"
    d  <- identifier
    ts <- angles (list identifier)
    reserved "in"
    e  <- expr
    return (Dim d ts e)

bind :: ReadCC a => Parser (TreeCC a)
bind = do
    reserved "let"
    v <- var
    reservedOp "="
    b <- expr
    reserved "in"
    u <- expr
    let b' = b `asTypeOf` u -- hack (and wrong)
    return (Let v (Bnd b') u)

ref :: Parser (TreeCC a)
ref = var >>= return . Ref
