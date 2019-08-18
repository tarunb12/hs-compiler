{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Lexer where

import Prelude hiding (EQ, GT, LT)
import Text.Parsec (Parsec, ParsecT, Stream, (<|>), many, skipMany)
import Text.Parsec.Char (anyChar, char, digit, letter, satisfy, spaces, string)
import Text.Parsec.Combinator (between, manyTill, many1)
import Text.Parsec.Pos (incSourceColumn, incSourceLine)
import Text.Parsec.Prim (tokenPrim)

data Token
  = LPAR | RPAR | LBRACE | RBRACE | COMMA | SEMI | APOSTROPHE | SQUOTE
  | PLUS | MINUS | MUL | DIV | ASSIGN | NOT | MOD | XOR
  | INCREMENT | DECREMENT
  | PLUSEQ | MINUSEQ | MULEQ | DIVEQ
  | LS | RS
  | EQ | NEQ | LT | LTE | GT | GTE | AND | OR
  | IF | ELSE | FOR | WHILE | BREAK | CONTINUE | RETURN
  | TINT | TFLOAT | TBOOL | TCHAR | TSTRING | TUNIT
  | NL | WS
  | ID String
  | INT Int
  | FLOAT Float
  | CHAR Char
  | BOOL Bool
  | STRING String
  deriving (Eq, Show)

type Lexer = Parsec [Char] ()
type Parser = Parsec [Token] ()

lexer :: Lexer [Token]
lexer = spaces >> many1 (token <* spaces)

token :: Lexer Token
token =  (char '\n' >> return NL)
     <|> (char '\t' >> return WS)
     <|> (char ' ' >> return WS)
     <|> (char '(' >> return LPAR)
     <|> (char ')' >> return RPAR)
     <|> (char '{' >> return LBRACE)
     <|> (char '}' >> return RBRACE)
     <|> (char ',' >> return COMMA)
     <|> (char ';' >> return SEMI)
     <|> (char '=' >> return ASSIGN)

     <|> (char '+' >> return PLUS)
     <|> (char '-' >> return MINUS)
     <|> (char '*' >> return MUL)
     <|> (char '/' >> return DIV)
     <|> (char '%' >> return MOD)

     <|> (string "++" >> return INCREMENT)
     <|> (string "--" >> return DECREMENT)
     <|> (string "+=" >> return PLUSEQ)
     <|> (string "-=" >> return MINUSEQ)
     <|> (string "*=" >> return MULEQ)
     <|> (string "/=" >> return DIVEQ)
     <|> (string "<<" >> return LS)
     <|> (string ">>" >> return RS)

     <|> (char '<' >> return LT)
     <|> (char '>' >> return GT)
     <|> (char '!' >> return NOT)
     <|> (char '^' >> return XOR)
     <|> (string "==" >> return EQ)
     <|> (string "!=" >> return NEQ)
     <|> (string "<=" >> return LTE)
     <|> (string ">=" >> return GTE)
     <|> (string "&&" >> return AND)
     <|> (string "||" >> return OR)

     <|> (string "if" >> return IF)
     <|> (string "else" >> return ELSE)
     <|> (string "for" >> return FOR)
     <|> (string "while" >> return WHILE)
     <|> (string "break" >> return BREAK)
     <|> (string "continue" >> return CONTINUE)
     <|> (string "return" >> return RETURN)

     <|> (string "int" >> return TINT)
     <|> (string "float" >> return TFLOAT)
     <|> (string "bool" >> return TBOOL)
     <|> (string "char" >> return TCHAR)
     <|> (string "string" >> return TSTRING)
     <|> (string "unit" >> return TUNIT)

     <|> (string "true" >> (return $ BOOL True))
     <|> (string "false" >> (return $ BOOL False))

     <|> (string "//" >> manyTill anyChar '\n')
     <|> (string "/*" >> manyTill anyChar (try (string "*/")))

    --  (INT i -> Token) (f INT i)  
     <|> (ID <$> many1 letter)
     <|> (INT <$> (many1 digit >>= \i -> return $ read i))
     <|> (char '\"' >> STRING <$> manyTill anyChar (char '\"'))

newLine :: (Token -> Bool) -> Parser Token
newLine f = tokenPrim show
  (\src _ _ -> incSourceLine src 1)
  (\x -> if f x then Just x else Nothing)

satisfy' :: (Token -> Bool) -> Parser Token
satisfy' f = tokenPrim show
  (\src _ _ -> incSourceColumn src 1)
  (\x -> if f x then Just x else Nothing)

lpar, rpar :: Parser Token
lpar = satisfy' $ \case { LPAR -> True ; _ -> False }
rpar = satisfy' $ \case { RPAR -> True ; _ -> False }

newline :: Parser Token
newline = satisfy' $ \case { NL -> tokenPrim show (\s _ _ -> incSourceColumn s 1) (\x -> Just x) >> True ; _ -> False }

ws :: Parser Token
ws = 

boolean :: Parser Bool
boolean = (\(BOOL b) -> b) <$> (satisfy' $ \case { BOOL {} -> True ; _ -> False })

character :: Parser Char
character = (\(CHAR c) -> c) <$> (satisfy' $ \case { CHAR {} -> True ; _ -> False })

integer :: Parser Int
integer  = (\(INT n) -> n) <$> (satisfy' $ \case { INT {} -> True ; _ -> False })

id' :: Parser String
id'  = (\(ID s) -> s) <$> (satisfy' $ \case { ID {} -> True ; _ -> False })

string' :: Parser String
string' = (\(STRING s) -> s) <$> (satisfy' $ \case { STRING {} -> True ; _ -> False })