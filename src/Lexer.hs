{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Lexer where

import Prelude hiding (EQ, GT, LT)
import Text.Parsec (Parsec, ParsecT, Stream, (<|>), many, skipMany)
import Text.Parsec.Char (anyChar, char, digit, letter, newline, satisfy, spaces, string)
import Text.Parsec.Combinator (between, manyTill, many1)
import Text.Parsec.Pos (incSourceColumn, incSourceLine)
import Text.Parsec.Prim (tokenPrim, try)

data Token
  = LPAR | RPAR | LBRACE | RBRACE | LBRACK | RBRACK | COMMA | SEMI
  | PLUS | MINUS | MUL | DIV | ASSIGN | NOT | MOD | XOR
  | INCREMENT | DECREMENT
  | PLUSEQ | MINUSEQ | MULEQ | DIVEQ
  | LS | RS
  | EQ | NEQ | LT | LTE | GT | GTE | AND | OR
  | IF | ELSE | FOR | WHILE | BREAK | CONTINUE | RETURN
  | TINT | TFLOAT | TBOOL | TCHAR | TSTRING | TUNIT
  | NL | WS | COMMENT
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
token =  (char '\t' >> return WS)
     <|> (char ' ' >> return WS)
     <|> (char '(' >> return LPAR)
     <|> (char ')' >> return RPAR)
     <|> (char '{' >> return LBRACE)
     <|> (char '}' >> return RBRACE)
     <|> (char '[' >> return LBRACK)
     <|> (char ']' >> return RBRACK)
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

     <|> (string "//" >> manyTill anyChar (try (char '\n')) >> return COMMENT)
     <|> (string "/*" >> manyTill anyChar (try (string "*/")) >> return COMMENT)

    --  (INT i -> Token) (f INT i)  
     <|> (ID <$> many1 letter)
     <|> (INT <$> (many1 digit >>= \i -> return $ read i))
     <|> (char '\"' >> STRING <$> manyTill anyChar (char '\"'))

newLine :: Parser Token
newLine = tokenPrim show
  (\src _ _ -> incSourceLine src 1)
  (\x -> Just x)

satisfy' :: (Token -> Bool) -> Parser Token
satisfy' f = tokenPrim show
  (\src _ _ -> incSourceColumn src 1)
  (\x -> if f x then Just x else Nothing)

lpar, rpar, lbrace, rbrace, lbrack, rbrack :: Parser Token
lpar   = satisfy' $ \case { LPAR   -> True ; _ -> False }
rpar   = satisfy' $ \case { RPAR   -> True ; _ -> False }
lbrace = satisfy' $ \case { LBRACE -> True ; _ -> False }
rbrace = satisfy' $ \case { RBRACE -> True ; _ -> False }
lbrack = satisfy' $ \case { LBRACK -> True ; _ -> False }
rbrack = satisfy' $ \case { RBRACK -> True ; _ -> False }
comma  = satisfy' $ \case { COMMA  -> True ; _ -> False }
semi   = satisfy' $ \case { SEMI   -> True ; _ -> False }
assign = satisfy' $ \case { ASSIGN -> True ; _ -> False }

plus, minus, mul, div, mod :: Parser Token
plus  = satisfy' $ \case { PLUS  -> True ; _ -> False }
minus = satisfy' $ \case { MINUS -> True ; _ -> False }
mul   = satisfy' $ \case { MUL   -> True ; _ -> False }
div   = satisfy' $ \case { DIV   -> True ; _ -> False }
mod   = satisfy' $ \case { MOD   -> True ; _ -> False }

incr, decr, pluseq, minuseq, muleq, diveq, ls, rs :: Parser Token
incr    = satisfy' $ \case { INCREMENT -> True ; _ -> False }
decr    = satisfy' $ \case { DECREMENT -> True ; _ -> False }
pluseq  = satisfy' $ \case { PLUSEQ    -> True ; _ -> False }
minuseq = satisfy' $ \case { MINUSEQ   -> True ; _ -> False }
muleq   = satisfy' $ \case { MULEQ     -> True ; _ -> False }
diveq   = satisfy' $ \case { DIVEQ     -> True ; _ -> False }
ls      = satisfy' $ \case { LS        -> True ; _ -> False }
rs      = satisfy' $ \case { RS        -> True ; _ -> False }

lt, gt, not, xor, eq, neq, lte, gte, and, or :: Parser Token
lt  = satisfy' $ \case { LT  -> True ; _ -> False }
gt  = satisfy' $ \case { GT  -> True ; _ -> False }
not = satisfy' $ \case { NOT -> True ; _ -> False }
xor = satisfy' $ \case { XOR -> True ; _ -> False }
eq  = satisfy' $ \case { EQ  -> True ; _ -> False }
neq = satisfy' $ \case { NEQ -> True ; _ -> False }
lte = satisfy' $ \case { LTE -> True ; _ -> False }
gte = satisfy' $ \case { GTE -> True ; _ -> False }
and = satisfy' $ \case { AND -> True ; _ -> False }
or  = satisfy' $ \case { OR  -> True ; _ -> False }

if', else', for, while, break, continue, return' :: Parser Token
if'      = satisfy' $ \case { IF       -> True ; _ -> False }
else'    = satisfy' $ \case { ELSE     -> True ; _ -> False }
for      = satisfy' $ \case { FOR      -> True ; _ -> False }
while    = satisfy' $ \case { WHILE    -> True ; _ -> False }
break    = satisfy' $ \case { BREAK    -> True ; _ -> False }
continue = satisfy' $ \case { CONTINUE -> True ; _ -> False }
return   = satisfy' $ \case { RETURN   -> True ; _ -> False }

tint, tfloat, tbool, tchar, tstring, tunit :: Parser Token
tint    = satisfy' $ \case { TINT    -> True ; _ -> False }
tfloat  = satisfy' $ \case { TFLOAT  -> True ; _ -> False }
tbool   = satisfy' $ \case { TBOOL   -> True ; _ -> False }
tchar   = satisfy' $ \case { TCHAR   -> True ; _ -> False }
tstring = satisfy' $ \case { TSTRING -> True ; _ -> False }
tunit   = satisfy' $ \case { TUNIT   -> True ; _ -> False }

ws :: Parser Token
ws = satisfy' $ \case { WS -> True ; _ -> False }

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