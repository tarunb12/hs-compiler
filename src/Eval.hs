module Eval where

-- whiteSpace :: Parser ()
-- whiteSpace = void $ many $ oneOf " \n\t"

-- integer :: Parser Expr
-- integer = lexeme $ do
--   int <- many1 digit
--   return $ Literal $ LInt $ read int

-- -- maybeBinOp
-- maybeAdd :: Expr -> Parser Expr
-- maybeAdd expr0 = 
--   let add expr0 = do { void $ lexeme $ char '+'
--                      ; expr1 <- expr
--                      ; maybeAdd $ BinOp Add expr0 expr1
--                      }
--   in  add expr0
--   <|> return expr0

-- variable :: Parser Expr
-- variable = 
--   let firstChar = satisfy $ \a -> isLetter a || a == '_'
--       restChar  = satisfy $ \a -> isLetter a || a == '_' || isDigit a
--   in lexeme $ do
--     fst <- firstChar
--     rest <- many restChar
--     return $ Id (fst : rest)

-- parens :: Parser Expr
-- parens = lexeme $ do
--   void $ lexeme $ char '('
--   exp <- expr
--   void $ lexeme $ char ')'
--   return $ Parens exp

-- -- literal :: Parser Literal
-- -- literal = do
-- --   int <- integer

-- literal :: Parser Expr
-- literal =  integer

-- term :: Parser Expr
-- term =  literal
--     <|> variable
--     <|> parens

-- opCheck :: Expr -> Parser Expr
-- opCheck e = do
--   maybeAdd e

-- expr :: Parser Expr
-- expr = do
--   e <- term
--   opCheck e

-- statement :: Parser Statement
-- statement = do
--   e <- expr
--   return $ Expr e

-- program :: Parser Program
-- program = do
--   prog <- statement
--   return $ Program [prog]

-- -- Lexer for interpreter
-- lexeme :: Parser a -> Parser a
-- lexeme parser = do
--   whiteSpace
--   x <- parser
--   whiteSpace
--   return x