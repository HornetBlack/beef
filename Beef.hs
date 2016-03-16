module Beef ( Program(..), Op(..), parseBF ) where

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import Control.Applicative

-- Here I merge the increment and decrement ops
data Op = Inc Integer | Move Integer | PrintByte | ReadByte
        deriving (Show, Eq)
                 
data Program = Ops Op
             | Loop [Program]
             deriving (Show, Eq)

bfs :: Parser [Program]
bfs = many bf

bf :: Parser Program
bf = Ops <$> op <|>
     Loop <$> loop
     
comment :: Parser ()
comment = noneOf "+-<>.,[]" >> return ()
  
loop :: Parser [Program]
loop = do char '['
          body <- bfs
          char ']'
          return body

op :: Parser Op
op = moveChain
     <|> incChain
     <|> ioOp

total :: (Char, Char) -> String -> Integer
total (down, up) str = total' str 0
  where total' "" n = n
        total' (c:cs) n | c == down = total' cs (n - 1)
                        | c == up   = total' cs (n + 1)
                        | otherwise = total' cs n
                                    
moveChain :: Parser Op
moveChain = do chs <- some $ oneOf "<>"
               let n = total ('<', '>') chs
               return $ Move n

incChain :: Parser Op
incChain = do chs <- some $ oneOf "-+"
              let n = total ('-', '+') chs
              return $ Inc n

ioOp :: Parser Op
ioOp =  (char '.' >> return PrintByte)
        <|> (char ',' >> return ReadByte)

parseBF :: String -> Either ParseError [Program]
parseBF input = parse bfs "brainfuck" input
