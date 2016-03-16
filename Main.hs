module Main (main) where

{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Beef
import Data.Word
import System.Exit (exitFailure)
import Control.Monad.Trans.State
import Control.Monad.IO.Class

data Tape a = Tape a [a] [a]

type Machine a = StateT (Tape Word8) IO a

showMachine :: Machine () -> IO String
showMachine m = do let ioT = runStateT m infinteTape
                   ((), Tape x l r) <- ioT
                   return $ "\n" ++ (show . reverse . trunc $ l) ++
                     " " ++ (show x) ++ " " ++
                     (show . trunc $ r)
  where trunc = take 10

goLeft :: Machine ()
goLeft = do (Tape x (l:ls) rs) <- get
            put (Tape l ls (x:rs))
goRight :: Machine ()
goRight = do (Tape x ls (r:rs)) <- get
             put (Tape r (x:ls) rs)

moveTape :: Integer -> Machine ()
moveTape 0 = return ()
moveTape n | n < 0 = do goLeft; moveTape $ n+1
           | n > 0 = do goRight; moveTape $ n-1

increaseValue :: Integer -> Machine ()
increaseValue n = do let n' = fromInteger n
                     (Tape x l r) <- get
                     put (Tape (x + n') l r)

getValue :: (Num a) => Machine Word8
getValue = do (Tape x _ _) <- get
              return x
setValue :: Word8 -> Machine ()
setValue x = do (Tape _ l r) <- get
                put (Tape x l r)

doOp :: Op -> Machine ()
doOp (Inc n) = increaseValue n
doOp (Move n) = moveTape n
doOp PrintByte = do ch <- getValue
                    liftIO $ putChar . toEnum . fromEnum $ ch
doOp ReadByte = do ch <- liftIO getChar
                   setValue . toEnum . fromEnum $ ch

doLoop :: [Program] -> Machine ()
doLoop body = do v <- getValue
                 if v /= 0
                   then run body >> doLoop body
                   else return ()

run :: [Program] -> Machine ()
run [] = return ()
run ((Ops op):xs) = doOp op >> run xs
run ((Loop body):xs) = doLoop body >> run xs

newMachine :: Machine ()
newMachine = return ()

finiteTape :: Int -> Tape Word8
finiteTape n = Tape 0 [] (replicate (n-1) 0)

infinteTape :: Tape Word8
infinteTape = Tape 0 [] (repeat 0)

runProgram :: String -> IO ()
runProgram s =
  let s' = filter (\c -> not $ c `elem` "<>+-,.[]") s
  in case parseBF s' of
      Left err -> error $ show err
      Right prog -> (showMachine $ run prog) >>= putStrLn

  
main = putStr "Not implemented" >> exitFailure

helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

rot13 = "-,+[-[>>++++[>++++++++<-]<+<-[>+>+>-[>>>]<[[>+<-]>>+>]<<<<<-]]>>>[-]+>--[-[<->+++[-]]]<[++++++++++++<[>-[>+>>]>[+[<+>-]>+>>]<<<<<-]>>[<+>-]>[-[-<<[-]>>]<<[<<->>-]>>]<<[<<+>>-]]<[-]<.[-]<-,+]"

