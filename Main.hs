{-# LANGUAGE LambdaCase #-}
module Main where

import qualified UI.NCurses as C
import Logic
import Rendering
import Controller
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Linear
import System.Exit

main :: IO()
main =  do
   titleScreen
   runGame

--The implementation of the "game loop"
runGame :: IO()
runGame = do
   initGame >>= \g -> C.runCurses $ do
         C.setCursorMode C.CursorInvisible
         C.setEcho       False
         (w,h) <- C.screenSize
         gameLoop g 0 
    where
       gameLoop :: Game -> Int -> C.Curses()
       gameLoop g i = do
          render g i
          e <- getInput
          if checkType e then return ()
          else do
             let dec = pcomp (g ^. wanderer) (g^. door)
             let step = extractMove e
             if not dec then do
                let ng = update step g
                gameLoop ng i
             else do
                liftIO initGame >>= \new -> gameLoop new (i+1)
       render :: Game -> Int -> C.Curses()
       render ng score = do
          dw <- C.defaultWindow
          C.updateWindow dw $ do
              let pcGlyph    = C.Glyph 'λ' [ C.AttributeBold ]
              let doorGlyph  = C.Glyph '▓' [C.AttributeBold]
              let (V2 wx wy) = point2Vector $ (_wanderer ng)
              let (V2 dx dy) = point2Vector $ (_door ng)
              C.clear
              C.drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
              C.moveCursor (fromIntegral wy) (fromIntegral wx)
              C.drawGlyph pcGlyph
              C.moveCursor (fromIntegral dy) (fromIntegral dx)
              C.drawGlyph doorGlyph
              C.moveCursor 0 0
              C.drawString ("Score: " ++ show score)
          C.render

--Converts an input into a direction. non-exhaustive!
extractMove :: Input -> Direction
extractMove (Move x) = x

