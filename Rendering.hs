module Rendering where

import qualified UI.NCurses as C
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Controller
import System.IO
        
--Function to print a [String] banner recursively
bannerPrint :: Integer -> Integer -> [String] -> C.Update()
bannerPrint x y [] = return ()
bannerPrint x y (w:walker) = do
    C.moveCursor x y
    C.drawString w 
    bannerPrint (x+1) y walker

--A function to print the controls of the game
wasd :: Integer -> Integer -> C.Update()
wasd w h = do
    C.moveCursor (w  `div` 2 + 2) (h `div` 2 - 16)
    C.drawString "Move: "
    C.moveCursor (w  `div` 2 + 2) (h `div` 2 - 8)
    C.drawString "w"
    C.moveCursor (w  `div` 2 + 3) (h `div` 2 - 8)
    C.drawString "s"
    C.moveCursor (w  `div` 2 + 3) (h `div` 2 - 10)
    C.drawString "a"
    C.moveCursor (w  `div` 2 + 3) (h `div` 2 - 6)
    C.drawString "d"

--Displays the title screen
titleScreen :: IO()
titleScreen = C.runCurses $ do
         C.setCursorMode C.CursorInvisible
         C.setEcho       False
         title <- liftIO $ readFile "title.txt"
         renderTitleScreen title
    where
        renderTitleScreen :: String -> C.Curses()
        renderTitleScreen title = do 
            (w,h) <- C.screenSize
            dw <- C.defaultWindow
            color <- C.newColorID C.ColorYellow C.ColorDefault 1 
            isColorable <- C.supportsColor
            (nothing: walker) <- return $ lines title
            C.updateWindow dw $ do 
              when isColorable $ C.setColor color
              C.clear
              bannerPrint 3 15 walker
              C.moveCursor (w  `div` 2) (h `div` 2 - 20)
              C.drawString "Controls:"
              wasd w h
              C.moveCursor (w  `div` 2 + 5) (h `div` 2 - 16)
              C.drawString "ESC: quit"
              C.moveCursor (w `div` 2 + 8) (h `div` 2 - 12)
              C.drawString "Press any key to start..."
            C.render
            _ <- dummyInput
            return ()