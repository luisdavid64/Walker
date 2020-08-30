module Controller where

import Logic
import qualified UI.NCurses as C
import Data.Maybe
import Control.Monad.Random   


--Input handler
getInput :: C.Curses Input
getInput = do
    w <- C.defaultWindow
    input <- fmap (getKey . fromJust) $ C.getEvent w Nothing
    case input of 
        Just i -> pure i
        Nothing -> getInput
    where
        getKey (C.EventCharacter 'w') = Just $ Move north
        getKey (C.EventCharacter 'a') = Just $ Move west
        getKey (C.EventCharacter 's') = Just $ Move south
        getKey (C.EventCharacter 'd') = Just $ Move east
        getKey (C.EventCharacter '\ESC') = Just $ Escape
        getKey _ = Nothing

--Catches any input for title screen
dummyInput :: C.Curses ()
dummyInput = do
    w <- C.defaultWindow
    input <- (C.getEvent w Nothing)
    return ()

--Returns whether input is a movement or escape
checkType :: Input -> Bool
checkType (Move x) = False
checkType _ = True