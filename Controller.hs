
{-# LANGUAGE LambdaCase #-}
module Controller where

import Logic
import qualified UI.NCurses as C
import Data.Maybe
import Control.Monad.Random   



--Input Handler
getInput :: C.Curses Input
getInput = let mif = fmap (getKey . fromJust)  <$> (`C.getEvent` Nothing)
           in C.defaultWindow >>= mif >>= \case
                                             Just i -> pure i
                                             Nothing -> getInput
    where
        getKey (C.EventCharacter 'w') = Just $ Move north
        getKey (C.EventCharacter 'a') = Just $ Move west
        getKey (C.EventCharacter 's') = Just $ Move south
        getKey (C.EventCharacter 'd') = Just $ Move east
        getKey (C.EventCharacter '\ESC') = Just $ Escape
        getKey _ = Nothing

--Input Handler
getInputTitle :: C.Curses Input
getInputTitle = let mif = fmap (getKey . fromJust)  <$> (`C.getEvent` Nothing)
           in C.defaultWindow >>= mif >>= \case
                                             Just i -> pure i
                                             Nothing -> getInput
    where
        getKey (C.EventCharacter _) = Nothing

--Returns whether input is a movement or escape
checkType :: Input -> Bool
checkType (Move x) = False
checkType _ = True