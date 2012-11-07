module Handler.Fay where

import Import
import Yesod.Fay

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

onCommand :: CommandHandler App App
onCommand render command =
    case command of
        GetFib index r -> render r $ fibs !! index
