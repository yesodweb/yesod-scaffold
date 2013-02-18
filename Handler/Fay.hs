module Handler.Fay where

import Import
import Yesod.Fay
import Fay.Convert (readFromFay)

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

onCommand :: CommandHandler App App
onCommand render command =
    case readFromFay command of
      Just (GetFib index r) -> render r $ fibs !! index
      Nothing               -> invalidArgs ["Invalid command"]
