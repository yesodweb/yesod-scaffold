{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}
module Home where

import Prelude
import Language.Fay.FFI
import Language.Fay.Yesod
import SharedTypes

data Element

getElementById :: String -> Fay Element
getElementById = ffi "document.getElementById(%1)"

getAttribute :: String -> Element -> Fay String
getAttribute = ffi "%2[%1]"

setInnerHTML :: Element -> String -> Fay ()
setInnerHTML = ffi "%1.innerHTML=%2"

onKeyUp :: Element -> Fay () -> Fay ()
onKeyUp = ffi "%1.onkeyup=%2"

alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

parseInt :: String -> Fay Int
parseInt = ffi "window.parseInt(%1, 10)"

main :: Fay ()
main = do
    input <- getElementById "fibindex"
    result <- getElementById "fibresult"
    onKeyUp input $ do
        indexS <- getAttribute "value" input
        index <- parseInt indexS
        call (GetFib index) $ setInnerHTML result . show
