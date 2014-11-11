-- | Simple DOM operations.

module DOM where

import Data.Text
import FFI

data Element

getElementById :: Text -> Fay Element
getElementById = ffi "document.getElementById(%1)"

getAttribute :: Text -> Element -> Fay Text
getAttribute = ffi "%2[%1]"

setInnerHTML :: Element -> Text -> Fay ()
setInnerHTML = ffi "%1.innerHTML=%2"

onKeyUp :: Element -> Fay () -> Fay ()
onKeyUp = ffi "%1.onkeyup=%2"

alert :: Text -> Fay ()
alert = ffi "window.alert(%1)"

parseInt :: Text -> Fay Int
parseInt = ffi "window.parseInt(%1, 10)"
