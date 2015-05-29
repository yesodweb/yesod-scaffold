module App.NoFoundationImport
    ( module Import
    , Form
    ) where

import ClassyPrelude.Yesod   as Import

import App.DB.Model          as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Auth            as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import


-- | A convenient synonym for creating forms.
type Form app x =
    (RenderMessage app FormMessage)
    => Html -> MForm (HandlerT app IO) (FormResult x, WidgetT app IO ())
