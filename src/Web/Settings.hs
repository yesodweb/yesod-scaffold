{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Web.Settings where

import ClassyPrelude.Yesod
import Control.Exception qualified as Exception
import Data.Aeson
  ( Result (..),
    fromJSON,
    withObject,
    (.!=),
    (.:?),
  )
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax (Exp, Name, Q)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)
import Yesod.Default.Util
  ( WidgetFileSettings,
    widgetFileNoReload,
    widgetFileReload,
  )

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
  { -- | Directory from which to serve static files.
    appStaticDir :: String,
    -- | Configuration settings for accessing the database.
    appDatabaseConf :: PostgresConf,
    -- | Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    appRoot :: Maybe Text,
    -- | Host/interface the server should bind to.
    appHost :: HostPreference,
    -- | Port to listen on
    appPort :: Int,
    -- | Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.
    appIpFromHeader :: Bool,
    -- | Use detailed request logging system
    appDetailedRequestLogging :: Bool,
    -- | Should all log messages be displayed?
    appShouldLogAll :: Bool,
    -- | Use the reload version of templates
    appReloadTemplates :: Bool,
    -- | Assume that files in the static dir may change after compilation
    appMutableStatic :: Bool,
    -- | Perform no stylesheet/script combining
    appSkipCombining :: Bool,
    -- Example app-specific configuration values.

    -- | Copyright text to appear in the footer of the page
    appCopyright :: Text,
    -- | Google Analytics code
    appAnalytics :: Maybe Text,
    -- | Indicate if auth dummy login should be enabled.
    appAuthDummyLogin :: Bool
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    appStaticDir <- o .: "static-dir"
    appDatabaseConf <- o .: "database"
    appRoot <- o .:? "approot"
    appHost <- fromString <$> o .: "host"
    appPort <- o .: "port"
    appIpFromHeader <- o .: "ip-from-header"
    appDetailedRequestLogging <- o .:? "detailed-logging" .!= True
    appShouldLogAll <- o .:? "should-log-all" .!= True
    appReloadTemplates <- o .:? "reload-templates" .!= True
    appMutableStatic <- o .:? "mutable-static" .!= True
    appSkipCombining <- o .:? "skip-combining" .!= True

    appCopyright <- o .: "copyright"
    appAnalytics <- o .:? "analytics"

    appAuthDummyLogin <- o .:? "auth-dummy-login" .!= True

    return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile =
  ( if appReloadTemplates compileTimeAppSettings
      then widgetFileReload
      else widgetFileNoReload
  )
    widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue =
  either Exception.throw id $
    decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
  case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
    Error e -> error e
    Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets =
  combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts =
  combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
