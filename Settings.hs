-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Yesod.Default.Util
import Control.Applicative
import Control.Exception (throw)
import Data.Aeson
import Data.Text (Text)
import Data.Default (def)
import Text.Hamlet
import Database.Persist.Postgresql (PostgresConf)
import Network.Wai.Handler.Warp (HostPreference)
import Data.String (fromString)
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Data.ByteString (ByteString)
import Data.Monoid (mempty)
import Yesod.Default.Config2 (applyEnv)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir :: FilePath
    -- ^ Directory from which to serve static files.
    , appPostgresConf :: PostgresConf
    -- ^ Configuration settings for accessing the PostgreSQL database.
    , appRoot :: Text
    -- ^ Base for all generated URLs.
    , appHost :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort :: Int
    -- ^ Port to listen on

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics :: Maybe Text
    -- ^ Google Analytics code
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appPostgresConf           <- o .: "database"
        appRoot                   <- o .: "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .: "copyright"
        appAnalytics              <- o .:? "analytics"

        return AppSettings {..}

-- Static setting below. Changing these requires a recompile

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYml :: ByteString
configSettingsYml = $(embedFile "config/settings.yml")

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case decodeEither' configSettingsYml of
        Left e -> throw e
        Right value ->
            case fromJSON $ applyEnv mempty value of
                Error e -> error e
                Success settings -> settings

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppSettings -> Text
staticRoot settings = [st|#{appRoot settings}/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings
