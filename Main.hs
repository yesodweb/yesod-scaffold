import Application () -- for YesodDispatch instance
import Foundation
import Yesod

main :: IO ()
main = warp 3000 App
