module Misc where

import Prelude

-- Define any little helper functions you need here.

foreach :: Functor f => f a -> (a -> b) -> f b
foreach = flip fmap

