{-# LANGUAGE OverloadedStrings #-}

module Version
  ( versionStr,
  )
where

import Data.Text (Text)

-- [FIXME] Isn't there a good way to get it from package.yaml?
versionStr :: Text
versionStr = "0.4.9"
