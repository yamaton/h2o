module Version
  ( versionStr,
  )
where

import Data.Text (Text, pack)
import Data.Version (showVersion)
import Paths_h2o (version)

versionStr :: Text
versionStr = pack (showVersion version)
