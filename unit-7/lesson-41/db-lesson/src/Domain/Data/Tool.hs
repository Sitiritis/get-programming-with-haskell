module Domain.Data.Tool where

import Data.Time

type ToolId = Int

data Tool = Tool
  { toolId :: ToolId
  , name :: String
  , description :: String
  , lastReturned :: Day
  , timesBorrowed :: Int
  }

instance Show Tool where
  show tool = mconcat
    [ show $ toolId tool, ".) "
    , name tool
    , "\n description: ", description tool
    , "\n last returned: ", show $ lastReturned tool
    , "\n times borrowed: ", show $ timesBorrowed tool
    , "\n"
    ]
