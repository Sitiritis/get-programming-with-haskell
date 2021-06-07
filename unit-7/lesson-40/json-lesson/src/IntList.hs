module IntList where

import Data.Aeson
import GHC.Generics

data IntList =
  Cons
    { hd :: Int
    , rst :: IntList
    }
  |
  EmptyList deriving (Show, Generic)

instance ToJSON IntList

intListExample :: IntList
intListExample = Cons 1 $
                 Cons 2 EmptyList
