module GamersData where

import qualified Data.Map as Map


type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [
    (1, "nYarlathoTep"),
    (2, "KINGinYELLOW"),
    (3, "dagon1997"),
    (4, "rcarter1919"),
    (5, "xCTHULHUx"),
    (6, "yogSOThoth")
  ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [
    ("nYarlathoTep", 2000),
    ("KINGinYELLOW", 15000),
    ("dagon1997", 300),
    ("rcarter1919", 12),
    ("xCTHULHUx", 50000),
    ("yogSOThoth", 150000)
  ]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromIdAlt :: GamerId -> Maybe PlayerCredits
creditsFromIdAlt id = altLookupCredits $ lookupUserName id

{-|

>>> creditsFromIdAlt 1
>>> creditsFromIdAlt 100
Just 2000
Nothing

>>> creditsFromIdStrange id = lookupCredits <$> lookupUserName id
>>> :t creditsFromIdStrange
creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits)

|-}

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [
    (1001, 1),
    (1002, 2),
    (1003, 3),
    (1004, 4),
    (1005, 5),
    (1006, 6)
  ]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId wId = Map.lookup wId gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId wId = lookupGamerId wId >>= lookupUserName >>= lookupCredits

{-|

>>> creditsFromWCId 1001
>>> creditsFromWCId 100
Just 2000
Nothing

|-}
