module Domain.Data.User where

type UserId = Int

data User = User
  { userId :: UserId
  , userName :: String
  }

instance Show User where
  show user = mconcat
    [ show $ userId user
    , ".) "
    , userName user
    ]
