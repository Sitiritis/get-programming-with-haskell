successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing


incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just i) = Just $ i + 1
incMaybe Nothing = Nothing

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe = fmap reverse

-- instance Functor Maybe where
--   fmap func (Just x) = Just $ func x
--   fmap _ Nothing = Nothing

successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest
