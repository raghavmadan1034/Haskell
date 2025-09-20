safeDiv _ 0 = Nothing
safeDiv m n = Just (m/n)


addMaybe Nothing _ = Nothing
addMaybe _ Nothing = Nothing
addMaybe (Just x) (Just y) = Just (x+y)

