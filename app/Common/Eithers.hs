module Common.Eithers where

hasData :: Either a [b] -> Bool
hasData (Left _) = False
hasData (Right []) = False
hasData _ = True
