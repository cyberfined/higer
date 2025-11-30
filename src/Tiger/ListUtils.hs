module Tiger.ListUtils (unsnoc) where

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = unsnoc' []
  where unsnoc' acc (x:xs@(_:_)) = unsnoc' (x:acc) xs
        unsnoc' acc [x]          = Just (reverse acc, x)
        unsnoc' _ _              = Nothing
