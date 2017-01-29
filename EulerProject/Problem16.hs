import Data.Char

problemsixteen y = sum $ map digitToInt $ show (2^y)
                    