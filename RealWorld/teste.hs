import Control.Monad 


main = do 
   colors <- forM [1,2,3,4] (\a -> do 
        putStrLn $ "Which color " ++ show a ++ "?"
        color <- getLine 
        return color)
   putStrLn "The colors that: " 
   mapM_ putStrLn colors 