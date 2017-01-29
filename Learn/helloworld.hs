main = do 
  putStrLn "Hello, como te chamas?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!!!")