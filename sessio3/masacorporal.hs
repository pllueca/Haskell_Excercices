import System.IO

-- index de massa corporal
indexMassa :: Float -> String
indexMassa x
  | x < 18.0 = "magror"
  | x >= 18.0 && x < 25.0 = "corpulencia normal"
  | x >= 25.0 && x < 30.0 = "sobrepes"
  | x >= 30.0 && x < 40.0 = "obesitat"
  | otherwise = "obesitat morbida"
                 
imc :: Float -> Float -> Float
imc m h = m / (h * h)


--getWord :: IO String 
getWord = do c <- getChar
             if (c == '\n') || (c == ' ')
               then return ""
               else do w <- getWord
                       return (c:w)
                
                    
calcIMC = do nom <- getWord
             if nom == "*" then return()
               else do
               m <- getWord
               h <- getWord
               putStr nom
               putStr ": "
               putStrLn $ indexMassa $ imc (read m :: Float) (read h :: Float)
               calcIMC

main = calcIMC  
  
          
          
          