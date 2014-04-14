import System.IO

main = do l <- getLine
          if (last l) == 'a' || (last l) == 'A'
            then putStrLn "Hola maca!"
            else putStrLn "Hola maco!"
  
  