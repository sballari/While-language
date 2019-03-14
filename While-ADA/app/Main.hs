module Main where

    import System.IO
    main :: IO() 
    main = do 
            putStrLn "Benvenuti in While-ADA v3.1!"
            putStrLn "inserisci l'indirizzo del file da analizzare..."
            address <- getLine

            handle <- openFile address ReadMode
            source_code <- hGetContents handle

            putStrLn "\n-----------CODICE------------"
            putStrLn source_code
            putStrLn "-------FINE PROGRAMMA--------"

            case (parse parseStms source_code) of
                [] -> putStrLn "ERRORE: parsing non riuscito"
                [(program,rest)] -> 
                    if rest /= "" then 
                        putStrLn "ERRORE: parsing non completo"
                        putStr "rimasto : "
                        putStr rest
                    else  
                        putStrLn "\n-----------ALBERO------------"
                        putStrLn (show program)
                        putStrLn "----------FINE ALBERO--------"


            putStrLn "\n-----------ANALISI------------"
            putStrLn "qualcosa succedera' qua...."
            putStrLn "----------FINE ANALISI--------"

            hClose handle 
            putStrLn "Arrivederci"
            

    