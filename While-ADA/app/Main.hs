module Main where

    import System.IO
    import WhileParser
    import CFG
    import WhileStructures
    import Lib

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

            let [(tree,rest)] = parse parseStms source_code 
                (cfg,nf) = app (debugCFG tree) 1 
                in 
                printTree ([(tree,rest)]) >>
                printCFG cfg >>
                printDenRes tree

            

            hClose handle 
            putStrLn "Arrivederci"


    