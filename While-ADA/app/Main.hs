module Main where

    import System.IO
    import System.Console.ANSI
    import WhileParser
    import CFG
    import WhileStructures
    import Lib
    import SignDomain
    import CondCFunc
    import IntervalDomain
    import System.Environment

    filePath :: [String] -> String
    filePath = head

    

    main :: IO() 
    main = do 
            setSGR [ SetConsoleIntensity BoldIntensity
                , SetColor Foreground Vivid Red
                -- , SetColor Background Dull Green
                ]           
            putStrLn "Benvenuti in While-ADA v3.1!"
            setSGR [ Reset ]
            
            -- putStrLn "inserisci l'indirizzo del file da analizzare..."
            -- address <- getLine
            args <- getArgs 
            putStrLn ("\nPROGRAMMA : "++(filePath args))
            handle <- openFile (filePath args) ReadMode
            source_code <- hGetContents handle

            -- putStrLn "\n-----------CODICE------------"
            -- putStrLn source_code
            -- putStrLn "-------FINE PROGRAMMA--------"

            let [(tree,rest)] = parse parseStms source_code 
                vars = variables tree
                (sign_cfg,r) = (app (createCFG signCondC tree) 1 )::(CGraph (Sign),Int)
                (int_cfg,r') = (app (createCFG intCondC tree) 1 )::(CGraph (Interval),Int)
                in 
                -- printTree ([(tree,rest)]) >>
                printLabCode tree >>
                let (cfg,nf) = app (debugCFG tree) 1  in printCFG cfg >>
                --let (cfg,nf) = app (debugCFG tree) 1  in printAdjList cfg >>
                printDenRes tree vars >>
                printCFGRes sign_cfg vars "Segni">>
                printCFGRes int_cfg vars "Intervalli"

            hClose handle 
            putStrLn "Arrivederci"


    