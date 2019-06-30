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
            
            putStrLn titleStr 
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


    titleStr = "\n888       888 888      d8b 888                d8888 8888888b.        d8888\n888   o   888 888      Y8P 888               d88888 888  'Y88b      d88888\n888  d8b  888 888          888              d88P888 888    888     d88P888\n888 d888b 888 88888b.  888 888  .d88b.     d88P 888 888    888    d88P 888\n888d88888b888 888 '88b 888 888 d8P  Y8b   d88P  888 888    888   d88P  888\n88888P Y88888 888  888 888 888 88888888  d88P   888 888    888  d88P   888\n8888P   Y8888 888  888 888 888 Y8b.     d8888888888 888  .d88P d8888888888\n888P     Y888 888  888 888 888  'Y8888 d88P     888 8888888P' d88P     888\n"