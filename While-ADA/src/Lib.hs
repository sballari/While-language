module Lib where

    import System.IO
    import WhileParser
    import CFG
    import WhileStructures
    import AbsDenSem
    import AbsState
    import SignDomain
    import AbsEval
    import IntervalDomain
    import AbsCfgSem
    import CondCFunc

    printTree :: [(Stm,String)] -> IO ()
    printTree resultP = 
        do
            putStrLn "\n-----------ALBERO------------"
            case resultP of
                [] -> putStrLn "ERRORE: parsing non riuscito"
                [(program,rest)] -> 
                    if rest /= "" then 
                        putStrLn ("ERRORE: parsing non completo\nrimasto:\n"++rest)
                    else         
                        putStrLn (show program)
            putStrLn "----------FINE ALBERO--------"   

    printCFG:: Graph(String) -> IO()
    printCFG cfg =
        do 
            putStrLn "\n------------CFG--------------"
            putStrLn (show  cfg) 
            putStrLn "------------FINE CFG---------" 

    printDenRes:: Stm -> IO()
    printDenRes prTree= 
        do
            putStrLn "\n-------ANALISI DEN----------"
            putStrLn "DOMINIO: Segni"
            putStrLn  (show ((semS False signCondC prTree (S[]))::AbsState (Sign)))
            putStrLn "DOMINIO: Intervalli"
            putStrLn (show ((semS False intCondC prTree (S[]))::AbsState (Interval)))
            putStrLn "----------FINE ANALISI--------"

    printCFGRes:: CGraph(Sign) -> IO()
    printCFGRes graph= 
        do
            putStrLn "\n-------ANALISI CFG----------"
            putStrLn "DOMINIO: Segni"
            --putStrLn  (show (analyze graph [] (S[])))
            putStrLn "----------FINE ANALISI--------"