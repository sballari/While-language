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
    import AbsDomain

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

    printLabCode :: Stm -> IO ()
    printLabCode tree = 
        do
            putStrLn "\n-----------LABELLED CODE------------"
            putStrLn ( printLabProg (fst (app (labelled tree) 1)) )
            putStrLn "----------FINE ALBERO--------"  
    
    printCFG:: Graph(String) -> IO()
    printCFG cfg =
        do 
            putStrLn "\n------------CFG--------------"
            putStrLn (show  cfg) 
            putStrLn "------------FINE CFG---------" 

    printDenRes:: Stm -> [Name] -> IO()
    printDenRes prTree vars= 
        do
            putStrLn "\n-------ANALISI DEN----------"
            putStrLn "DOMINIO: Segni"
            putStrLn  (show (semS False signCondC prTree stateSign))
            putStrLn "DOMINIO: Intervalli"
            putStrLn (show (semS True intCondC prTree stateInt))
            putStrLn "----------FINE ANALISI--------"
        where 
            stateSign = (topVarsInit vars) ::AbsState (Sign)
            stateInt =  (topVarsInit vars) ::AbsState (Interval)

    printCFGRes:: (AbsDomain a, Show a) => CGraph(a)  -> [Name] -> IO()
    printCFGRes graph vars= 
        do
            putStrLn "\n-------ANALISI CFG----------"
            putStrLn  (printClm fp_clm)
            putStrLn "----------FINE ANALISI--------"
        where 
            fp_clm = analyze graph [L 6] (topVarsInit vars)
        
    topVarsInit :: AbsDomain a => [Name] -> AbsState a
    topVarsInit vars = foldr (\x st -> alter st x top) (S []) vars 

    printClm :: Show a => Clm a -> String
    printClm = foldr (\(l,val) sr -> (show l)++": "++(show val)++"\n"++sr  ) ""