module Lib where

    import System.IO
    import System.Console.ANSI
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
            putStrLnCBold "\n-----------ALBERO------------" Red
            case resultP of
                [] -> putStrLn "ERRORE: parsing non riuscito"
                [(program,rest)] -> 
                    if rest /= "" then 
                        putStrLn ("ERRORE: parsing non completo\nrimasto:\n"++rest)
                    else         
                        putStrLn (show program)
            putStrLnCBold "----------FINE ALBERO--------" Red

    printLabCode :: Stm -> IO ()
    printLabCode tree = 
        do
            putStrLnCBold "\n-----------LABELLED CODE------------" Red
            putStrLn ( printLabProg (fst (app (labelled tree) 1)) )
            putStrLnCBold "----------FINE LABELLED CODE--------" Red
    
    printCFG:: Graph(String) -> IO()
    printCFG cfg =
        do 
            putStrLn "\n------------CFG--------------"
            putStrLn (show  cfg) 
            putStrLn "------------FINE CFG---------" 


    snToBool :: String -> Bool
    snToBool "s" = True
    snToBool "n" = False

    printDenRes:: Stm -> [Name] -> IO()
    printDenRes prTree vars = 
        do
            putStrLnCBold "\n-------ANALISI DEN----------" Red
            putStrLnCBold "DOMINIO: Segni" Blue
            putStrLn "Usare widening ? [s/n]" 
            fmap (snToBool) getLine >>= \wideningS ->
                putStrLnResult  (show (semS wideningS signCondC prTree stateSign)) >>

                putStrLnCBold "\nDOMINIO: Intervalli" Blue>> 
                putStrLn "Usare widening ? [s/n]" >>
                fmap (snToBool) getLine >>= \wideningI ->
                putStrLnResult (show (semS wideningI intCondC prTree stateInt)) >>
                putStrLnCBold "----------FINE ANALISI--------" Red
        where 
            stateSign = (topVarsInit vars) ::AbsState (Sign)
            stateInt =  (topVarsInit vars) ::AbsState (Interval)

    printCFGRes:: (AbsDomain a, Show a) => CGraph(a)  -> [Name] -> String -> IO()
    printCFGRes graph vars domain_name= 
        do
            putStrLnCBold "\n-------ANALISI CFG----------" Red
            putStrLnCBold ("DOMINIO: "++domain_name) Blue
            putStrLn "mostrare iterazioni ? [s/n]" 
            fmap (snToBool) getLine >>= \verbose ->
                putStrLn "indicare punti di widening (es: [1,2,3,...]) :" >>
                fmap stringToWPoints getLine >>= \wideningPoints ->
                let fp_clm = analyze graph wideningPoints (topVarsInit vars) in 
                putStrLnResult ( if verbose then printClmSeq fp_clm
                                    else printClm (last fp_clm) ) >>
                putStrLnCBold "----------FINE ANALISI--------" Red
         
            
        
    stringToWPoints :: String -> [Label]       
    stringToWPoints str = foldr (\x sr -> (L x):sr ) [] arr
        where arr = (read str)::[Int]

    topVarsInit :: AbsDomain a => [Name] -> AbsState a
    topVarsInit vars = foldr (\x st -> alter st x top) (S []) vars 

    printClm :: Show a => Clm a -> String
    printClm = foldr (\(l,val) sr -> (show l)++": "++(show val)++"\n"++sr  ) ""


    printClmSeq :: Show a => [Clm a] -> String
    printClmSeq clms =
        "\nVERBOSE ITERATION:\n" ++
        (foldr (\col sr -> (printClm col)++"\n" ++ sr) "" clms) ++
        "FINE VERBOSE ITERATION\n"

    putStrLnCBold :: String -> Color -> IO()
    putStrLnCBold str color= 
        do
            setSGR [ SetConsoleIntensity BoldIntensity
                , SetColor Foreground Vivid color
                ]           
            putStrLn str
            setSGR [ Reset ]

    putStrLnResult :: String -> IO()
    putStrLnResult str= 
        do
            setSGR [ SetConsoleIntensity BoldIntensity
                , SetColor Foreground Vivid Red
                , SetColor Background Dull Yellow
                ]           
            putStrLn str
            setSGR [ Reset ]