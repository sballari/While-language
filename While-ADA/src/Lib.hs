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
    import CondCFunSign
    import CondCFunInt

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

    printAdjList:: Graph(String) -> IO()
    printAdjList cfg =
        do 
            putStrLn "\n------------inAdjList--------------"
            putStrLn (show (in_adjs cfg)) 
            putStrLn "------------FINE inAdjList---------" 

    snToBool :: String -> Bool
    snToBool "s" = True
    snToBool "n" = False

    askIntInitialState :: [Name] -> IO(AbsState Interval)
    askIntInitialState vars = 
        do 
            putStrLn "\nSTATO INIZIALE INTERVALLI:"
            putStrLn "Usare stato iniziale tutto a IntervalTop ? [s/n]"
            getLine >>= \ans ->
                if (snToBool ans) then 
                    return (topVarsInit vars)
                else 
                    putStrLn "Fornire lo stato iniziale:">>
                    putStrLn "es:">>
                    putStrLn "S [(\"X\",Interval MinInf (B 100)),...] oppure Bottom">>
                    putStrLn "Intervalli: Interval Bound Bound">>
                    putStrLn "Bound: (B Int), MinInf, PlusInf">>
                    getLine >>= \s -> 
                        let 
                            is = read s 
                            topState = (topVarsInit vars)
                        in 
                        putStrLn ("stato iniziale: "++(show is))>>
                        return (is `AbsState.meet` topState) 


    askSignInitialState :: [Name] -> IO(AbsState Sign)
    askSignInitialState vars = 
        do 
            putStrLn "\nSTATO INIZIALE SEGNI:"
            putStrLn "Usare stato iniziale tutto a SignTop ? [s/n]"
            getLine >>= \ans ->
                if (snToBool ans) then 
                    return (topVarsInit vars)
                else 
                    putStrLn "Fornire lo stato iniziale:">>
                    putStrLn "es:">>
                    putStrLn "S[(\"X\",LessEqZero),...] oppure Bottom">>
                    putStrLn "segni: LessEqZero, MoreEqZero, Zero, SignTop, SignBottom" >>
                    getLine >>= \s -> 
                        let 
                            is = read s 
                            topState = (topVarsInit vars)
                        in 
                        putStrLn ("stato iniziale: "++(show is))>>
                        return (is `AbsState.meet` topState) 


    printDenRes:: Stm -> AbsState Sign -> AbsState Interval  -> [Name] -> IO()
    printDenRes prTree signIS intIS vars = 
        do
            putStrLnCBold "\n-------ANALISI DEN----------" Red
            putStrLnCBold "DOMINIO: Segni" Blue
            putStrLn "Usare widening ? [s/n]" 
            fmap (snToBool) getLine >>= \wideningS ->
                putStrLnResult  (show (semS wideningS signCondC prTree signIS)) >>

                putStrLnCBold "\nDOMINIO: Intervalli" Blue>> 
                putStrLn "Usare widening ? [s/n]" >>
                fmap (snToBool) getLine >>= \wideningI ->
                putStrLnResult (show (semS wideningI intCondC prTree intIS)) >>
                putStrLnCBold "----------FINE ANALISI--------" Red

    printCFGRes:: (AbsDomain a, Show a) => CGraph(a) -> AbsState a -> [Name] -> String -> IO()
    printCFGRes graph initialState vars domain_name= 
        do
            putStrLnCBold "\n-------ANALISI CFG----------" Red
            putStrLnCBold ("DOMINIO: "++domain_name) Blue
            putStrLn "mostrare iterazioni ? [s/n]" 
            fmap (snToBool) getLine >>= \verbose ->
                putStrLn "indicare punti di widening (es: [1,2,3,...]) :" >>
                fmap stringToWPoints getLine >>= \wideningPoints ->
                putStrLn ("WIDENING POINTS = "++ (show wideningPoints)) >>
                let fp_clm = analyze graph wideningPoints (topVarsInit vars) in 
                putStrLnResult ( if verbose then printClmSeq fp_clm
                                    else printClm (last fp_clm) ) >>
                putStrLnCBold "----------FINE ANALISI--------" Red
         
            
        
    stringToWPoints :: String -> [Label]       
    stringToWPoints str = foldr (\x sr -> (L x):sr ) [] arr
        where arr = (read str)::[Int]

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