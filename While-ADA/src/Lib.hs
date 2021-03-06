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
    import KarrDomain
    import AbsDenSemR
    import CondCFunInt


    mainRoutine :: Stm -> [String]->IO()
    mainRoutine tree vars =
        putStrLn "\nScegliere il tipo di dominio :\n1) non relazionale;\n2) relazionale" >>
        getLine >>= \ans -> case ans of
            "2" -> routine_relazionale  tree vars
            "1" -> routine_non_relazionale tree vars
            otherwise -> putStrLn "\nselezione errata: inserire 1 o 2"

    routine_relazionale :: Stm -> [String]-> IO()
    routine_relazionale tree vars = 
        do
            putStrLnCBold "\n-------ANALISI DEN----------" Red
            putStrLnCBold "DOMINIO: Karr" Blue
            putStrLn "widening = join "
            initSys <- askKarrInitialState vars
            putStrLnResult  ( show (AbsDenSemR.semS False tree  initSys))
            putStrLnCBold "----------FINE ANALISI--------" Red



    askKarrInitialState :: [String] -> IO(EQs)
    askKarrInitialState vars = 
         do 
            putStrLn "\nSTATO INIZIALE - DOMINIO DI KARR:"
            putStrLn "Inizializzare lo stato a IntervalTop ? [s/n]"
            getLine >>= \ans ->
                if (snToBool ans) then 
                    return (EQs ([],[], vars))
                else 
                    putStrLn "Fornire lo stato iniziale:">>
                    putStrLn "es:">>
                    putStrLn "([[1,0],[1,0]],[1,1]) oppure EQsBottom">>
                    putStrLn ("L'ordine delle variabile e' "++(show vars)++".")>>
                    getLine >>= \s -> 
                        let 
                            (coef,b) = read s 
                            is = EQs (coef,b,vars)
                        in 
                        putStrLn ("stato iniziale: "++(show is))>>
                        return is
    routine_non_relazionale :: Stm -> [String] -> IO()
    routine_non_relazionale tree vars= 
            let 
                
                (sign_cfg,r) = (app (createCFG signCondC tree) 1 )::(CGraph (Sign),Int)
                (int_cfg,r') = (app (createCFG intCondC tree) 1 )::(CGraph (Interval),Int)
                in 
                
                do 
                    signIS <- askSignInitialState vars
                    intIS <- askIntInitialState vars
                    printDenRes tree signIS intIS vars 
                    printCFGRes sign_cfg signIS vars "Segni"
                    printCFGRes int_cfg intIS vars "Intervalli"

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
            putStrLn "\nSTATO INIZIALE - DOMINIO DEGLI INTERVALLI:"
            putStrLn "Inizializzare lo stato a IntervalTop ? [s/n]"
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
            putStrLn "\nSTATO INIZIALE - DOMINIO DEI SEGNI:"
            putStrLn "Inizializzare lo stato a SignTop ? [s/n]"
            getLine >>= \ans ->
                if (snToBool ans) then 
                    return (topVarsInit vars)
                else 
                    putStrLn "Fornire lo stato iniziale:">>
                    putStrLn "es:">>
                    putStrLn "S[(\"X\",LessEqZero),...] oppure Bottom">>
                    putStrLn "Segni: LessEqZero, MoreEqZero, Zero, SignTop, SignBottom" >>
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
            putStrLn "widening ? [s/n]" 
            fmap (snToBool) getLine >>= \wideningS ->
                putStrLnResult  (show (AbsDenSem.semS wideningS signCondC prTree signIS)) >>

                putStrLnCBold "\nDOMINIO: Intervalli" Blue>> 
                putStrLn "widening ? [s/n]" >>
                fmap (snToBool) getLine >>= \wideningI ->
                putStrLnResult (show (AbsDenSem.semS wideningI intCondC prTree intIS)) >>
                putStrLnCBold "----------FINE ANALISI--------" Red

    printCFGRes:: (AbsDomain a, Show a) => CGraph(a) -> AbsState a -> [Name] -> String -> IO()
    printCFGRes graph initialState vars domain_name= 
        do
            putStrLnCBold "\n-------ANALISI CFG----------" Red
            putStrLnCBold ("DOMINIO: "++domain_name) Blue
            putStrLn "mostrare le iterazioni ? [s/n]" 
            fmap (snToBool) getLine >>= \verbose ->
                putStrLn "indicare i punti di widening (es: [1,2,3,...]) :" >>
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