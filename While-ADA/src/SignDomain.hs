{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module SignDomain where
    import AbsDomain as AD
    import WhileStructures

    data Sign = SignBottom | Zero | LessEqZero | MoreEqZero | SignTop 
                deriving (Eq)

    instance Show Sign where
        show SignBottom  = "\8869"++"sign"
        show Zero        = "0"
        show LessEqZero  = '\8804':"0"
        show MoreEqZero  =  '\8805': "0"
        show SignTop     =  '\8868':"sign"


    instance AbsDomain Sign where
        top = SignTop
        bottom = SignBottom
  
        soundC n
            |n == 0 = Zero
            |n >= 0 = MoreEqZero
            |n Prelude.<= 0 = LessEqZero
        
        soundRange (n, n')
            |n == 0 = MoreEqZero
            |n Prelude.<= 0 && n' Prelude.<= 0 = LessEqZero
            |n Prelude.<= 0 && n' >= 0 = SignTop
            |n >= 0 && n' >= 0 = MoreEqZero
            |n >= 0 && n' Prelude.<= 0 = SignBottom
          
        (<=) _ SignTop = True
        (<=) SignTop _ = False
        (<=) LessEqZero MoreEqZero = False
        (<=) _ MoreEqZero = True
        (<=) MoreEqZero LessEqZero = False
        (<=) _ LessEqZero = True
        (<=) SignBottom Zero = True
        (<=) Zero Zero = True
        (<=) _ Zero = False
        (<=) Zero SignBottom = False
        (<=) SignBottom _ = True
        (<=) _ SignBottom = False
                
        --join :: Sign -> Sign -> Sign
        join x y = lub x y
            where 
                lub a b  
                    |(AD.<=) a b == True = b
                    |(AD.<=) a b == False && (AD.<=) b a == True = a 
                    |otherwise = SignTop -- inconfrontabili

        --meet :: Sign -> Sign -> Sign
        meet x y= glb x y
            where
                glb a b
                    | (AD.<=) a b == True = a
                    | (AD.<=) a b == False && (AD.<=) b a == True = b 
                    | otherwise = SignBottom -- inconfrontabili

        --UNARY OPERATORS
        --absMinus :: a -> a
        absMinus SignBottom = SignBottom
        absMinus Zero = Zero
        absMinus LessEqZero = MoreEqZero
        absMinus MoreEqZero = LessEqZero
        absMinus SignTop = SignTop

        --BINARY OPERATORS 
        --absSum :: a -> a -> a
        absSum SignBottom _ = SignBottom
        absSum _ SignBottom = SignBottom
        absSum SignTop _ = SignTop 
        absSum _ SignTop = SignTop
        absSum Zero MoreEqZero = MoreEqZero
        absSum Zero LessEqZero = LessEqZero
        absSum Zero Zero = Zero 
        absSum LessEqZero MoreEqZero = SignTop
        absSum LessEqZero LessEqZero = LessEqZero
        absSum LessEqZero Zero = LessEqZero
        absSum MoreEqZero MoreEqZero = MoreEqZero
        absSum MoreEqZero LessEqZero = SignTop
        absSum MoreEqZero Zero = MoreEqZero
        --absMul :: a -> a -> a  
        absMul SignBottom _ = SignBottom
        absMul _ SignBottom = SignBottom
        absMul Zero _ = Zero
        absMul _ Zero = Zero
        absMul SignTop _ = SignTop 
        absMul _ SignTop = SignTop
        absMul LessEqZero MoreEqZero = LessEqZero
        absMul LessEqZero LessEqZero = MoreEqZero        
        absMul MoreEqZero MoreEqZero = MoreEqZero
        absMul MoreEqZero LessEqZero = LessEqZero 
        --absDiv :: a -> a -> a  
        absDiv SignBottom _ = SignBottom
        absDiv _ SignBottom = SignBottom
        absDiv Zero SignTop = Zero
        absDiv _ SignTop = SignTop
        absDiv _ Zero = SignBottom 
        absDiv SignTop _ = SignTop
        absDiv Zero _ = Zero   
        absDiv LessEqZero MoreEqZero = LessEqZero
        absDiv LessEqZero LessEqZero = MoreEqZero        
        absDiv MoreEqZero MoreEqZero = MoreEqZero
        absDiv MoreEqZero LessEqZero = LessEqZero 
        
        
        -- widening :: Sign -> Sign -> Sign
        widening x1 x2 = if x2 AD.<= x1 == True then x1 else SignTop -- naive
