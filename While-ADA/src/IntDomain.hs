module IntDomain where
    import AbsDomain
    
    

    instance AbsDomain Int where
        (<=) a b = a Prelude.<= b
        bottom = 0
        top = 100
        union a b= a+b 
        intersection a b = a*b
        -- abstract operator 
        absSum a b = a+b
        absMul a b = a*b
        absDiv a b = a `div` b
        absMinus a = -a
        omega x = 21127
        
    

    