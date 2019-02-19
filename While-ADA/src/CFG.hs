module CFG where 
    import WhileStructures


    newtype Label = Label Int deriving Show

    -- cfg :: Stm -> [(Label, )]