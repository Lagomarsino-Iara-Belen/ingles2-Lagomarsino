data NExp = Var Variable
          | NCte Int
          | NBOp NBinOp NExp NExp
    deriving Show 

data NBinOp = Add | Sub | Mul | Div | Mod | Pow
    deriving Show

type Variable = String

--Ejemplos de como funciona el tipo
nexp1 = NBOp Add (NCte 5)
                 (Var "x")

nexp2 = NBOp Add (NCte 5)
                 (NCte 6) 

nexp3 = NBOp Add (NCte 5)
                 ( NBOp Mul (Var "x")  
                            (Var "y"))              



--cfNE :: NExp -> NExp ==> Devuelve una NExp sin expresiones optimizables 
-- Nota: le doy una expresion y la devuelve toda optimizada (simplificada)
cfNE :: NExp -> NExp
cfNE (Var st)           = (Var st)
cfNE (NCte n)           = (NCte n)
cfNE (NBOp bop ne1 ne2) = reducir bop (cfNE ne1) (cfNE ne2)

reducir :: NBinOp -> NExp -> NExp -> NExp
reducir bop (NCte n) (NCte m) = NCte (evalNBinOp bop n m)
reducir bop  ex1      ex2     = NBOp bop ex1 ex2

evalNBinOp :: NBinOp -> Int -> Int -> Int
evalNBinOp Add  = (+) 
evalNBinOp Sub  = (-) 
evalNBinOp Mul  = (*) 
evalNBinOp Div  = div 
evalNBinOp Mod  = mod 
evalNBinOp Pow  = (^) 
