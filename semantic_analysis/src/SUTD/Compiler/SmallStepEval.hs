module SUTD.Compiler.SmallStepEval where 


import Control.Monad.Except
import SUTD.Compiler.Util
import SUTD.Compiler.LambdaCalculus hiding ((-))


-- | implementing small step operational semantics for lambda calculus
evalOneStep :: Term -> StateResult StateInfo Term
evalOneStep (AppTerm (LambdaTerm x t1) t2) = do 
    t3 <- appSubst (x, t2) t1 
    return t3
evalOneStep (AppTerm t1 t2) = do 
    t3 <- evalOneStep t1 
    return (AppTerm t3 t2)
evalOneStep (ConstTerm c) = return (ConstTerm c)
-- fixme 
evalOneStep t@(FixTerm (LambdaTerm f t1)) = do 
    t2 <- appSubst (f,t) t1
    return t2 
evalOneStep (FixTerm t) = do 
    t' <- evalOneStep t
    return (FixTerm t')
evalOneStep (IfTerm (ConstTerm (BoolConst True)) t2 t3) = return t2
evalOneStep (IfTerm (ConstTerm (BoolConst False)) t2 t3) = return t3
evalOneStep (IfTerm t1 t2 t3) = do 
    t1' <- evalOneStep t1 
    return (IfTerm t1' t2 t3) 
evalOneStep t@(LambdaTerm x body) = return t
evalOneStep (LetTerm x t1 t2) = do 
    t2' <- appSubst (x, t1) t2 
    return t2' 
evalOneStep (OpTerm (ConstTerm (IntConst i)) Plus (ConstTerm (IntConst j))) = return (ConstTerm (IntConst (i + j)))
evalOneStep (OpTerm (ConstTerm (IntConst i)) Minus (ConstTerm (IntConst j))) = return (ConstTerm (IntConst (i - j)))
evalOneStep (OpTerm (ConstTerm (IntConst i)) Mult (ConstTerm (IntConst j))) = return (ConstTerm (IntConst (i * j)))
evalOneStep (OpTerm (ConstTerm (IntConst i)) Div (ConstTerm (IntConst 0))) = throwError "division by zero error."
evalOneStep (OpTerm (ConstTerm (IntConst i)) Div (ConstTerm (IntConst j))) = return (ConstTerm (IntConst (i `div` j)))
evalOneStep (OpTerm (ConstTerm (IntConst i)) DEqual (ConstTerm (IntConst j))) = return (ConstTerm (BoolConst (i == j)))
evalOneStep (OpTerm (ConstTerm (BoolConst i)) DEqual (ConstTerm (BoolConst j))) = return (ConstTerm (BoolConst (i == j)))
evalOneStep (OpTerm (ConstTerm (BoolConst i)) _ (ConstTerm (BoolConst j))) = throwError "type error."
evalOneStep (OpTerm (ConstTerm c1) op t2) = do 
    t2' <- evalOneStep t2
    return (OpTerm (ConstTerm c1) op t2')
evalOneStep (OpTerm t1 op t2) = do 
    t1' <- evalOneStep t1 
    return (OpTerm t1' op t2) 
evalOneStep t@(VarTerm x) = return t


eval :: Term -> StateResult StateInfo Value
eval t = do 
    t' <- evalOneStep t 
    case t' of 
        ConstTerm c   -> return (ConstValue c)
        _ | t' == t   -> throwError "evaluation is stuck." 
          | otherwise -> eval t'  
