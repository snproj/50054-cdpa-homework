module SUTD.Compiler.SmallStepEval where


import Control.Monad.Except
import SUTD.Compiler.Util
import SUTD.Compiler.LambdaCalculus hiding ((-))

-- | task 1
-- | implementing small step operational semantics for lambda calculus
evalOneStep :: Term -> StateResult StateInfo Term
evalOneStep (AppTerm (LambdaTerm x t1) t2) = do -- beta reduction
    t3 <- appSubst (x, t2) t1
    return t3
evalOneStep (AppTerm t1 t2) = do -- NOR
    t3 <- evalOneStep t1
    return (AppTerm t3 t2)
evalOneStep (ConstTerm c) = return (ConstTerm c) -- ??
evalOneStep (IfTerm (ConstTerm (BoolConst True)) t2 _) = return t2 -- ifTrue
evalOneStep (IfTerm (ConstTerm (BoolConst False)) _ t3) = return t3 -- ifFalse
evalOneStep (IfTerm t1 t2 t3) = do -- ifI
    t'1 <- evalOneStep t1
    return (IfTerm t'1 t2 t3)
evalOneStep (OpTerm (ConstTerm (IntConst c1)) op (ConstTerm (IntConst c2))) = -- OpC
    return (ConstTerm (case op of
        DEqual ->  BoolConst (c1 == c2)
        _ -> IntConst (case op of
            Plus -> c1 + c2
            Minus -> c1 - c2
            Mult -> c1 * c2
            Div -> c1 `div` c2)))
evalOneStep (OpTerm (ConstTerm c1) op t1) = do -- OpI2
    t'1 <- evalOneStep t1
    return (OpTerm (ConstTerm c1) op t'1)
evalOneStep (OpTerm t1 op t2) = do -- OpI1
    t'1 <- evalOneStep t1
    return (OpTerm t'1 op t2)
evalOneStep (LetTerm x t1 t2) = do -- Let
    t3 <- appSubst (x, t1) t2
    return t3
evalOneStep (FixTerm (LambdaTerm f t)) = do -- Fix2
    t2 <- appSubst (f, (FixTerm (LambdaTerm f t))) t
    return t2
evalOneStep (FixTerm t) = do -- Fix1
    t' <- evalOneStep t
    return (FixTerm t')

evalOneStep _ = undefined



eval :: Term -> StateResult StateInfo Value
eval t = do
    t' <- evalOneStep t
    case t' of
        ConstTerm c   -> return (ConstValue c)
        _ | t' == t   -> throwError "evaluation is stuck."
          | otherwise -> eval t'
