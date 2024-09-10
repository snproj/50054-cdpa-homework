module SUTD.Compiler.RegExp where 

data RE = Choice RE RE 
    | Seq RE RE 
    | Star RE 
    | Epsilon 
    | Letter Char
    | Phi
    deriving (Show, Eq)


eps :: RE -> Bool
eps (Choice r1 r2) = eps r1 || eps r2
eps (Seq r1 r2) = eps r1 && eps r2
eps (Star _) = True
eps Epsilon = True
eps (Letter _) = False
eps Phi = False


deriv :: RE -> Char -> RE 
deriv Phi _ = Phi
deriv Epsilon _ = Phi
deriv (Letter l1) l2
    | l1 == l2 = Epsilon
    | otherwise = Phi
deriv (Choice r1 r2) l = Choice (deriv r1 l) (deriv r2 l)
deriv (Seq r1 r2) l
    | eps r1 = Choice (Seq (deriv r1 l) r2) (deriv r2 l)
    | otherwise = Seq (deriv r1 l) r2
deriv (Star r) l = Seq (deriv r l) (Star r)


wordMatch :: [Char] -> RE -> Bool 
wordMatch [] r = eps r
wordMatch (l:w) r = wordMatch w (deriv r l)
