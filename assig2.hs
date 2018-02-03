
data ME = Num Int
          | Var Char
          | Group ME
          | Add ME ME
          | Sub ME ME
          | Mul ME ME
          | Power ME Int
          | Neg ME
          deriving (Show, Ord, Eq)

deriv :: ME -> Char -> ME

-- Placeholder
deriv m c = m

simplifyME :: ME -> ME

mkNum :: Int -> ME
mkVar :: Char -> ME
mkGroup :: ME -> ME
mkAdd :: ME -> ME -> ME
mkSub :: ME -> ME -> ME
mkMul :: ME -> ME -> ME
mkPower :: ME -> Int -> ME
mkNeg :: ME -> ME

simplifyME (Num i) = mkNum i
simplifyME (Var c) = mkVar c
simplifyME (Group e) = mkGroup (simplifyME e)
simplifyME (Add e1 e2) = mkAdd (simplifyME e1) (simplifyME e2)
simplifyME (Sub e1 e2) = mkSub (simplifyME e1) (simplifyME e2)
simplifyME (Mul e1 e2) = mkMul (simplifyME e1) (simplifyME e2)
simplifyME (Power e i) = mkPower (simplifyME e) i
simplifyME (Neg e) = mkNeg (simplifyME e)


--Placeholders
mkNum i = (Num i)
mkVar c = (Var c)
mkGroup e = (Group e)
mkAdd e1 e2 = (Add e1 e2)
mkSub e1 e2 = (Sub e1 e2)
mkMul e1 e2 = (Mul e1 e2)
mkPower e i = (Power e i)
mkNeg e = (Neg e)


unparseME :: ME -> [Char]

-- Placeholder
unparseME m = ""
