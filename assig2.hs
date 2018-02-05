
data ME = Num Int
          | Var Char
          | Group ME
          | Add ME ME
          | Sub ME ME
          | Mul ME ME
          | Power ME Int
          | Neg ME
          deriving (Show, Ord, Eq)


-- Todo:
--   Finish deriv
deriv :: ME -> Char -> ME

deriv (Num n) x = (Num 0)

deriv (Var a) x
  | a==x = (Num 1)
  | otherwise = (Num 0)

deriv (Neg f) x = mkNeg (deriv f x)

deriv (Add f g) x = mkAdd (deriv f x) (deriv g x)

deriv (Sub f g) x = mkSub (deriv f x) (deriv g x)

deriv (Mul f g) x = (mkAdd (mkMul g (deriv f x)) (mkMul f (deriv g x)))

deriv (Power f n) x = mkMul (mkMul (Num n) (mkPower f (n-1))) (deriv f x)

deriv (Group f) x = deriv f x


-- Todo:
--   Add canonical form rules

mkNum :: Int -> ME
mkVar :: Char -> ME
mkGroup :: ME -> ME
mkAdd :: ME -> ME -> ME
mkSub :: ME -> ME -> ME
mkMul :: ME -> ME -> ME
mkPower :: ME -> Int -> ME
mkNeg :: ME -> ME

mkNum i = Num i

mkVar c = Var c

mkGroup e = Group e

mkAdd (Mul (Num i1) e1) (Mul (Num i2) e2)
    | e1 == e2      = mkMul (Num (i1 + i2)) e1
    | otherwise     = Add (mkMul (Num i1) e1) (mkMul (Num i2) e2)
mkAdd (Num i1) (Num i2) = Num (i1 + i2)
mkAdd e (Num 0) = e
mkAdd (Add e (Num i1)) (Num i2) = mkAdd e (Num (i1 + i2))
mkAdd e1 e2 = Add e1 e2

mkSub (Num i1) (Num i2) = Num (i1 - i2)
mkSub (Num 0) e = Neg e
mkSub e (Num 0) = e
mkSub (Sub e (Num i1)) (Num i2) = mkSub e (Num (i1 + i2))
mkSub e1 e2 = Sub e1 e2

mkMul (Power e1 i1) (Power e2 i2)
    | e1 == e2      = mkPower e1 (i1 + i2)
    | otherwise     = Mul (mkPower e1 i1) (mkPower e2 i2)
mkMul (Num i1) (Num i2) = Num (i1 * i2)
mkMul (Num 0) e = Num 0
mkMul (Num 1) e = e
mkMul e1 e2 = Mul e1 e2

mkPower (Num i1) i2 = Num (i1 ^ i2)
mkPower e 0 = Num 1
mkPower e 1 = e
mkPower e i = Power e i

mkNeg e = Neg e


simplifyME :: ME -> ME

simplifyME (Num n) = mkNum n
simplifyME (Var c) = mkVar c
simplifyME (Group e) = mkGroup (simplifyME e)
simplifyME (Add e1 e2) = mkAdd (simplifyME e1) (simplifyME e2)
simplifyME (Sub e1 e2) = mkSub (simplifyME e1) (simplifyME e2)
simplifyME (Mul e1 e2) = mkMul (simplifyME e1) (simplifyME e2)
simplifyME (Power e i) = mkPower (simplifyME e) i
simplifyME (Neg e) = mkNeg (simplifyME e)


-- Todo:
--   Implement unparse
unparseME :: ME -> [Char]

unparseME m = ""
