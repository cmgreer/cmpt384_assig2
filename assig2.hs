
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
deriv m = m

simplifyME :: ME -> ME

-- Placeholder
simplifyME m = m

unparseME :: ME -> [Char]

-- Placeholder
unparseME m = ""
