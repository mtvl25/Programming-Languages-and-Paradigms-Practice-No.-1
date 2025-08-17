-- NumericalSeries.hs
-- Aproximación de e^x y cos(x) usando series; sin I/O.

-----------------------------------------------------------------------
-- | isEven
-- ¿Qué hace? Determina si un número entero es par.
-- Entradas: n :: Int
-- Salidas:  Bool
-----------------------------------------------------------------------
isEven :: Int -> Bool
isEven n = (n `mod` 2) == 0

-----------------------------------------------------------------------
-- | altSign
-- ¿Qué hace? Devuelve (-1)^n para el signo alternante.
-- Entradas: n :: Int
-- Salidas:  Double — 1.0 si n par, -1.0 si n impar.
-----------------------------------------------------------------------
altSign :: Int -> Double
altSign n = if isEven n then 1.0 else (-1.0)

-----------------------------------------------------------------------
-- | pow
-- ¿Qué hace? Calcula x^n con multiplicaciones repetidas.
-- Entradas: x :: Double (base), n :: Int (exponente; puede ser negativo).
-- Salidas:  Double — x^n
-----------------------------------------------------------------------
pow :: Double -> Int -> Double
pow x n
  | n == 0    = 1.0
  | n < 0     = 1.0 / pow x ((-1) * n)
  | otherwise = loop n 1.0
  where
    loop k acc =
      if k == 0 then acc
      else loop (k - 1) (acc * x)

-----------------------------------------------------------------------
-- | factD
-- ¿Qué hace? Calcula n! como Double (definido para n >= 0).
-- Entradas: n :: Int
-- Salidas:  Double — factorial de n
-----------------------------------------------------------------------
factD :: Int -> Double
factD n
  | n <= 1    = 1.0
  | otherwise = loop n 1.0
  where
    loop k acc =
      if k <= 1 then acc
      else loop (k - 1) (acc * fromIntegral k)

-----------------------------------------------------------------------
-- | expSeries
-- ¿Qué hace? Aproxima e^x con N términos: sum_{n=0}^{N-1} x^n / n!
-- Entradas: x :: Double, nTerms :: Int
-- Salidas:  Double
-----------------------------------------------------------------------
expSeries :: Double -> Int -> Double
expSeries x nTerms =
  if nTerms <= 0 then 0.0 else go 0 0.0
  where
    go n acc =
      if n >= nTerms
        then acc
        else
          let term = (pow x n) / factD n
          in go (n + 1) (acc + term)

-----------------------------------------------------------------------
-- | cosSeries
-- ¿Qué hace? Aproxima cos(x) con N términos:
--            sum_{n=0}^{N-1} (-1)^n * x^(2n) / (2n)!
-- Entradas: x :: Double, nTerms :: Int
-- Salidas:  Double
-----------------------------------------------------------------------
cosSeries :: Double -> Int -> Double
cosSeries x nTerms =
  if nTerms <= 0 then 0.0 else go 0 0.0
  where
    go n acc =
      if n >= nTerms
        then acc
        else
          let numPow = pow x (2 * n)
              denom  = factD (2 * n)
              term   = altSign n * numPow / denom
          in go (n + 1) (acc + term)
main :: IO ()
main = do
  let x = 1.0
      n = 10
  putStrLn "Aproximaciones:"
  putStrLn "Funcion Exponencial:" 
  print (expSeries x n)       -- e^1 con 10 términos
  ----------
  putStrLn "Funcion Coseno:"
  print (cosSeries x n)       -- cos(1) con 10 términos
