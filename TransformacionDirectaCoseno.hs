--Porgrama para calcular la DCT 
--(Transformación discreta del coseno)
-- Hecho por: María Paula Gómez --

module Main where 

-- Constante π (Para no usar el "pi" del Predule) --
miPi :: Double 
miPi = 3.141592653589793

-------------------------------------------------------
-- Valor absoluto --
-- Retorna el valor absoluto de un número real --
-- Entrada   : x :: Double --
-- Salida    : |x| :: Double --

valorAbsoluto :: Double -> Double 
valorAbsoluto x = if x < 0 then (-x) else x 

--------------------------------------------------------
-- Raiz cuadrada --
-- Aproxima sqrt(a) Newton - Raphson --
-- Entrada   : a :: Double (a >= 0) --
-- Salida    : sqrt(a) :: Double --
-- Sin usar sqrt del Prelude (no permitido).
-- Se itera hasta que el error sea menor a 1e-12
-- o se alcance un número razonable de iteraciones.--

raizCuadrada :: Double -> Double
raizCuadrada a =
  if a == 0 then 0
  else iterar (if a < 1 then 1 else a / 2) 0
  where
    margenError = 1e-12
    maxIter = 200 :: Int

    -- iterar y,k: "y" es la aproximación actual, "k" el número de pasos
    iterar :: Double -> Int -> Double
    iterar y k =
      let y'  = 0.5 * (y + a / y)
          err = valorAbsoluto (y' * y' - a)
      in if (err < margenError) || (k >= maxIter)
            then y'
            else iterar y' (k + 1)
---------------------------------------------------------
-- longitud
-- Propósito  : Calcula la cantidad de elementos de una lista.
-- Entrada    : xs :: [a]
-- Salida     : n :: Int

longitud :: [a] -> Int
longitud []     = 0
longitud (_:xs) = 1 + longitud xs

---------------------------------------------------------
-- rango
-- Propósito  : Genera la lista [0,1,2,...,n-1] sin usar [0..].
-- Entrada    : n :: Int (n >= 0)
-- Salida     : [Int]

rango :: Int -> [Int]
rango n = construir 0 n
  where
    construir i m =
      if i >= m then []
      else i : construir (i + 1) m

---------------------------------------------------------
-- enumerarDesde
-- Propósito  : Asocia índice con cada elemento: [(i, xi)].
-- Entradas   : i0 :: Int índice inicial, xs :: [a]
-- Salida     : [(Int, a)]

enumerarDesde :: Int -> [a] -> [(Int, a)]
enumerarDesde _ []     = []
enumerarDesde i (x:xs) = (i, x) : enumerarDesde (i + 1) xs

---------------------------------------------------------
-- mapear
-- Propósito  : Versión propia de map.
-- Entradas   : f :: (a -> b), xs :: [a]
-- Salida     : [b]

mapear :: (a -> b) -> [a] -> [b]
mapear _ []     = []
mapear f (x:xs) = f x : mapear f xs

---------------------------------------------------------
-- sumatoria
-- Propósito  : Suma todos los elementos de una lista de Doubles.
-- Entrada    : xs :: [Double]
-- Salida     : suma :: Double

sumatoria :: [Double] -> Double
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs

---------------------------------------------------------
-- coefA
-- Propósito  : Calcula el factor a(k) de la DCT.
-- Entradas   : n :: Int (tamaño N de la señal), k :: Int
-- Salida     : a(k) :: Double
-- Definición : a(0) = sqrt(1/N), a(k>0) = sqrt(2/N)

coefA :: Int -> Int -> Double
coefA n k =
  let nD = fromIntegral n
      base = if k == 0 then (1.0 / nD) else (2.0 / nD)
  in raizCuadrada base

---------------------------------------------------------
-- terminoDCT
-- Propósito  : Calcula el término x(n) * cos(((n+0.5)*π*k)/N).
-- Entradas   : nTotal :: Int (N), k :: Int, par :: (Int, Double)=(n,xn)
-- Salida     : término :: Double

terminoDCT :: Int -> Int -> (Int, Double) -> Double
terminoDCT nTotal k (n, xn) =
  let nD = fromIntegral n
      kD = fromIntegral k
      nTotD = fromIntegral nTotal
      argumento = ((nD + 0.5) * miPi * kD) / nTotD
  in xn * cos argumento

---------------------------------------------------------
-- calcularXk
-- Propósito  : Calcula X(k) para una señal dada.
-- Entradas   : xs :: [Double] (señal), k :: Int
-- Salida     : X(k) :: Double

calcularXk :: [Double] -> Int -> Double
calcularXk xs k =
  let n = longitud xs
      pares = enumerarDesde 0 xs
      suma = sumatoria (mapear (terminoDCT n k) pares)
  in coefA n k * suma

---------------------------------------------------------
-- dct
-- Propósito  : Calcula la DCT-II (vector X) de una señal x.
-- Entrada    : xs :: [Double] con N elementos.
-- Salida     : lista [X(0), X(1), ..., X(N-1)] :: [Double]

dct :: [Double] -> [Double]
dct xs =
  let n = longitud xs
      ks = rango n
  in mapear (calcularXk xs) ks
---------------------------------------------------------
-- main: Demostración con el ejemplo del enunciado
-- (el IO no está restringido por la normativa de funciones
-- matemáticas; se usa sólo para mostrar resultados).

main :: IO ()
main = do
  let ejemplo = [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]
  putStrLn "DCT de [1..10]:"
  print (dct ejemplo)
