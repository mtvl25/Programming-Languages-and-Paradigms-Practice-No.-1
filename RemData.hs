{-|
  Función: min2
  Descripción: Devuelve el menor de dos enteros.
  Entradas: a :: Int, b :: Int
  Salidas:  Int (el menor entre a y b)
-}
min2 :: Int -> Int -> Int
min2 a b = if a <= b then a else b

{-|
  Función: max2
  Descripción: Devuelve el mayor de dos enteros.
  Entradas: a :: Int, b :: Int
  Salidas:  Int (el mayor entre a y b)
-}
max2 :: Int -> Int -> Int
max2 a b = if a <= b then b else a

{-|
  Función: enRangoCerrado
  Descripción: Indica si x pertenece al intervalo cerrado [lo, hi].
  Entradas: x :: Int, lo :: Int, hi :: Int
  Salidas:  Bool (True si lo <= x <= hi; False en otro caso)
-}
enRangoCerrado :: Int -> Int -> Int -> Bool
enRangoCerrado x lo hi =
  if lo <= x then
    if x <= hi then True else False
  else False

{-|
  Función: remData
  Descripción: Elimina de la lista todos los elementos que NO estén dentro del
               intervalo cerrado definido por li y hs. Soporta que el intervalo
               venga invertido (li > hs).
  Entradas: lista :: [Int], li :: Int, hs :: Int
  Salidas:  [Int] con solo los elementos x tales que min(li,hs) <= x <= max(li,hs)
  Ejemplo:
    remData [1, 25, 5, -4] 0 5  ==>  [1, 5]
-}
remData :: [Int] -> Int -> Int -> [Int]
remData lista li hs = remDataNorm lista (min2 li hs) (max2 li hs)

{-|
  Función: remDataNorm (auxiliar)
  Descripción: Igual que remData, pero recibe el intervalo ya ordenado.
  Entradas: lista :: [Int], low :: Int, high :: Int  (con low <= high)
  Salidas:  [Int] filtrada en [low, high]
-}
remDataNorm :: [Int] -> Int -> Int -> [Int]
remDataNorm [] _ _ = []
remDataNorm (x:xs) low high =
  if enRangoCerrado x low high
     then x : remDataNorm xs low high   -- conservo x
     else      remDataNorm xs low high  -- descarto x
