{-|
  Función: insertDesc
  Descripción: Inserta un elemento en una lista de Floats manteniendo el orden descendente.
  Entradas: x :: Float, xs :: [Float]
  Salidas:  [Float] (lista con x insertado en orden descendente)
-}
insertDesc :: Float -> [Float] -> [Float]
insertDesc x [] = [x]
insertDesc x (y:ys) =
  if x >= y
     then x : y : ys
     else y : insertDesc x ys

{-|
  Función: orderDesc
  Descripción: Ordena una lista de Floats en orden descendente usando inserción.
  Entradas: xs :: [Float]
  Salidas:  [Float] (lista ordenada de mayor a menor)
  Ejemplo:
    orderDesc [1, 25, 5, -4]  ==>  [25, 5, 1, -4]
-}
orderDesc :: [Float] -> [Float]
orderDesc [] = []
orderDesc (x:xs) = insertDesc x (orderDesc xs)
