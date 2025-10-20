module Utilidades where

-- Devuelve todas las combinaciones entre pares de elementos de la lista sin duplicados y sin importar el orden
combinaciones :: [a] -> [(a, a)]
combinaciones [] = []
combinaciones (x:xs) = [(x, y) | y <- xs] ++ combinaciones xs

replaceItem :: Int -> a -> [a] -> [a]
replaceItem _ _ [] = []
replaceItem 0 newVal (_:xs) = newVal : xs
replaceItem n newVal (x:xs) = x : replaceItem (n-1) newVal xs
