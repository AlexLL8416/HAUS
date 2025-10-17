module Utilidades where

-- Devuelve todas las combinaciones entre pares de elementos de la lista sin duplicados y sin importar el orden
combinaciones :: [a] -> [(a, a)]
combinaciones [] = []
combinaciones (x:xs) = [(x, y) | y <- xs] ++ combinaciones xs
