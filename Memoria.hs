-- 4. Sistema de Memoria para Agentes

-- Los agentes necesitan una memoria (implementada como diccionario) para tomar
--  decisiones inteligentes. Crea un tipo de datos flexible que pueda almacenar
--   diferentes tipos de información: enteros, cadenas de texto, puntos/coordenadas, booleanos, etc.

-- MÓDULOS IMPORTADOS (SU USO SE EXPLICA ABAJO)

module Memoria where

import qualified Data.Map as M
import Data.Typeable (Typeable, cast)

-- ============================================================
-- || ALTERNATIVA 1: MEMORIA FLEXIBLE CON TIPOS RESTRINGIDOS ||
-- ============================================================
  
-- Los diccionarios en Haskell se implementan con el tipo Map (de Data.Map).
-- La signatura de este tipo es Map k v donde:
--     · k es un tipo de datos comparable (Ord k)
--     · v es un tipo de datos cualquiera.
--
-- Sin embargo, Haskell utiliza tipado fuerte por lo que v ha de ser un tipo concreto,
-- es decir todos los elementos asociados a las distintas claves han de ser del mismo tipo.
-- Esto impediría por ejemplo que el agente tuviese en su memoria {"posicion":(3,4), "velocidad":5}
-- ya que el primer valor corresponde al tipo Point mientras que el segundo corresponde a Double.
--
-- Hemos pues de encapsular los distintos tipos en un único tipo que los enmascare bajo una apariencia de
-- tipo único. Y después proveer los mecanismos necesarios para extraer los valores y gestionarlos de 
-- manera adecuada.
--

-- Para ello, podemos nosotros definir un nuevo tipo de dato que actúe como máscara, utilizando un constructor
-- distinto para cada tipo de dato que vayamos a admitir en el tipo encapsulado. Declaremos este tipo como FlexibleR:

data FlexibleR =
    VInt Int | VFloat Float | VStr String | VFloatP (Float, Float) | VBool Bool

instance Show FlexibleR where
  show (VInt n) = show n
  show (VFloat f) = show f
  show (VStr s) = show s
  show (VFloatP p) = show p
  show (VBool b) = show b

-- Con esto, sí podríamos definir la memoria como un Map con datos flexibles, pero sin perder 
-- el fuerte tipado inherente a haskell.

type MemoriaFR = M.Map String FlexibleR 

memoriaVacia :: MemoriaFR
memoriaVacia = M.empty

memoriaInsert :: String -> FlexibleR -> MemoriaFR -> MemoriaFR
memoriaInsert = M.insert

-- Para cada uno de los tipos admitidos tendríamos el correspondiente constructor de manera que luego podríamos gestionar
-- la extracción de esos valores con el uso de case ... of o similar. 

memoriaGetInt :: String -> MemoriaFR -> Maybe Int
memoriaGetInt k m = case M.lookup k m of
    Just (VInt n)  -> Just n
    _ -> Nothing

memoriaGetFloat :: String -> MemoriaFR -> Maybe Float
memoriaGetFloat k m = case M.lookup k m of
    Just (VFloat f) -> Just f
    _ -> Nothing

memoriaGetString :: String -> MemoriaFR -> Maybe String
memoriaGetString k m = case M.lookup k m of
    Just (VStr s) -> Just s
    _ -> Nothing

memoriaGetFloatP :: String -> MemoriaFR -> Maybe (Float, Float)
memoriaGetFloatP k m = case M.lookup k m of
    Just (VFloatP p) -> Just p
    _ -> Nothing

memoriaGetBool :: String -> MemoriaFR -> Maybe Bool
memoriaGetBool k m = case M.lookup k m of
    Just (VBool b) -> Just b
    _ -> Nothing

-- Podemos almacenar distintos tipos de valores pero todos ellos enmascarados bajo el tipo FlexibleR. 
