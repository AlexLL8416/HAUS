-- ==============================================================
-- || ALTERNATIVA 2: MEMORIA FLEXIBLE SIN RESTRICCIÓN DE TIPOS ||
-- ==============================================================

-- Podemos ir un paso más allá de lo presentado anteriormente, declarando un tipo que admita cualquier
-- tipo identificable como posible tipo del elemento enmascarado. Los tipos identificables son aquellos
-- que derivan de la clase Typable. Aunque esto puede parecer una restricción, en realidad, desde la
-- versión GHC 7.10, todos los tipos derivan de esta clase así que, en la práctica no estamos imponiendo
-- ninguna restricción sobre los tipos. 
--
-- Fuentes: https://stackoverflow.com/questions/32980745/haskell-typeable-instance
--          https://chrisdone.com/posts/data-typeable/

-- Así, en nuestro tipo flexible tendremos un único constructor, que enmascarará todo elemento (de un tipo
-- identificable en un tipo flexible).

-- El tipo FlexibleT puede contener cualquier valor que sea Typeable
data FlexibleT = forall a. Typeable a => FlexibleT a

-- Ahora, tendremos que proveer los métodos necesarios para poder extraer ese valor flexible. 
-- Al contrario que en la alternativa anterior, no conocemos de antemano (por el constructor)
-- el tipo del que se trata a, por ello, debemos tener una función que consiga saber si el tipo
-- que esperamos extraer cuadra con el tipo a. Para ello la función `cast` permite precisamente
-- realizar ese punto. 
-- Fuente: https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Typeable.html

--Así, podemos establecer funciones de extracción específica para distintos tipos: 

flexibleT2Int :: FlexibleT -> Integer
flexibleT2Int (FlexibleT a) =
  case cast a :: Maybe Integer of 
    Just i -> i
    Nothing -> error "El valor encapsulado no corresponde al tipo Integer"

-- De manera análoga al anterior, podríamos definir ahora una memoria totalmente flexible, con los
-- distintos tipos encapsulados en el tipo FlexibleT

-- Con esto, sí podríamos definir la memoria como un Map con datos flexibles, pero sin perder 
-- el fuerte tipado inherente a haskell.

type MemoriaFT = M.Map String FlexibleT
