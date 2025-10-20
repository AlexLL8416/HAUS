module Entidades where
import Fisicas (Position,Vector,Angle,Distance, Size, Point)
import Data.List (findIndex)
import Utilidades (replaceItem)
import Memoria (MemoriaFR, memoriaVacia)


-- La función de acción recibe el índice del tanque que actúa y el mundo actual.
-- La definición e instancias de este tipo deberían estar en un archivo aparte, pero por razones de ciclos de importación es necesario ponerlo aquí.
newtype Accion a = Accion { fn :: Int -> Mundo -> (a, Mundo) }

instance Show (Accion a) where
    show _ = "<accion>"

instance Functor Accion where
    fmap f (Accion fn) = Accion $ \t m -> -- Aplica la función f al valor de la función de mutación de mundo, devolviendo el mismo mundo
        let (a, m') = fn t m
        in (f a, m')

instance Applicative Accion where
    pure x = Accion $ \t m -> (x, m)  -- Devuelve una acción que no cambia el mundo, sólo devuelve un valor
    (Accion f) <*> (Accion a) = Accion $ \t m -> -- Encadena dos operaciones de mutaciones
        let (g, m1) = f t m     -- La primera Accion devuelve una función que transforma el valor que devuelve la segunda Accion
            (x, m2) = a t m1    -- Llamamos a la segunda Accion con el mundo que devolvió la primera Accion
        in (g x, m2)            -- Transformamos el valor de la segunda Accion y lo devolvemos junto con el mundo devuelto por la segunda Accion

instance Monad Accion where
    return = pure

    -- Ejecuta una Accion, le pasa el valor devuelto a una función que devuelve una Accion, y las secuencia una detrás de otra
    (Accion a) >>= f = Accion $ \t m ->
        let (x, m') = a t m     -- Ejecuta la Accion, devolviendo un valor y un mundo mutado
            (Accion b) = f x    -- Llama a la función con el valor devuelto, recibiendo una nueva Accion
        in b t m'               -- Devuelve el resultado de llamar a la nueva Accion con el mundo mutado de la primera Accion


-- Creación de tipo genérico
data Objeto a
    = Objeto {
        idObjeto :: Int,
        posicion :: Position,
        velocidad :: Vector,
        tamanyo :: Size,
        extra :: a
    } deriving (Show, Eq)

instance Functor Objeto where
  fmap f obj = obj { extra = f (extra obj) }

-- Propiedades individuales de cada tanque
data InfoTanque
    = InfoTanque {
        nombre :: String,
        energia :: Float,
        radar :: Distance,
        angulo :: Angle,
        quiereAngulo :: Angle,
        anguloCannon :: Angle,
        quiereAnguloCannon :: Angle,
        acelerando :: Bool,
        frenando :: Bool,
        memoria :: MemoriaFR,
        cerebro :: Accion (),
        tiempoUltimoDisparo :: Float,
        cadenciaDisparo :: Float,
        longitudCannon :: Float
    } deriving (Show) -- no podemos derivar Eq ya que Accion contiene una función, que no es comparable

type Tanque = Objeto InfoTanque

-- Propiedades individuales de cada bala
data InfoBala
    = InfoBala {
        danyo :: Float,
        disparador :: Int
    } deriving (Show, Eq)

type Bala = Objeto InfoBala

-- Propiedades individuales de cada explosión
data InfoExplosion
    = InfoExplosion {
       radio :: Float,
       tiempoVida :: Float   -- duración restante de la explosión en segundos
    } deriving (Show, Eq)

type Explosion = Objeto InfoExplosion

data Mundo
    = Mundo {
        tiempoJuego :: Float,
        tanques :: [Tanque],
        balas :: [Bala],
        explosiones :: [Explosion],
        contadorID :: Int,
        tamanyoMundo :: Size
    } deriving (Show)

mundoVacio t = Mundo {
    tiempoJuego = 0,
    tanques = [],
    balas = [],
    explosiones = [],
    contadorID = 0,
    tamanyoMundo = t
}

-- Añade un tanque al mundo dada su posición y su información de tanque, asignándole una nueva ID
addTanque :: Point -> (InfoTanque -> InfoTanque) -> Mundo -> Mundo
addTanque pos f m =
    let ts = tanques m
        id = contadorID m
        t = Objeto {
            idObjeto = id,
            posicion = pos,
            velocidad = (0, 0),
            tamanyo = (40, 40),
            extra = f (InfoTanque {
                nombre = "Tanque " ++ show (id),
                energia = 100,
                radar = 300,
                angulo = 0,
                quiereAngulo = 0,
                anguloCannon = 0,
                quiereAnguloCannon = 0,
                acelerando = False,
                frenando = False,
                memoria = memoriaVacia,
                cerebro = do return (),
                longitudCannon = 40,
                cadenciaDisparo = 0.8,
                tiempoUltimoDisparo = 0
            })
        }
    in m { tanques = ts++[t], contadorID = id + 1 }

-- Busca un tanque dada su ID y devuelve su índice en la lista de tanques, si existe
findTanque :: Int -> [Tanque] -> Maybe Int
findTanque id = findIndex (\t -> idObjeto t == id)

-- Modifica un tanque dado su índice en la lista de tanques del mundo y devuelve el mundo mutado y el tanque mutado
modificarTanque :: Int -> (Tanque -> Tanque) -> Mundo -> (Mundo, Tanque)
modificarTanque idx f m =
    let ts = tanques m
        t = ts !! idx
        t' = f t
    in (m { tanques = replaceItem idx t' ts }, t')

addBala :: Point -> Vector -> Float -> Int -> Mundo -> Mundo
addBala pos vel dan idDisparador m =
    let bs = balas m
        id = contadorID m
        b = Objeto {
            idObjeto = id,
            posicion = pos,
            velocidad = vel,
            tamanyo = (5, 5),
            extra = InfoBala {
                danyo = dan,
                disparador = idDisparador
            }
        }
    in m { balas = bs ++ [b], contadorID = id + 1 }

-- Crear una nueva explosión en una posición concreta
addExplosion :: Point -> Float -> Explosion
addExplosion pos radioInicial = Objeto
    { idObjeto = 0           -- El id se puede asignar en updateMundo para evitar colisiones
    , posicion = pos
    , velocidad = (0,0)
    , tamanyo = (radioInicial*2, radioInicial*2)
    , extra = InfoExplosion { radio = radioInicial, tiempoVida = 0.5 }  -- 0.5s por ejemplo
    }
