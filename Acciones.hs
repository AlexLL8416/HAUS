module Acciones where
import Entidades
import Fisicas
import Memoria
import Logica
import Data.List (sortOn)
import Utilidades (replaceItem)
import Constantes 
import qualified Data.Map as M

-- Definición de acciones que un bot puede tomar

nada :: Accion ()
nada = return ()

-- Funcion auxiliar para acciones que solo actúan en los datos del mismo tanque
modif :: (Tanque -> Tanque) -> Accion Tanque
modif f = Accion $ \tidx m ->
    let (m', t) = modificarTanque tidx f m
    in (t, m')

-- Funcion auxiliar para acciones que solo actúan en los datos extra del mismo tanque
modif' :: (InfoTanque -> InfoTanque) -> Accion Tanque
modif' f = Accion $ \tidx m ->
    let (m', t) = modificarTanque tidx (fmap f) m
    in (t, m')

-- Hace que el tanque empieze a acelerar
acelerar :: Accion ()
acelerar = do
    modif' (\t -> t { acelerando = True, frenando = False })
    return ()

-- Hace que el tanque deje de acelerar y frene
parar :: Accion ()
parar = do
    modif' (\t -> t { acelerando = False, frenando = True })
    return ()

-- "Control de crucero", intenta mantener una velocidad objetivo alternando entre acelerar y frenar
mantenerVelocidad :: Float -> Accion ()
mantenerVelocidad v = do
    t <- miTanque
    let vel = modV (velocidad t)
    if vel < v then acelerar
    else parar

-- Devuelve la instancia del tanque correspondiente al cerebro que llama a esta mónada
miTanque :: Accion Tanque
miTanque = Accion $ \tidx m ->
    let t = tanques m !! tidx
    in (t, m)

-- Gira el tanque de manera que apunte hacia el punto dado
girarHacia :: Point -> Accion ()
girarHacia p = Accion $ \tidx m ->
    let posYo = posicion (tanques m !! tidx)
        ang = angleToTarget posYo p
        (m', _) = modificarTanque tidx (fmap (\t -> t { quiereAngulo = ang })) m
    in ((), m')

-- Apunta el cañón del tanque hacia el punto dado
-- Devuelve True si el cañón está cerca del objetivo
apuntarCanyonHacia :: Point -> Accion Bool
apuntarCanyonHacia p = Accion $ \tidx m ->
    let posYo = posicion (tanques m !! tidx)
        ang = angleToTarget posYo p
        (m', t') = modificarTanque tidx (fmap (\t -> t { quiereAnguloCannon = ang })) m
        a = anguloCannon (extra t')
        cerca = abs (a - ang) < deg2rad 1 -- Se considera que el cañón está apuntando al objetivo si hay menos de X grados de diferencia
    in (cerca, m')

-- Dispara una bala desde el extremo del cañón, respetando la cadencia de disparo
disparar :: Accion ()
disparar = Accion $ \tidx m ->
    let t = tanques m !! tidx
        tx = extra t
        tj = tiempoJuego m
        m' = if tj - tiempoUltimoDisparo tx > (1 / cadenciaDisparo tx)
            then let (m'', _) = modificarTanque tidx (fmap (\t' -> t' { tiempoUltimoDisparo = tj })) m    -- Almacenamos el tiempo actual para limitar la cadencia de disparo
                     posBala = addVV (posicion t) (mulVS (longitudCannon tx) (anguloV (anguloCannon tx))) -- Calculamos la posición inicial de la bala, que será el extremo del cañón
                 in addBala posBala (mulVS velBalaDisparada (anguloV (anguloCannon tx))) 5 (idObjeto t) m''
            else m
    in ((), m')

-- Devuelve una lista de enemigos ordenados de más cercano a más lejano
enemigos :: Accion [Tanque]
enemigos = Accion $ \tidx m ->
    let t = tanques m !! tidx
        r = radar (extra t)
        enems = [x | x <- tanques m, idObjeto x /= idObjeto t, distanceBetween (posicion x) (posicion t) < r, energia (extra x) > 0]
        ordenado = sortOn (\t' -> distanceBetween (posicion t) (posicion t')) enems
    in (ordenado, m)

-- Devuelve los segundos transcurridos desde el comienzo del juego
tiempo :: Accion Float
tiempo = Accion $ \_ m -> (tiempoJuego m, m)

-- Lee la memoria del tanque y la devuelve
leerMemoria :: Accion MemoriaFR
leerMemoria = Accion $ \tidx m ->
    let t = tanques m !! tidx
    in (Entidades.memoria (extra t), m)

-- Guarda la memoria del tanque
guardarMemoria :: MemoriaFR -> Accion ()
guardarMemoria mem = do
    modif' (\t -> t { memoria = mem })
    return ()

tamanyoCampo :: Accion Size
tamanyoCampo = Accion $ \_ m -> (tamanyoMundo m, m)

-- Implementación de un algoritmo trivial que evita chocarse contra las paredes a todo coste
evitarParedes :: Accion ()
evitarParedes = do
    t <- miTanque
    (anchoMundo, altoMundo) <- tamanyoCampo

    let (x, y) = posicion t
        margen = 100
        estaDentro = x >= -(anchoMundo / 2) + margen && x <= (anchoMundo / 2) - margen &&
                     y >= -(altoMundo / 2) + margen && y <= (altoMundo / 2) - margen

    if estaDentro then nada -- Si el bot está dentro del campo (teniendo en cuenta los márgenes), no hacemos nada
    else do
        girarHacia (0, 0) -- En caso contrario, orientaremos el bot hacia el centro del campo para que se aleje de las paredes
        if dot (x, y) (anguloV (angulo (extra t))) > 0
        then mantenerVelocidad 40 -- Si nos estamos alejando del centro, reduciremos la velocidad para asegurarnos de que no nos salimos del campo y facilitar la maniobra.
        else nada

-- Funciones auxiliares para usar la memoria del bot --

-- Guarda un dato (por ejemplo una posición) en la memoria del tanque actual
guardarDato :: String -> (Float, Float) -> Accion ()
guardarDato clave valor = do
    mem <- leerMemoria
    let memNueva = M.insert clave (VFloatP valor) mem
    guardarMemoria memNueva

-- Recupera un dato previamente guardado en la memoria
recuperarDato :: String -> Accion (Maybe (Float, Float))
recuperarDato clave = do
    mem <- leerMemoria
    case M.lookup clave mem of
        Just (VFloatP v) -> return (Just v)
        _ -> return Nothing

-- Comprueba si el tanque ha sido golpeado desde la última iteración
fuiGolpeado :: Accion Bool
fuiGolpeado = do
    t <- miTanque
    mem <- leerMemoria
    let energiaAct = energia (extra t)
        energiaPrev = case M.lookup "energiaPrev" mem of
            Just (VFloat e) -> e
            _ -> energiaAct
        golpeado = energiaAct < energiaPrev
        memNueva = M.insert "energiaPrev" (VFloat energiaAct) mem
    guardarMemoria memNueva
    return golpeado
