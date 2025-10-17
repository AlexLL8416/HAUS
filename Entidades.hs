module Entidades where
import Fisicas (Position,Vector,Angle,Distance, Size)

type Vida = Float

-- Creación de tipo genérico
data Objeto a 
    = Objeto {
        posicion :: Position,
        velocidad :: Vector,
        tamanyo :: Size,
        extra :: a
    } deriving (Show, Eq)

-- Propiedades individuales de cada tanque
data InfoTanque
    = InfoTanque {
        nombre :: String,
        energia :: Vida,
        radar :: Distance,
        anguloCannon :: Angle
    } deriving (Show, Eq)

type Tanque = Objeto InfoTanque

-- Propiedades individuales de cada bala
data InfoBala
    = InfoBala {
        danyo :: Float
    } deriving (Show, Eq)

type Bala = Objeto InfoBala

-- Propiedades individuales de cada explosión
data InfoExplosion 
    = InfoExplosion {
       radio :: Float
    } deriving (Show, Eq)

type Explosion = Objeto InfoExplosion

data Mundo
    = Mundo {
        tanques :: [Tanque],
        balas :: [Bala],
        explosiones :: [Explosion]
    } deriving (Show, Eq)

instance Functor Objeto where
  fmap f (Objeto pos vel tam ext) = Objeto pos vel tam (f ext)

instance Applicative Objeto where
  pure x = Objeto (0,0) (0,0) (0,0) x
  (Objeto _ _ _ f) <*> (Objeto pos vel tam x) = Objeto pos vel tam (f x)