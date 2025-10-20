module Constantes where

-- Velocidad de giro del cañón en radianes por segundo
velGiroCannon :: Num a => a
velGiroCannon = 2

-- Velocidad de giro del tanque en radianes por segundo
velGiroTanque :: Num a => a
velGiroTanque = 1

-- Velocidad total máxima permitida para los tanques en pixeles por segundo
maxTanqueVelocity :: Num a => a
maxTanqueVelocity = 100

-- Aceleración que se aplicará cuando el tanque quiera acelerar, en pixeles por segundo^2
magnitudAceleracion :: Num a => a
magnitudAceleracion = 50

-- Factor por el que se multiplicará la velocidad cuando el tanque quiera frenar
factorFrenado :: Fractional a => a
factorFrenado = -1.5

-- Velocidad de las balas al ser disparadas
velBalaDisparada :: Fractional a => a
velBalaDisparada = 300
