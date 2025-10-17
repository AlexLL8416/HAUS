module Fisicas where

type Point = (Float, Float) 
type Vector = (Float, Float)
type Angle = Float
type Distance = Float
type Position = (Float, Float)
type Size = (Float, Float) -- Tipo auxiliar para isInBounds
type Rect = (Point, Point, Point, Point)

distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) = sqrt (((x2 - x1)**2) + ((y2 - y1)**2))

angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1) -- atan2 para tener en cuenta los signos de las coordenadas. 

deg2rad :: Angle -> Angle
deg2rad a = (a / 360.0) * 2 * pi

rad2deg :: Angle -> Angle
rad2deg a = (a / pi) * 180.0

subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2) 

getVertices :: Rect -> Angle -> Rect
getVertices (p1,p2,p3,p4) a = (t1, t2, t3, t4)
    where c = centro (p1, p2, p3, p4)
          [t1, t2, t3, t4] = [trasladoOr (rotacion (traslado p c) a) c | p <- [p1, p2, p3, p4]]

centro :: Rect -> Point -- Calcular el centro geométrico de cuatro puntos.
centro ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) = ((x1 + x2 + x3 + x4) / 4 , (y1 + y2 + y3 + y4) / 4) 

traslado :: Point -> Point -> Point  -- Traslada un punto al centro geométrico dado (colocar punto en origen). P'i = (xi - Cx, yi - Cy)
traslado (x1, y1) (x2, y2) = ((x1 - x2), (y1 - y2))

rotacion :: Point -> Angle -> Point   -- Calcula la rotación de un punto respecto al ángulo dado. P''i = (x'i * cos a - y'i * sin a, x'i * sin a + y'i * cos a)
rotacion (x, y) a = (((x * cos a) - (y * sin a)), ((x * sin a) + (y * cos a))) 

trasladoOr :: Point -> Point -> Point  -- Devuelve el punto rotado al centro original. P.rotado = (x''i + Cx, y''i + Cy)
trasladoOr (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dot :: Point -> Point -> Float 
dot (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

perp :: Vector -> Vector
perp (x, y) = (-y, x)

isInBounds :: Point -> Size -> Bool -- Miro si un punto (x,y) está entre (0,0) y el tamaño dado (a,b)
isInBounds (x1, y1) (x2, y2) = x1 >= 0 && x1 <= x2 && y1 >= 0 && y1 <= y2

-- mul: tal que (w,h) `mul` (sw,sh) = (w * sw, h * sh)
mul :: (Float,Float) -> (Float,Float) -> (Float,Float)
mul (w,h) (sw,sh) = (w*sw, h*sh)

normalizar :: Vector -> Vector
normalizar (x, y) = (x / m, y / m)
    where m = sqrt ((x ^ 2) + (y ^ 2))

-- Añade el vector a todos los puntos del rectángulo
trasladoRect :: Rect -> Vector -> Rect
trasladoRect ((ax, ay), (bx, by), (cx, cy), (dx, dy)) (x, y) = ((ax + x, ay + y), (bx + x, by + y), (cx + x, cy + y), (dx + x, dy + y))
