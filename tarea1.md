Definir los tipos básicos del juego y las funciones básicas de geometría.


   1. Tipos a definir:

      - Point. Un punto 2D en el espacio.

      - Vector. Vector siempre se considera que empieza en (0,0)

      - Angle. Un angulo con decimales

      - Distance. Un valor de distancia con decimales.

      - Position. Representa la posición de objeto en un mundo 2D.


   2. Definir las siguientes funciones:


       - distanceBetween :: Position -> Position -> Distance. Calcula la distancia euclidiana entre dos posiciones en el espacio. Toma dos puntos como entrada y devuelve la distancia lineal que los separa.


       - angleToTarget :: Position -> Position -> Angle. Determina el ángulo desde una posición origen hacia una posición objetivo. Útil para calcular la dirección en la que debe apuntar o moverse un objeto.


       - deg2rad :: Angle -> Angle. Convierte un ángulo expresado en grados a su equivalente en radianes.


       - rad2deg :: Angle -> Angle. Convierte un ángulo expresado en radianes a su equivalente en grados.


       - subVec :: Vector -> Vector -> Vector. Realiza la resta de dos vectores, devolviendo un nuevo vector que representa la diferencia entre ellos.


       - getVertices :: (Point, Point, Point, Point, Angle) -> [Point]. Genera una lista de vértices (puntos) a partir de cuatro puntos base y un ángulo de rotación.


       - dot :: Point -> Point -> Float. Calcula el producto escalar (dot product) entre dos puntos tratados como vectores


       - sub :: Point -> Point -> Point. Resta un punto de otro, devolviendo un nuevo punto que representa la diferencia entre las coordenadas.


       - perp :: Vector -> Vector. Calcula el vector perpendicular a un punto dado (tratado como vector).


       - isInBounds :: Point -> Size -> Bool. Verifica si un punto se encuentra dentro de los límites definidos por un tamaño dado.


Entrega un archivo de código llamado solución.hs con las implementaciones.


Las entregas atrasadas tendrán una penalización del 20% sobre la puntuación máxima del ejercicio por cada semana de retraso.


Las defensas de los ejercicios se realizarán sobre la implementación entregada.