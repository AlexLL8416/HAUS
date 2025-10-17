import Graphics.Gloss

main :: IO ()
main = display ventana fondo dibujo 
    where
        ventana = InWindow "Ejemplo 1" (400, 400) (10, 10)
        fondo = makeColor 0.698 1 1 0.5  -- Genera un azul celeste clarito
        dibujo = Pictures [ Color yellow $ Rotate 45 (rectangleSolid 20 40), 
            Color red $ Translate 50 0 $ Polygon [(0, 20), (20,-20), (-20, -20)]]
