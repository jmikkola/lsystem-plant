import Graphics.Gloss

{-
    Algorithm for drawing the plant is from
    http://en.wikipedia.org/wiki/L-system
-}

stepSize :: Int -> Float
stepSize n = fromIntegral $ 2 ^ n

twig :: Float -> Picture
twig size = Line [(0, 0), (0, size)]

push :: Float -> Picture -> Picture
push = Translate 0

afterLine :: Int -> [Picture] -> Picture
afterLine step parts = Pictures [twig size, push size $ Pictures parts]
  where size = stepSize step

-- (X → F-[[X]+X]+F[+FX]-X), (F → FF)

plant :: Int -> Float -> Picture
plant 0    _     = twig (stepSize 1)
plant step angle = afterLine step [
               left nextPlant,
               nextPlant,
               afterLine step [
                 right $ afterLine step [nextPlant],
                 left nextPlant
                 ]
               ]
  where nextPlant = plant (step - 1) angle
        left = Rotate (angle - 25)
        right = Rotate ((angle / 2) + 25)

plantOfSize :: Int -> Float -> Picture
plantOfSize n angle = Scale factor factor $ plant n angle
  where factor = 100 / (stepSize n)

windowHeight = 650
windowWidth = 500
window = InWindow "Plant" (windowWidth, windowHeight) (20, 20)

darkGreen = makeColor8 55 93 33 255

drawPlant :: Float -> Picture
drawPlant time = Translate (-75) (-300) $ Color darkGreen $ plantOfSize 5 angle
  where t = time / 1.5
        x = ((sin t) + (sin (t + pi * 0.4))) / 2
        angle = 5 * (x ^ 5)

main = animate window black drawPlant
