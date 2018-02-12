module Demo where

import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.Vector

-- | Угловой момент системы частиц.
angularMomentum :: [Particle] -> Float
angularMomentum _ = 0 -- реализуйте самостоятельно :)

-- | Рассчитать центр масс системы частиц.
massCenter :: [Particle] -> Point
massCenter ps = sum (map g ps)
  where
    m = sum (map mass ps)
    g p = mulSV (mass p / m) (position p)

-- | Кинетическая энергия системы частиц.
kineticEnergy :: [Particle] -> Float
kineticEnergy = sum . map k
  where
    k p = mass p * (v p)^2 / 2
    v p = magV (velocity p)

-- | Запустить моделирование с заданным начальным состоянием вселенной.
demo :: Universe -> IO ()
demo universe = simulate display bgColor fps universe drawUniverse updateUniverse
  where
    display = InWindow "Столкновение галактик" (500, 500) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 10      -- кол-во кадров в секунду

-- | Стандартная вселенная с двумя вращающимися галактиками.
defaultUniverse :: Universe
defaultUniverse = Universe
  [ genGalaxy Clockwise         90 (-10,-10) ( 0.3,  0.01)
  , genGalaxy Counterclockwise 150 ( 10, 10) (-0.3, -0.01)
  ]

-- | Модель вселенной.
data Universe = Universe
  { galaxies :: [Galaxy]  -- ^ Галактики моделируемой вселенной.
  }

-- | Модель галактики.
data Galaxy = Galaxy
  { stars :: [Particle]  -- ^ Звёзды галактики.
  }

-- | Масса — это просто число.
type Mass = Float

-- | Частица (например, звезда или планета).
data Particle = Particle
  { mass      :: Mass   -- ^ Масса частицы.
  , position  :: Point  -- ^ Положение частицы.
  , velocity  :: Vector -- ^ Вектор скорости частицы.
  }

-- | Направление вращательного движения.
data RotationDir
  = Clockwise         -- ^ По часовой стрелке.
  | Counterclockwise  -- ^ Против часовой стрелки.

-- | Сгенерировать галактику.
genGalaxy
  :: RotationDir -- ^ Направление вращения галактики.
  -> Int         -- ^ Количество звёзд (не считая чёрной дыры в центре галактики).
  -> Point       -- ^ Расположение центра галактики.
  -> Vector      -- ^ Вектор скорости центра масс галактики.
  -> Galaxy
genGalaxy rotationDir n center vel = Galaxy (blackHole : galaxyStars)
  where
    rsign = case rotationDir of
      Clockwise        ->  1
      Counterclockwise -> -1

    galaxyStars = take n (map mkStar locs)

    locs  = zipWith mulSV dists dirs
    dirs  = iterate (rotateV (- rsign * phi)) (1, 0)
    dists = [3, 3.1 ..]

    m = 1
    blackHole = Particle (10 * m * fromIntegral n) center vel

    -- создать звезду в заданном месте галактики
    mkStar loc = star
      where
        accel = magV (gravityAccel star (Particle (m * fromIntegral n + mass blackHole - m) center (0, 0)))
        dist  = magV loc
        star  = Particle m (center + loc) (vel + mulSV (sqrt (accel * dist)) (normalizeV (rotateV (rsign * pi/2) loc)))

    phi = (1 + sqrt 5) / 2  -- золотое сечение

-- | Отобразить вселенную.
drawUniverse :: Universe -> Picture
drawUniverse u = scale 6 6 $ pictures
  [ pictures (zipWith drawGalaxy [red, magenta] (galaxies u))
  , color green (drawMassCenter (massCenter (allParticles u)))
  , color green (drawAngularMomentum (angularMomentum (allParticles u)) (massCenter (allParticles u)))
  , color green (text' 0 ("K = "  ++ show (kineticEnergy (allParticles u))))
  , color red (text' 1 ("K1 = " ++ show (kineticEnergy (stars (galaxies u !! 0)))))
  , color magenta (text' 2 ("K2 = " ++ show (kineticEnergy (stars (galaxies u !! 1)))))
  ]
  where
    text' n = translate (-40) (30 - n * 4) . scale 0.02 0.02 . text

-- | Отобразить галактику.
drawGalaxy :: Color -> Galaxy -> Picture
drawGalaxy c g = pictures
  [ pictures (map drawStar (stars g))
  , color c (drawMassCenter (massCenter (stars g)))
  , color c (drawAngularMomentum (angularMomentum (stars g)) (massCenter (stars g)))
  ]

-- | Отобразить звезду.
drawStar :: Particle -> Picture
drawStar p = color white (uncurry translate (position p) (thickCircle (r / 2) r))
  where
    r = 0.4 * mass p ** 0.1

-- | Отобразить точку как центр масс.
drawMassCenter :: Point -> Picture
drawMassCenter pos = uncurry translate pos (thickCircle 0.2 0.4)

-- | Отобразить угловой момент в виде стрелки.
-- Толщина стрелки показывать абсолютную величину углового момента.
drawAngularMomentum :: Float -> Point -> Picture
drawAngularMomentum am pos
  | am == 0   = blank
  | otherwise = uncurry translate pos (pictures [thickArc 0 180 r (dr * r/3), triangle])
  where
    dr = 1 - 1 / (am / 300 + 1)
    r = 2
    triangle = if am < 0 then triangleL else triangleR
    triangleR = polygon [(-r, -0.3*r), (-1.3*r, 0.3*r), (-0.7*r, 0.3*r)]
    triangleL = polygon [(r, -0.3*r), (1.3*r, 0.3*r), (0.7*r, 0.3*r)]

-- | Обновить состояние вселенной по прошествии заданного времени.
updateUniverse :: ViewPort -> Float -> Universe -> Universe
updateUniverse _ dt = updateEveryParticle (updateParticle dt)

-- | Обновить каждую частицу во вселенной используя заданную функцию обновления вселенной.
updateEveryParticle :: (Universe -> Particle -> Particle) -> Universe -> Universe
updateEveryParticle f u = u { galaxies = map updateGalaxy (galaxies u) }
  where
    updateGalaxy g = g { stars = map updateStar (stars g) }
    updateStar s = f u s

-- | Обновить частицу, учитывая все силы вселенной.
updateParticle :: Float -> Universe -> Particle -> Particle
updateParticle dt u p = p
  { position = position p + mulSV dt (velocity p)
  , velocity = velocity p + mulSV dt accel
  }
  where
    accel = sum (map (gravityAccel p) (allParticles u))

-- | Получить список всех частиц во вселенной.
allParticles :: Universe -> [Particle]
allParticles = concatMap stars . galaxies

-- | Вычислить ускорение частицы, придаваемое ей притяжением другой частицы.
gravityAccel :: Particle -> Particle -> Vector
gravityAccel p1 p2
  | d > 0.0001 && d < 500 = mulSV (0.005 * mass p2 / d^2) (normalizeV r)
  | otherwise  = (0, 0)
  where
    r = position p2 - position p1
    d = magV r
