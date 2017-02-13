module Demo where

import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.Vector

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
  [ genGalaxy 100 (-10,-10) (0.5, 0.01)
  , genGalaxy 150 (10, 10) (-0.1, -0.01)
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

-- | Сгенерировать галактику с заданным кол-вом звёзд и положением центра
genGalaxy :: Int -> Point -> Vector -> Galaxy
genGalaxy n center vel = Galaxy (blackHole : take n stars)
  where
    stars = map mkStar locs
    locs  = zipWith mulSV dists dirs
    dirs  = iterate (rotateV phi) (1, 0)
    dists = [3, 3.1 ..]

    m = 1
    blackHole = Particle (10 * m * fromIntegral n) center vel

    -- создать звезду в заданном месте галактики
    mkStar loc = star
      where
        accel = magV (gravityAccel star (Particle (m * fromIntegral n + mass blackHole - m) center (0, 0)))
        dist  = magV loc
        star = Particle m (center + loc) (vel + mulSV (sqrt (accel * dist)) (normalizeV (rotateV (-pi/2) loc)))

    phi = (1 + sqrt 5) / 2  -- золотое сечение

-- | Отобразить вселенную.
drawUniverse :: Universe -> Picture
drawUniverse = scale 3 3 . pictures . map drawGalaxy . galaxies

-- | Отобразить галактику.
drawGalaxy :: Galaxy -> Picture
drawGalaxy = pictures . map drawStar . stars

-- | Отобразить звезду.
drawStar :: Particle -> Picture
drawStar p = color white (uncurry translate (position p) (thickCircle (r / 2) r))
  where
    r = 0.7 * mass p ** 0.1

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
