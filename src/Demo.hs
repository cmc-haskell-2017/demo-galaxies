module Demo where

import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.Vector

-- | Запустить моделирование с заданным начальным состоянием вселенной.
demo :: Universe -> IO ()
demo universe = simulate display bgColor fps universe drawUniverse updateUniverse
  where
    display = InWindow "Столкновение галактик" (500, 500) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Стандартная вселенная с двумя вращающимися галактиками
defaultUniverse :: Universe
defaultUniverse = Universe [genGalaxy 100 (-100,-100), genGalaxy 200 (100, 100)]

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

genGalaxy :: Int -> Point -> Galaxy
genGalaxy n center = Galaxy (take n stars)
  where
    mkStar loc = Particle 1 (center + loc) (rotateV (pi/2) loc)
    stars = map mkStar locs
    locs  = zipWith mulSV dists dirs
    dirs  = iterate (rotateV phi) (1, 0)
    dists = [0, 0.2 ..]

    phi = (1 + sqrt 5) / 2  -- золотое сечение

-- | Отобразить вселенную.
drawUniverse :: Universe -> Picture
drawUniverse = pictures . map drawGalaxy . galaxies

-- | Отобразить галактику.
drawGalaxy :: Galaxy -> Picture
drawGalaxy = pictures . map drawStar . stars

-- | Отобразить звезду.
drawStar :: Particle -> Picture
drawStar p = color white (uncurry translate (position p) (circle (mass p)))

-- | Обновить состояние вселенной по прошествии заданного времени.
updateUniverse :: ViewPort -> Float -> Universe -> Universe
updateUniverse _ dt u = u

