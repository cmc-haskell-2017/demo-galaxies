module Demo where

import Graphics.Gloss.Interface.Pure.Simulate

-- | Запустить моделирование с заданным начальным состоянием вселенной.
demo :: Universe -> IO ()
demo universe = simulate display bgColor fps universe drawUniverse updateUniverse
  where
    display = InWindow "Столкновение галактик" (500, 500) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Стандартная вселенная с двумя вращающимися галактиками
defaultUniverse :: Universe
defaultUniverse = Universe

-- | Модель вселенной.
data Universe = Universe

-- | Отобразить вселенную.
drawUniverse :: Universe -> Picture
drawUniverse u = blank

-- | Обновить состояние вселенной по прошествии заданного времени.
updateUniverse :: ViewPort -> Float -> Universe -> Universe
updateUniverse _ dt u = u
