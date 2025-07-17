module MendelInheritance.PunnettGloss where

import Graphics.Gloss
import MendelInheritance

drawPunnett :: [Genotype] -> IO ()
drawPunnett genotypes = 
  display (InWindow "Punnett Square" (width, height) (100, 100)) white (pictures allCells)
  where
    -- Constants
    cellSize :: Float
    cellSize = 120  -- Увеличенный размер для лучшей читаемости
    borderWidth :: Float
    borderWidth = 1  -- Тонкие границы
    padding :: Float
    padding = 40     -- Большие отступы
    
    -- Fixed 4x4 grid
    gridSize = 4
    totalCells = gridSize * gridSize
    
    -- Calculate window size
    width = round $ cellSize * fromIntegral gridSize + padding * 2
    height = round $ cellSize * fromIntegral gridSize + padding * 2
    
    -- Generate cell positions (perfectly aligned)
    cellPositions =
      [ (xPos, yPos)
      | row <- [0..gridSize-1]
      , let yPos = cellSize * fromIntegral row - totalHeight/2 + cellSize/2
      , col <- [0..gridSize-1]
      , let xPos = cellSize * fromIntegral col - totalWidth/2 + cellSize/2
      ]
      where
        totalWidth = cellSize * fromIntegral gridSize
        totalHeight = cellSize * fromIntegral gridSize
    
    -- Create perfectly aligned cells with shared borders
    genotypeCells =
      [ translate x y $
          color black $
            pictures 
              [ -- Только правая и нижняя границы для каждой ячейки
                translate (cellSize/2 - borderWidth/2) 0 $ 
                  rectangleSolid borderWidth cellSize  -- Правая граница
              , translate 0 (cellSize/2 - borderWidth/2) $ 
                  rectangleSolid cellSize borderWidth  -- Нижняя граница
              , translate (-cellSize/3) (-cellSize/4) $ 
                  scale 0.12 0.12 $ text (prettyGenotype g)
              ]
      | ((x,y), g) <- zip cellPositions (take totalCells genotypes)
      ]
    
    -- Add outer border
    outerBorder = 
      color black $
        translate 0 0 $
          rectangleWire (cellSize * fromIntegral gridSize) 
                       (cellSize * fromIntegral gridSize)
    
    allCells = genotypeCells ++ [outerBorder]