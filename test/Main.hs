module Main where

import qualified Data.Map.Strict as Map
import MendelInheritance

main :: IO ()
main = do
  -- 1) Создаём аллели с помощью безопасного конструктора
  let Just alleleA = makeAllele 'A' "гребень"
      Just allelea = makeAllele 'a' "нет гребня"
      Just alleleB = makeAllele 'B' "оперённые ноги"
      Just alleleb = makeAllele 'b' "голые ноги"

  -- 2) Строим родительские генотипы
  let parent1Genes =
        sequence
          [ makeGen "comb" (alleleA, alleleA),
            makeGen "legs" (alleleB, alleleB)
          ]
      parent2Genes =
        sequence
          [ makeGen "comb" (alleleA, allelea),
            makeGen "legs" (alleleb, alleleb)
          ]

  let Just rooster = parent1Genes >>= makeGenotype
      Just hen = parent2Genes >>= makeGenotype

  -- 3) Печатаем информацию о родителях
  printIndividual "Петух (AABB)" rooster
  printIndividual "Курица (Aabb)" hen

  -- 4) Получаем первое поколение (F1)
  let f1 = cross rooster hen
      f1Genotypes = getGenotypes f1

  putStrLn "\n--- Первое поколение (F1) ---"
  mapM_ printIndividualSimple f1Genotypes

  -- 5) Соотношение по генотипам в F1
  putStrLn "\n--- Соотношение по генотипам в F1 ---"
  Map.foldrWithKey
    (\g count acc -> putStrLn (prettyGenotype g ++ " : " ++ show count) >> acc)
    (return ())
    (genotypeRatio f1)

-- | Печать полной информации об индивидууме
printIndividual :: String -> Genotype -> IO ()
printIndividual label genotype = do
  putStrLn $ "\n" ++ label
  putStrLn $ "Генотип: " ++ prettyGenotype genotype
  putStrLn $ "Фенотип:\n" ++ prettyPhenotype (phenotypeFromGenotype genotype)

-- | Печать без заголовка (для списка потомков)
printIndividualSimple :: Genotype -> IO ()
printIndividualSimple genotype = do
  putStrLn $ "Генотип: " ++ prettyGenotype genotype
  putStrLn $ "Фенотип:\n" ++ prettyPhenotype (phenotypeFromGenotype genotype)
