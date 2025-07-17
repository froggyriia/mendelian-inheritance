module Main where

import qualified Data.Map.Strict as Map
import MendelInheritance
import MendelInheritance.PunnettGloss
import qualified Data.Map as Map

main :: IO ()
main = do
  -- Alleles:
  -- A – comb
  -- a – no comb
  -- B – feathered legs
  -- b – bare legs
  let Just a = makeAllele 'A' "гребень" -- Dominant allele for comb
  let Just a' = makeAllele 'a' "нет гребня" -- Recessive allele for no comb
  let Just b = makeAllele 'B' "оперённые ноги" -- Dominant allele for feathered legs
  let Just b' = makeAllele 'b' "голые ноги" -- Recessive allele for bare legs

  -- Parent genotypes:
  -- Rooster (AABB) × Hen (Aabb)
  let Just genAABB =
        makeGenotype
          =<< sequence
            [ makeGen "comb" (a, a), -- Homozygous dominant for comb
              makeGen "legs" (b, b) -- Homozygous dominant for legs
            ]
  let Just genAabb =
        makeGenotype
          =<< sequence
            [ makeGen "comb" (a, a'), -- Heterozygous for comb
              makeGen "legs" (b', b') -- Homozygous recessive for legs
            ]


  -- Print parent information
  putStrLn "\n--- Родители ---"
  putStrLn "Петух (AABB):"
  putStrLn $ "Генотип: " ++ prettyGenotype genAABB -- Print rooster genotype
  putStrLn $ "Фенотип:\n" ++ prettyPhenotype (phenotypeFromGenotype genAABB)

  putStrLn "\nКурица (Aabb):"
  putStrLn $ "Генотип: " ++ prettyGenotype genAabb -- Print hen genotype
  putStrLn $ "Фенотип:\n" ++ prettyPhenotype (phenotypeFromGenotype genAabb)

  -- First generation (F1) – result of crossing AABB × Aabb
  let gen1 = cross genAABB genAabb
  let gen1List = getGenotypes gen1

  -- Getting Alleles of parents
  let rowHeaders = extractAlleles genAabb
  let colHeaders = extractAlleles genAABB
  
  

  -- Print F1 generation
  putStrLn "\n--- Первое поколение ---"
  mapM_
    ( \g -> do
        putStrLn $ "Генотип: " ++ prettyGenotype g
        putStrLn $ "Фенотип:\n" ++ prettyPhenotype (phenotypeFromGenotype g)
    )
    gen1List

  -- Print genotype ratio in F1
  putStrLn "\n--- Соотношение по генотипам ---"

  Map.foldrWithKey
    (\g count acc -> putStrLn (prettyGenotype g ++ " : " ++ show count) >> acc)
    (return ())

    (genotypeRatio gen1)

  -- Punnett square for first generation
  drawPunnett gen1List

