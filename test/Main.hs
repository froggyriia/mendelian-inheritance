module Main where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import MendelInheritance
import MendelInheritance (phenotypeRatio)
import MendelInheritance.Probability
  ( Probability,
    genotypeProbabilities,
    genotypeRatio,
    phenotypeProbabilities,
    phenotypeRatio,
  )
import MendelInheritance.PunnettGloss

main :: IO ()
main = do
  -- Alleles:
  -- A – comb, a – no comb; B – feathered legs, b – bare legs
  let Just a = makeAllele 'A' "гребень"
  let Just a' = makeAllele 'a' "нет гребня"
  let Just b = makeAllele 'B' "оперённые ноги"
  let Just b' = makeAllele 'b' "голые ноги"

  -- Parent genotypes: Rooster (AABB) × Hen (Aabb)
  let Just genAABB =
        makeGenotype
          =<< sequence
            [ makeGen "comb" (a, a),
              makeGen "legs" (b, b)
            ]
  let Just genAabb =
        makeGenotype
          =<< sequence
            [ makeGen "comb" (a, a'),
              makeGen "legs" (b', b')
            ]

  putStrLn "\n--- Parents ---"
  putStrLn $ "Rooster: " ++ prettyGenotype genAABB
  putStrLn $ "          " ++ prettyPhenotype (phenotypeFromGenotype genAABB)
  putStrLn $ "Hen:     " ++ prettyGenotype genAabb
  putStrLn $ "          " ++ prettyPhenotype (phenotypeFromGenotype genAabb)

  -- F1 generation
  let gen1 = cross genAABB genAabb
  let f1 = getGenotypes gen1

  putStrLn "\n--- F1 Generation ---"
  mapM_ (putStrLn . prettyGenotype) f1

  let countG1 = genotypeRatio gen1
  putStrLn "\nGenotypes ratio (F1):"
  mapM_
    (\(g, c) -> putStrLn $ prettyGenotype g ++ " : " ++ show c)
    (Map.toList countG1)

  let countF1 = phenotypeRatio gen1
  putStrLn "\nPhenotypes ratio (F1):"
  mapM_
    (\(g, c) -> putStrLn $ prettyGenotype g ++ " : " ++ show c)
    (Map.toList countG1)

  let probG1 = genotypeProbabilities gen1
  putStrLn "\nGenotypes probabilities (F1):"
  mapM_
    (\(g, p) -> putStrLn $ prettyGenotype g ++ " : " ++ show (p * 100) ++ "%")
    (Map.toList probG1)

  -- F2 by crossing F1 individuals pairwise
  let gen2 = computeNextGenerationFrom gen1

  let probG2 = genotypeProbabilities gen2
  putStrLn "\nGenotypes probabilities (F2):"
  mapM_
    (\(g, p) -> putStrLn $ prettyGenotype g ++ " : " ++ show (p * 100) ++ "%")
    (Map.toList probG2)

  let probF2 = phenotypeProbabilities gen2
  putStrLn "\nPhenotype probabilities (F2):"
  mapM_
    (\(g, p) -> putStrLn $ prettyGenotype g ++ " : " ++ show (p * 100) ++ "%")
    (Map.toList probF2)

  let f2 = getGenotypes gen2

  -- infer possible parent pairs for observed F1 phenotypes
  let f1Pheno = getGenerationToPhenotypes gen1
  let candidates = inferParentGenotypes f1Pheno

  putStrLn "\n--- Candidate parent genotype pairs for observed F1 phenotypes ---"
  mapM_ (\(p1, p2) -> putStrLn $ prettyGenotype p1 ++ " × " ++ prettyGenotype p2) (nub candidates)

  -- draw Punnett square for F1
  putStrLn "\n--- Punnett square for parents ---"
  drawPunnett f1
