module Main where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import MendelInheritance
import MendelInheritance.Probability
  ( genotypeProbabilities,
    genotypeRatio,
    phenotypeProbabilities,
    phenotypeRatio,
  )
import MendelInheritance.PunnettGloss

main :: IO ()
main = do
  -- Alleles:
  let Just a = makeAllele 'A' "comb"
  let Just a' = makeAllele 'a' "no comb"
  let Just b = makeAllele 'B' "feathered legs"
  let Just b' = makeAllele 'b' "bare legs"

  -- Parents: Rooster (AABB) × Hen (Aabb)
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

  putStrLn "\n--- F1 Generation ---"
  mapM_ (putStrLn . prettyGenotype) (getGenotypes gen1)

  putStrLn "\nGenotypes ratio (F1):"
  mapM_ (\(g, c) -> putStrLn $ prettyGenotype g ++ " : " ++ show c) (Map.toList (genotypeRatio gen1))

  putStrLn "\nPhenotypes ratio (F1):"
  mapM_ (\(p, c) -> putStrLn $ prettyPhenotype p ++ " : " ++ show c) (Map.toList (phenotypeRatio gen1))

  putStrLn "\nGenotypes probabilities (F1):"
  mapM_ (\(g, p) -> putStrLn $ prettyGenotype g ++ " : " ++ show (fromRational p * 100) ++ "%") (Map.toList (genotypeProbabilities gen1))

  -- F2 generation (self-cross F1 individuals, classic Mendel cross)
  let gen2 = selfCrossGeneration gen1

  putStrLn "\n--- F2 Generation (Self-cross F1) ---"
  putStrLn "\nGenotypes probabilities (F2):"
  mapM_ (\(g, p) -> putStrLn $ prettyGenotype g ++ " : " ++ show (fromRational p * 100) ++ "%") (Map.toList (genotypeProbabilities gen2))

  putStrLn "\nPhenotype probabilities (F2):"
  mapM_ (\(p, pval) -> putStrLn $ prettyPhenotype p ++ " : " ++ show (fromRational pval * 100) ++ "%") (Map.toList (phenotypeProbabilities gen2))

  -- F3 generation (random mating with gamete frequencies from F2)
  let gen3 = nextGenerationByGameteFrequencies gen2

  putStrLn "\n--- F3 Generation (Random mating by gamete frequencies from F2) ---"
  putStrLn "\nGenotypes probabilities (F3):"
  mapM_ (\(g, p) -> putStrLn $ prettyGenotype g ++ " : " ++ show (fromRational p * 100) ++ "%") (Map.toList (genotypeProbabilities gen3))

  putStrLn "\nPhenotype probabilities (F3):"
  mapM_ (\(p, pval) -> putStrLn $ prettyPhenotype p ++ " : " ++ show (fromRational pval * 100) ++ "%") (Map.toList (phenotypeProbabilities gen3))

  -- F4 generation (simulate further random mating up to F4)
  let gen4 = computeNGameteGenerations 4 gen1

  putStrLn "\n--- F4 Generation (Random mating by gamete frequencies up to F4) ---"
  putStrLn "\nGenotypes probabilities (F4):"
  mapM_ (\(g, p) -> putStrLn $ prettyGenotype g ++ " : " ++ show (fromRational p * 100) ++ "%") (Map.toList (genotypeProbabilities gen4))

  putStrLn "\nPhenotype probabilities (F4):"
  mapM_ (\(p, pval) -> putStrLn $ prettyPhenotype p ++ " : " ++ show (fromRational pval * 100) ++ "%") (Map.toList (phenotypeProbabilities gen4))

  -- Candidate parent pairs for observed F1 phenotypes (optional)
  let f1Pheno = getGenerationToPhenotypes gen1
  let candidates = inferParentGenotypes f1Pheno

  putStrLn "\n--- Candidate parent genotype pairs for observed F1 phenotypes ---"
  mapM_ (\(p1, p2) -> putStrLn $ prettyGenotype p1 ++ " × " ++ prettyGenotype p2) (nub candidates)

  -- Draw Punnett square for F1
  putStrLn "\n--- Punnett square for F1 ---"
  drawPunnett (getGenotypes gen1)
