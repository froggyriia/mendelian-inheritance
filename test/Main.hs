import Data.Complex (phase)
import MendelInheritance

-- import Test.HUnit

-- Простые аллели
a = Dominant 'A' "comb is present"

a' = Recessive 'a' "comb is not present"

b = Dominant 'B' "legs are present"

b' = Recessive 'b' "legs are not present"

-- Гены
gen1 = Gen "comb" (a, a') -- Aa

gen2 = Gen "comb" (a', a') -- aa

gen3 = Gen "legs" (b', b) -- bB

gen4 = Gen "legs" (b', b') -- bb

g1 = Genotype [gen1, gen3]

g2 = Genotype [gen2, gen4]

-- -- Тесты
-- tests :: Test
-- tests =
--   TestList
--     [ "dominant from Aa" ~: dominantAllele gen1 ~?= a,
--       "dominant from aa" ~: dominantAllele gen2 ~?= a',
--       "dominant from bB" ~: dominantAllele gen3 ~?= b,
--       "dominant from bb" ~: dominantAllele gen4 ~?= b'
--     ]
f1_genotypes = cross g1 g2

f1_unique = uniqueGenotypes f1_genotypes

f1_phenotypes = map phenotypeFromGenotype (case f1_unique of (Generation gens) -> gens)

main = do
  putStrLn "=== Уникальные генотипы и их количество ==="
  mapM_ printGenotypeRatio (countGenotypeRatios f1_genotypes)

  putStrLn "\n=== Уникальные фенотипы и их количество ==="
  mapM_ printPhenotypeRatio (countPhenotypeRatios f1_genotypes)

-- Печать результатов
printGenotypeRatio :: (String, Int) -> IO ()
printGenotypeRatio (gen, n) = putStrLn (gen ++ " — " ++ show n)

printPhenotypeRatio :: (String, Int) -> IO ()
printPhenotypeRatio (phen, n) = putStrLn (phen ++ " — " ++ show n)