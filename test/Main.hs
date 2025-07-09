import MendelInheritance
import Test.HUnit

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

-- Тесты
tests :: Test
tests =
  TestList
    [ "dominant from Aa" ~: dominantAllele gen1 ~?= a,
      "dominant from aa" ~: dominantAllele gen2 ~?= a',
      "dominant from bB" ~: dominantAllele gen3 ~?= b,
      "dominant from bb" ~: dominantAllele gen4 ~?= b'
    ]

main :: IO ()
main = runTestTT tests >> return ()
