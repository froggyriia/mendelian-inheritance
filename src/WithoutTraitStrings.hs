module WithoutTraitStrings (
  Allele, 
  Gen,
  Genotype,
  Phenotype,
  Gamete,
  GametePool,
  expressedAllele,
  getPhenotype,
  getGametePool,
  cross,
  applyGetPhenotypeOnGeneration
) where

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.List (nub)

type Letter = Char

data Allele = Dominant Letter | Recessive Letter deriving (Eq, Ord)

data Gen = Gen Allele Allele deriving (Eq, Ord)

newtype Genotype = Genotype [Gen]
newtype Phenotype = Phenotype [Allele]
newtype Gamete = Gamete [Allele] deriving (Eq, Ord)
newtype GametePool = GametePool [Gamete] deriving (Show)
newtype Generation = Generation [Genotype]

instance Show Allele where
  show (Dominant letter) = [letter]
  show (Recessive letter) = [letter]

instance Show Gen where
  show (Gen allele1 allele2) = show allele1 ++ show allele2

instance Show Genotype where
  show (Genotype gens) = concatMap show gens

instance Show Phenotype where
  show (Phenotype alleles) = concatMap show alleles
  
instance Show Gamete where
  show (Gamete alleles) = concatMap show alleles

instance Show Generation where
  show (Generation gs) = show gs

-- Example genotypes
--gen1 = Gen (Dominant 'A') (Recessive 'a')
--gen2 = Gen (Recessive 'b') (Recessive 'b')
--gen3 = Gen (Recessive 'c') (Dominant 'C')

g--enotype1 = Genotype [gen1, gen2, gen3]

expressedAllele :: Gen -> Allele
expressedAllele (Gen allele1 allele2) = 
    case allele1 of
        Dominant _ -> allele1
        Recessive _ -> allele2

getPhenotype :: Genotype -> Phenotype
getPhenotype (Genotype gens) = Phenotype (map expressedAllele gens)

getGametePool :: Genotype -> GametePool
getGametePool (Genotype gens) = GametePool $ nub $ map Gamete $ sequence possibleAlleles
  where
    possibleAlleles = [ [a1, a2] | Gen a1 a2 <- gens ]

cross :: Genotype -> Genotype -> Generation
cross g1 g2 = Generation offspring
  where
    (GametePool pool1) = getGametePool g1
    (GametePool pool2) = getGametePool g2
    offspring = [ Genotype (zipWith Gen alleles1 alleles2) 
               | Gamete alleles1 <- pool1
               , Gamete alleles2 <- pool2
               ]

applyGetPhenotypeOnGeneration :: Generation -> [Phenotype]
applyGetPhenotypeOnGeneration (Generation gs) = map getPhenotype gs

--parent1 :: Genotype
--parent1 = Genotype [Gen (Dominant 'A') (Recessive 'a'), Gen (Dominant 'B') (Dominant 'B')]

--parent2 :: Genotype
--parent2 = Genotype [Gen (Dominant 'A') (Recessive 'a'), Gen (Recessive 'b') (Recessive 'b')]

--main :: IO()
--main = do 
--    print gen1
--    print $ expressedAllele gen3
--    print genotype1
--    print $ getPhenotype genotype1
--    print $ cross parent1 parent2
--    print $ applyGetPhenotypeOnGeneration (cross parent1 parent2)