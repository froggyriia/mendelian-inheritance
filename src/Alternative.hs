module MyLib (
  Trait, 
  Gen, 
  Genotype, 
  Gamete, 
  GametePool, 
  Generation, 
  Species, 
  Specimen, 
  Allele, 
  mkSpecimen, 
  getGametePool, 
  gametesToGenotype, 
  cross) 
    where
      
import Data.Char (toUpper, toLower)
import Data.List (nub)
import Data.Maybe (maybeToList)

data Trait = Trait Char deriving (Eq)

data Gen = 
  Het Trait |
  HomoDom Trait |
  HomoRec Trait deriving (Eq)
  
instance Show Gen where
  show (Het (Trait c)) = [toUpper c, toLower c]
  show (HomoDom (Trait c)) = [toUpper c, toUpper c]
  show (HomoRec (Trait c)) = [toLower c, toLower c]

trait1 = Trait 'a'

data Allele = Dom Trait | Rec Trait deriving (Eq)

getTraitFromAllele :: Allele -> Trait
getTraitFromAllele (Dom t) = t
getTraitFromAllele (Rec t) = t


instance Show Allele where
  show (Dom (Trait c)) = show $ toUpper c
  show (Rec (Trait c)) = show $ toLower c
  
newtype Genotype = Genotype [Gen] deriving (Eq)
newtype Gamete = Gamete [Allele] deriving (Eq)
newtype GametePool = GametePool [Gamete] deriving (Eq)
newtype Generation = Generation [Genotype] deriving (Eq)

instance Show Genotype where
  show (Genotype gens) = concatMap show gens

instance Show Gamete where
  show (Gamete alleles) = concatMap show alleles

instance Show GametePool where
  show (GametePool gametes) = unwords $ map show gametes
  
instance Show Generation where
  show (Generation genotypes) = unwords $ map show genotypes
 
newtype Species = Species [Trait]
data Specimen = Specimen 
  { species :: Species
  , genotype :: Genotype
  }

mkSpecimen :: Species -> Genotype -> Maybe Specimen
mkSpecimen (Species speciesTraits) (Genotype gens) =
  if checkCompatibility speciesTraits gens
    then Just $ Specimen (Species speciesTraits) (Genotype gens)
    else Nothing
  where
    checkCompatibility :: [Trait] -> [Gen] -> Bool
    checkCompatibility traits gens =
      length traits == length gens &&
      and (zipWith matchesTrait traits gens)
    
    matchesTrait trait (Het t) = t == trait
    matchesTrait trait (HomoDom t) = t == trait
    matchesTrait trait (HomoRec t) = t == trait

getGametePool :: Genotype -> GametePool
getGametePool (Genotype gens) = 
  GametePool $ map Gamete $ combineAlleles (map getAlleles gens)
    where
      getAlleles :: Gen -> [Allele]
      getAlleles (Het t) = [Dom t, Rec t]          
      getAlleles (HomoDom t) = [Dom t]            
      getAlleles (HomoRec t) = [Rec t]  
      combineAlleles :: [[Allele]] -> [[Allele]]
      combineAlleles [] = [[]]
      combineAlleles (alleles:rest) = [a:combo | 
        a <- alleles, combo <- combineAlleles rest]

gametesToGenotype :: Gamete -> Gamete -> Maybe Genotype
gametesToGenotype (Gamete a1) (Gamete a2)
  | length a1 /= length a2 = Nothing
  | otherwise = Just $ Genotype $ zipWith combine a1 a2
  where
    combine (Dom t1) (Dom t2) 
      | t1 == t2  = HomoDom t1
      | otherwise = Het t1
    combine (Dom t1) (Rec t2) = Het t1
    combine (Rec t1) (Dom t2) = Het t2
    combine (Rec t1) (Rec t2)
      | t1 == t2  = HomoRec t1
      | otherwise = Het t1

cross :: Genotype -> Genotype -> Generation
cross parent1 parent2 = Generation $ nub $ catMaybes offspring
  where
    (GametePool gametes1) = getGametePool parent1
    (GametePool gametes2) = getGametePool parent2
    offspring = [gametesToGenotype g1 g2 | g1 <- gametes1, g2 <- gametes2]
    catMaybes = concatMap maybeToList

parent1 = Genotype [Het (Trait 'a'), HomoDom (Trait 'b')]  -- AaBB
parent2 = Genotype [Het (Trait 'a'), HomoRec (Trait 'b')]  -- Aabb

main :: IO()
main = do print $ ( cross parent1 parent2)