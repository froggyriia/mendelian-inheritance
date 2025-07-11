module MendelInheritance
  ( Allele,
    Gen,
    Genotype,
    Gamete,
    GametePool,
    Generation,
    makeAllele,
    makeGen,
    makeGenotype,
    makeGamete,
    makeGametePool,
    makeGeneration,
    getGametes,
    getGenotypes,
    getTraitName,
    getAlleles,
    getTraitSpecification,
    dominantAllele,
    gametesFromGenotype,
    combineGametes,
    crossGametePools,
    cross,
    phenotypeFromGenotype,
    prettyGenotype,
    prettyPhenotype,
    uniqueGenotypes,
    genotypeRatio,
    phenotypeRatio,
    pprintGeneration,
  )
where

import Data.Char (toLower)
import Data.Function (on)
import Data.List (nub, nubBy)
import qualified Data.Map.Strict as Map

-- Type synonyms

-- | TraitName is the name of a characteristic (e.g., "comb", "legs")
type TraitName = String

-- | TraitSpecification is a description of what a particular allele codes for
type TraitSpecification = String

-- | Letter represents a gene symbol (e.g., 'A', 'a')
type Letter = Char

-- Data types

-- | An allele can be dominant or recessive, each with a symbol and trait description
data Allele
  = Dominant Letter TraitSpecification
  | Recessive Letter TraitSpecification
  deriving (Eq, Ord)

-- | A pair of alleles for a single trait
data Gen = Gen TraitName (Allele, Allele)
  deriving (Eq, Ord)

-- | A genotype is a list of genes representing a full genetic profile
data Genotype = Genotype [Gen]
  deriving (Eq, Ord)

-- | A phenotype is the observable traits derived from a genotype
data Phenotype = Phenotype [(TraitName, Allele)]
  deriving (Eq, Ord)

-- | A gamete is a set of single alleles, one per trait, used in reproduction
data Gamete = Gamete [(TraitName, Allele)]
  deriving (Eq, Ord)

-- | A collection of gametes representing all reproductive options
data GametePool = GametePool [Gamete]
  deriving (Eq, Ord)

-- | A generation is a list of genotypes produced from crossing parents
data Generation = Generation [Genotype]
  deriving (Eq, Ord)

-- Instances Show
instance Show Allele where
  show (Dominant letter _) = [letter]
  show (Recessive letter _) = [letter]

instance Show Gen where
  show (Gen _ (allele1, allele2)) = show allele1 ++ show allele2

instance Show Genotype where
  show (Genotype genes) = concatMap show genes

instance Show Phenotype where
  show (Phenotype []) = ""
  show (Phenotype ((traitname, a) : xs)) = show a ++ show (Phenotype xs)

instance Show Gamete where
  show (Gamete []) = ""
  show (Gamete ((traitname, a) : as)) = show a ++ show (Gamete as)

instance Show GametePool where
  show (GametePool gametes) = unwords (map show gametes)

instance Show Generation where
  show (Generation genotypes) = unwords (map show genotypes)

-- Make functions

-- | Construct an allele from a letter and trait specification.
-- Uppercase letters produce Dominant, lowercase Recessive.
makeAllele :: Char -> TraitSpecification -> Maybe Allele
makeAllele c spec
  | c >= 'A' && c <= 'Z' = Just (Dominant c spec)
  | c >= 'a' && c <= 'z' = Just (Recessive c spec)
  | otherwise = Nothing

-- | Construct a gene from a trait name and pair of alleles.
-- Only allowed if both alleles refer to the same gene letter (case-insensitive).
makeGen :: TraitName -> (Allele, Allele) -> Maybe Gen
makeGen name (a1, a2)
  | sameGene a1 a2 = Just (Gen name (a1, a2))
  | otherwise = Nothing
  where
    sameGene x y = toLower (geneLetter x) == toLower (geneLetter y)
    geneLetter (Dominant l _) = l
    geneLetter (Recessive l _) = l

-- | Construct a genotype from a list of genes.
-- Ensures each trait name is unique.
makeGenotype :: [Gen] -> Maybe Genotype
makeGenotype gens
  | length uniqueNames == length gens = Just (Genotype gens)
  | otherwise = Nothing
  where
    uniqueNames = nubBy ((==) `on` getName) gens
    getName (Gen name _) = name

-- | Construct a gamete from a list of trait-allele pairs.
-- Ensures trait names are unique in the gamete.
makeGamete :: [(TraitName, Allele)] -> Maybe Gamete
makeGamete alleles
  | length names == length (nub names) = Just (Gamete alleles)
  | otherwise = Nothing
  where
    names = map fst alleles

-- | Construct a gamete pool from a list of gametes.
-- Fails if list is empty.
makeGametePool :: [Gamete] -> Maybe GametePool
makeGametePool gs
  | not (null gs) = Just (GametePool gs)
  | otherwise = Nothing

-- | Construct a generation from a list of genotypes.
-- Fails if list is empty.
makeGeneration :: [Genotype] -> Maybe Generation
makeGeneration gens
  | not (null gens) = Just (Generation gens)
  | otherwise = Nothing

-- Get functions

-- | Extracts list of gametes from a GametePool
getGametes :: GametePool -> [Gamete]
getGametes (GametePool gs) = gs

-- | Extracts list of genotypes from a Generation
getGenotypes :: Generation -> [Genotype]
getGenotypes (Generation gs) = gs

-- | Gets the trait name from a gene
getTraitName :: Gen -> TraitName
getTraitName (Gen name _) = name

-- | Gets the pair of alleles from a gene
getAlleles :: Gen -> (Allele, Allele)
getAlleles (Gen _ pair) = pair

-- | Gets the descriptive trait specification from an allele
getTraitSpecification :: Allele -> TraitSpecification
getTraitSpecification (Dominant _ trait) = trait
getTraitSpecification (Recessive _ trait) = trait

-- Core functions

-- | Returns the dominant allele from a gene
dominantAllele :: Gen -> Allele
dominantAllele (Gen _ (a1, a2)) =
  case (a1, a2) of
    (Dominant _ _, _) -> a1
    (_, Dominant _ _) -> a2
    _ -> a1

-- | Builds all possible gametes from a genotype
gametesFromGenotype :: Genotype -> GametePool
gametesFromGenotype (Genotype gens) = GametePool (map Gamete (buildGametes gens))
  where
    buildGametes [] = [[]]
    buildGametes (Gen name (a1, a2) : gs) =
      prepend (name, a1) (buildGametes gs)
        ++ prepend (name, a2) (buildGametes gs)
    prepend _ [] = []
    prepend pair (x : xs) = (pair : x) : prepend pair xs

-- | Combines two gametes to form a genotype (if valid)
combineGametes :: Gamete -> Gamete -> Maybe Genotype
combineGametes (Gamete as1) (Gamete as2) = fmap Genotype (buildGens as1 as2)
  where
    buildGens [] [] = Just []
    buildGens ((name1, a1) : xs1) ((name2, a2) : xs2)
      | name1 == name2 = do
          rest <- buildGens xs1 xs2
          return (Gen name1 (a1, a2) : rest)
      | otherwise = Nothing
    buildGens _ _ = Nothing

-- | Crosses two gamete pools to generate all possible offspring genotypes
crossGametePools :: GametePool -> GametePool -> Maybe [Genotype]
crossGametePools (GametePool g1s) (GametePool g2s) = fmap concat (sequence (crossAll g1s g2s))
  where
    crossAll [] _ = []
    crossAll (g : gs) others = combineWithAll g others : crossAll gs others
    combineWithAll _ [] = Just []
    combineWithAll g (x : xs) = do
      fstGen <- combineGametes g x
      rest <- combineWithAll g xs
      return (fstGen : rest)

-- | Crosses two parent genotypes to produce a generation of offspring
cross :: Genotype -> Genotype -> Maybe Generation
cross parent1 parent2 = fmap Generation (crossGametePools gp1 gp2)
  where
    gp1 = gametesFromGenotype parent1
    gp2 = gametesFromGenotype parent2

-- | Derives phenotype from genotype by choosing dominant allele for each gene
phenotypeFromGenotype :: Genotype -> Phenotype
phenotypeFromGenotype (Genotype gens) = Phenotype (map toTrait gens)
  where
    toTrait g@(Gen name _) = (name, dominantAllele g)

-- | Produces a compact string like "AaBb" from a genotype
prettyGenotype :: Genotype -> String
prettyGenotype (Genotype gens) = concatMap showPair gens
  where
    showAllele (Dominant l _) = [l]
    showAllele (Recessive l _) = [l]
    showPair (Gen _ (a1, a2)) = showAllele a1 ++ showAllele a2

-- | Produces a readable string of traits like "comb: red"
prettyPhenotype :: Phenotype -> String
prettyPhenotype (Phenotype traits) = unlines (map showTrait traits)
  where
    showTrait (name, allele) = name ++ ": " ++ getTraitSpecification allele

-- | Removes duplicate genotypes from a generation
uniqueGenotypes :: Generation -> Generation
uniqueGenotypes (Generation gens) = Generation (removeDuplicates gens)
  where
    removeDuplicates [] = []
    removeDuplicates (x : xs)
      | x `elem` xs = removeDuplicates xs
      | otherwise = x : removeDuplicates xs

-- | Computes how many times each genotype appears in a generation
genotypeRatio :: Generation -> Map.Map Genotype Int
genotypeRatio (Generation gens) = Map.fromListWith (+) (map (\g -> (g, 1)) gens)

-- | Computes how many times each phenotype appears in a generation
phenotypeRatio :: Generation -> Map.Map Phenotype Int
phenotypeRatio (Generation gens) = Map.fromListWith (+) (map (\g -> (phenotypeFromGenotype g, 1)) gens)

-- | Pretty-prints a generation with genotypes, phenotypes, and their ratios
pprintGeneration :: Generation -> IO ()
pprintGeneration gen = do
  -- extracting genotypes from generation
  let genList = getGenotypes gen
  let uniqueGenList = getGenotypes $ uniqueGenotypes gen

  putStrLn "\n--- First generation ---"

  mapM_
    ( \g -> do
        putStrLn $ "Genotype: " ++ prettyGenotype g
        putStrLn $ "Phenotype:\n" ++ prettyPhenotype (phenotypeFromGenotype g)
    )
    uniqueGenList

  -- Then print genotype ratio
  putStrLn "\n--- Genotype ratio ---"
  Map.foldrWithKey
    (\g n acc -> putStrLn (prettyGenotype g ++ " : " ++ show n) >> acc)
    (return ())
    (genotypeRatio gen)
  -- Then print phenotype ratio
  putStrLn "\n--- Phenotype ratio ---"
  Map.foldrWithKey
    (\g n acc -> putStrLn (prettyPhenotype g ++ " : " ++ show n) >> acc)
    (return ())
    (phenotypeRatio gen)
