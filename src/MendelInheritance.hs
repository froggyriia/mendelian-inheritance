{-# LANGUAGE TupleSections #-}

module MendelInheritance
  ( -- * Core types
    Allele,
    Gen,
    Genotype,
    Phenotype,
    Gamete,
    GametePool,
    Generation,

    -- * Safe smart-constructors
    makeAllele,
    isDominant,
    canonPair,
    makeGen,
    makeGenotype,
    makeGamete,
    makeGametePool,
    makeGeneration,
    alleleSymbol,
    unGenotype,
    extractAlleles,

    -- * Unsafe constructors (partial)
    unsafeAllele,
    unsafeGen,
    unsafeGenotype,
    unsafeGamete,
    unsafeGeneration,
    unsafePhenotype,

    -- * Getters
    getGeneLetter,
    getGametes,
    getPhenotypeTraits,
    getGenotypes,
    getTraitName,
    getAlleles,
    getTraitSpecification,
    getGenerationToPhenotypes,

    -- * Core functionality
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

    inferParentGenotypes

    computeNGenerations,
    computeNextGenerationFrom,

  )
where

import Data.Char (toLower, toUpper)
import Data.Function (on)
import Data.List (nub, nubBy, sortOn)
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.Map.Strict as Map
import GHC.Conc (par)

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
newtype Genotype = Genotype [Gen]
  deriving (Eq, Ord)

-- | A phenotype is the observable traits derived from a genotype
newtype Phenotype = Phenotype [(TraitName, Allele)]
  deriving (Eq, Ord)

-- | A gamete is a set of single alleles, one per trait, used in reproduction
newtype Gamete = Gamete [(TraitName, Allele)]
  deriving (Eq, Ord)

-- | A collection of gametes representing all reproductive options
newtype GametePool = GametePool [Gamete]
  deriving (Eq, Ord)

-- | A generation is a list of genotypes produced from crossing parents
newtype Generation = Generation [Genotype]
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
makeAllele :: Letter -> TraitSpecification -> Maybe Allele
makeAllele c spec
  | c >= 'A' && c <= 'Z' = Just (Dominant c spec)
  | c >= 'a' && c <= 'z' = Just (Recessive c spec)
  | otherwise = Nothing

-- | “Is this allele the dominant one?”
isDominant :: Allele -> Bool
isDominant (Dominant _ _) = True
isDominant _ = False

-- | Given a pair of alleles that are the same gene, always
-- put the Dominant one on the left.
canonPair :: (Allele, Allele) -> (Allele, Allele)
canonPair (a1, a2)
  | isDominant a2 && not (isDominant a1) = (a2, a1)
  | otherwise = (a1, a2)

-- | Construct a gene from a trait name and pair of alleles.
-- Only allowed if both alleles refer to the same gene letter (case-insensitive).
makeGen :: TraitName -> (Allele, Allele) -> Maybe Gen
makeGen name (a1, a2)
  | sameGene a1 a2 = Just (Gen name (canonPair (a1, a2)))
  | otherwise = Nothing
  where
    sameGene x y = toLower (geneLetter x) == toLower (geneLetter y)
    geneLetter (Dominant l _) = l
    geneLetter (Recessive l _) = l

-- | Construct a genotype from a list of genes.
-- Ensures each trait name is unique.
makeGenotype :: [Gen] -> Maybe Genotype
makeGenotype gens
  | length uniqueTraits == length gens =
      Just . Genotype $ sortOn geneKey gens
  | otherwise =
      Nothing
  where
    uniqueTraits = nubBy ((==) `on` getTraitName) gens

    -- for sorting we pull out the letter of the first allele, uppercase
    geneKey (Gen _ (a1, _)) = toUpper (getGeneLetter a1)

    getTraitName (Gen nm _) = nm

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
makeGametePool :: NonEmpty Gamete -> GametePool
makeGametePool gs = GametePool (toList gs)

-- | Construct a generation from a list of genotypes.
-- Fails if list is empty.
makeGeneration :: NonEmpty Genotype -> Generation
makeGeneration gens = Generation (toList gens)

unsafeAllele :: Letter -> TraitSpecification -> Allele
unsafeAllele c spec
  | c >= 'A' && c <= 'Z' = Dominant c spec
  | c >= 'a' && c <= 'z' = Recessive c spec
  | otherwise = error "The letter is not in A..Z a..z"

unsafeGen :: TraitName -> (Allele, Allele) -> Gen
unsafeGen name pair = Gen name pair

unsafeGenotype :: [Gen] -> Genotype
unsafeGenotype = Genotype

unsafeGamete :: [(TraitName, Allele)] -> Gamete
unsafeGamete = Gamete

unsafeGeneration :: [Genotype] -> Generation
unsafeGeneration = Generation

unsafePhenotype :: [(TraitName, Allele)] -> Phenotype
unsafePhenotype = Phenotype

-- Get functions

-- | Extract the “gene letter” from an allele
getGeneLetter :: Allele -> Letter
getGeneLetter (Dominant c _) = c
getGeneLetter (Recessive c _) = c

-- | Extracts list of gametes from a GametePool
getGametes :: GametePool -> [Gamete]
getGametes (GametePool gs) = gs

-- | Extracts list of genotypes from a Generation
getGenotypes :: Generation -> [Genotype]
getGenotypes (Generation gs) = gs

-- | Extract traits from phenotype
getPhenotypeTraits :: Phenotype -> [(TraitName, Allele)]
getPhenotypeTraits (Phenotype traits) = traits

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

-- | Get all phenotypes from a generation
getGenerationToPhenotypes :: Generation -> [Phenotype]
getGenerationToPhenotypes (Generation genotypes) = map phenotypeFromGenotype genotypes

-- Core functions

pairs :: [a] -> [(a, a)]
pairs ls =
  [ (x, y)
    | (i, x) <- zip [0 ..] ls,
      (j, y) <- zip [0 ..] ls,
      i < j
  ]

-- | Returns the dominant allele from a gene
dominantAllele :: Gen -> Allele
dominantAllele (Gen _ (a1, a2))
  | isDominant a1 = a1
  | otherwise = a2

-- | Builds all possible gametes from a genotype
gametesFromGenotype :: Genotype -> GametePool
gametesFromGenotype (Genotype gens) = GametePool (map unsafeGamete (buildGametes gens))
  where
    buildGametes [] = [[]]
    buildGametes (Gen name (a1, a2) : gs) =
      prepend (name, a1) (buildGametes gs)
        ++ prepend (name, a2) (buildGametes gs)
    prepend _ [] = []
    prepend pair (x : xs) = (pair : x) : prepend pair xs

-- | Combines two gametes to form a genotype
combineGametes :: Gamete -> Gamete -> Genotype
combineGametes (Gamete as1) (Gamete as2)
  | map fst as1 /= map fst as2 = error "Gametes must have the same traits in the same order"
  | otherwise = unsafeGenotype [unsafeGen name (a1, a2) | ((name, a1), (_, a2)) <- zip as1 as2]

-- | Crosses two gamete pools to generate all possible offspring genotypes
crossGametePools :: GametePool -> GametePool -> [Genotype]
crossGametePools (GametePool g1s) (GametePool g2s) =
  [ combineGametes g1 g2
    | g1 <- g1s,
      g2 <- g2s
  ]

-- | Crosses two parent genotypes to produce a generation of offspring
cross :: Genotype -> Genotype -> Generation
cross parent1 parent2 = unsafeGeneration (crossGametePools gp1 gp2)
  where
    gp1 = gametesFromGenotype parent1
    gp2 = gametesFromGenotype parent2

-- | Derives phenotype from genotype by choosing dominant allele for each gene
phenotypeFromGenotype :: Genotype -> Phenotype
phenotypeFromGenotype (Genotype gens) = unsafePhenotype (map toTrait gens)
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
uniqueGenotypes (Generation gens) = unsafeGeneration (removeDuplicates gens)
  where
    removeDuplicates [] = []
    removeDuplicates (x : xs)
      | x `elem` xs = removeDuplicates xs
      | otherwise = x : removeDuplicates xs

computeNextGenerationFrom :: Generation -> Generation
computeNextGenerationFrom (Generation individuals) =
  let pools = map gametesFromGenotype individuals
      parentPairs = pairs pools
      nextGeneration = [g | (gp1, gp2) <- parentPairs, g <- crossGametePools gp1 gp2]
   in unsafeGeneration nextGeneration

computeNGenerations :: Int -> Genotype -> Genotype -> Generation
computeNGenerations n parent1 parent2
  | n <= 1 = cross parent1 parent2
  | otherwise = computeNextGenerationFrom (computeNGenerations (n - 1) parent1 parent2)

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


-- | Infer possible parent genotype pairs that could produce the given phenotypes
inferParentGenotypes :: [Phenotype] -> [(Genotype, Genotype)]
inferParentGenotypes phenotypes =
    [ (p1, p2) 
    | p1 <- possibleGenotypes
    , p2 <- possibleGenotypes
    , all (`elem` offspringPhenotypes p1 p2) phenotypes
    ]
  where
    -- All possible genotypes that could produce any of the phenotypes
    possibleGenotypes = concatMap phenotypeToGenotypes phenotypes
    
    -- Convert phenotype to possible genotypes that could produce it
    phenotypeToGenotypes (Phenotype traits) =
        [ unsafeGenotype $ zipWith (Gen . fst) traits genePairs
        | genePairs <- sequence (map traitToGenes traits)
        ]
      where
        traitToGenes (_, a@(Dominant l _)) = 
            [(a,a), (a, Recessive (toLower l) (getTraitSpecification a))]
        traitToGenes (_, a@(Recessive _ _)) = [(a,a)]

    -- Generate all possible offspring phenotypes from two parent genotypes
    offspringPhenotypes p1 p2 = 
        map phenotypeFromGenotype $ 
        crossGametePools (gametesFromGenotype p1) (gametesFromGenotype p2)

alleleSymbol :: Allele -> Char
alleleSymbol (Dominant c _) = c
alleleSymbol (Recessive c _) = c


unGenotype :: Genotype -> [(String, (Allele, Allele))]
unGenotype (Genotype gens) = [(getTraitName g, getAlleles g) | g <- gens]

extractAlleles :: Genotype -> [(Char, Char)]
extractAlleles (Genotype gens) = map getAllelePair gens
  where
    getAllelePair (Gen _ (a1, a2)) = (getGeneLetter a1, getGeneLetter a2)

