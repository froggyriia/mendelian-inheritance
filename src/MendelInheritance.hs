module MendelInheritance
  ( Allele (..),
    Gen (..),
    Genotype (..),
    Gamete (..),
    GametePool (..),
    dominantAllele,
    gametesFromGenotype,
    combineGametes,
    crossGametePools,
    cross,
  )
where

type TraitName = String

type TraitSpecification = String

type Letter = Char

data Allele
  = Dominant Letter TraitSpecification
  | Recessive Letter TraitSpecification
  deriving (Eq, Show)

data Gen = Gen TraitName (Allele, Allele)
  deriving (Eq, Show)

data Genotype = Genotype [Gen]
  deriving (Eq, Show)

data Phenotype = Phenotype [(TraitName, Allele)]
  deriving (Eq, Show)

data Gamete = Gamete [(TraitName, Allele)]
  deriving (Eq, Show)

data GametePool = GametePool [Gamete]
  deriving (Eq, Show)

data Generation = Generation [Genotype]
  deriving (Eq, Show)

dominantAllele :: Gen -> Allele
dominantAllele (Gen _ (a1, a2)) =
  case (a1, a2) of
    ((Dominant _ _), _) -> a1 -- first allele is dominant
    (_, (Dominant _ _)) -> a2 -- second allele is dominant
    (_, _) -> a1 -- both are recessive

traitOf :: Allele -> TraitSpecification
traitOf (Dominant _ trait) = trait
traitOf (Recessive _ trait) = trait

allelesOf :: Gen -> (Allele, Allele)
allelesOf (Gen _ alleles) = alleles

gametesFromGenotype :: Genotype -> GametePool
gametesFromGenotype (Genotype gens) = GametePool (map Gamete (buildGametes gens))
  where
    buildGametes :: [Gen] -> [[(TraitName, Allele)]]
    buildGametes [] = [[]]
    buildGametes (Gen name (a1, a2) : gs) =
      prepend (name, a1) (buildGametes gs)
        ++ prepend (name, a2) (buildGametes gs)

    prepend :: (TraitName, Allele) -> [[(TraitName, Allele)]] -> [[(TraitName, Allele)]]
    prepend _ [] = []
    prepend pair (x : xs) = (pair : x) : prepend pair xs

combineGametes :: Gamete -> Gamete -> Genotype
combineGametes (Gamete as1) (Gamete as2) = Genotype (buildGens as1 as2)
  where
    buildGens :: [(TraitName, Allele)] -> [(TraitName, Allele)] -> [Gen]
    buildGens [] [] = []
    buildGens ((name1, a1) : xs1) ((name2, a2) : xs2)
      | name1 == name2 = Gen name1 (a1, a2) : buildGens xs1 xs2
      | otherwise = error "Mismatched trait names in gametes"
    buildGens _ _ = error "Gametes have different lengths"

crossGametePools :: GametePool -> GametePool -> [Genotype]
crossGametePools (GametePool g1s) (GametePool g2s) = crossAll g1s g2s
  where
    crossAll :: [Gamete] -> [Gamete] -> [Genotype]
    crossAll [] _ = []
    crossAll (g : gs) others = combineWithAll g others ++ crossAll gs others

    combineWithAll :: Gamete -> [Gamete] -> [Genotype]
    combineWithAll _ [] = []
    combineWithAll g (x : xs) = combineGametes g x : combineWithAll g xs

cross :: Genotype -> Genotype -> Generation
cross parent1 parent2 = Generation (crossGametePools gp1 gp2)
  where
    gp1 = gametesFromGenotype parent1
    gp2 = gametesFromGenotype parent2

phenotypeFromGenotype :: Genotype -> Phenotype
phenotypeFromGenotype (Genotype gens) = Phenotype (buildPhenotype gens)
  where
    buildPhenotype [] = []
    buildPhenotype ((Gen name as) : gs) = (name, dominantAllele (Gen name as)) : buildPhenotype gs

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
