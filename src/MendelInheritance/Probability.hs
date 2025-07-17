{-# LANGUAGE TupleSections #-}

module MendelInheritance.Probability where

import qualified Data.Map.Strict as Map
import Data.Ratio (Ratio, (%))
import MendelInheritance (Generation, Genotype, Phenotype, getGenotypes, phenotypeFromGenotype)

type Probability = Ratio Int

-- частота каждого генотипа
genotypeRatio :: Generation -> Map.Map Genotype Int
genotypeRatio generation =
  Map.fromListWith (+) [(g, 1) | g <- getGenotypes generation]

-- частота каждого фенотипа
phenotypeRatio :: Generation -> Map.Map Phenotype Int
phenotypeRatio generation =
  Map.fromListWith (+) [(phenotypeFromGenotype g, 1) | g <- getGenotypes generation]

genotypeProbabilities :: Generation -> Map.Map Genotype Probability
genotypeProbabilities generation =
  let counts = genotypeRatio generation
      total = length (getGenotypes generation)
   in Map.map (\c -> c % total) counts

phenotypeProbabilities :: Generation -> Map.Map Phenotype Probability
phenotypeProbabilities generation =
  let counts = phenotypeRatio generation
      total = length (getGenotypes generation)
   in Map.map (\c -> c % total) counts
