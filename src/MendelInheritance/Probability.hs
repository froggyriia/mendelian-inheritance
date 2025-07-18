{-# LANGUAGE TupleSections #-}

module MendelInheritance.Probability where

import qualified Data.Map.Strict as Map
import Data.Ratio (Ratio, (%))
import MendelInheritance

-- | Probability type as a rational number of counts over total population.
type Probability = Ratio Int

-- | Count occurrences of each Genotype in a Generation
genotypeRatio :: Generation -> Map.Map Genotype Int
genotypeRatio generation =
  Map.fromListWith (+) [(g, 1) | g <- getGenotypes generation]

-- | Count occurrences of each Phenotype in a Generation
phenotypeRatio :: Generation -> Map.Map Phenotype Int
phenotypeRatio generation =
  Map.fromListWith (+) [(phenotypeFromGenotype g, 1) | g <- getGenotypes generation]

-- | Convert genotype counts to probabilities by dividing by total individuals
genotypeProbabilities :: Generation -> Map.Map Genotype Probability
genotypeProbabilities generation =
  let counts = genotypeRatio generation
      total = length (getGenotypes generation)
   in Map.map (\c -> c % total) counts

-- | Convert phenotype counts to probabilities by dividing by total individuals
phenotypeProbabilities :: Generation -> Map.Map Phenotype Probability
phenotypeProbabilities generation =
  let counts = phenotypeRatio generation
      total = length (getGenotypes generation)
   in Map.map (\c -> c % total) counts
