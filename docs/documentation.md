# MendelInheritance: Library Documentation

This document provides a full reference for the types and functions in the `MendelInheritance` Haskell library.

---

## Data Types

### `Allele`

Represents a genetic allele. Can be:

- `Dominant Char TraitSpecification`
- `Recessive Char TraitSpecification`

**Example:**

```haskell
Just a = makeAllele 'A' "comb"
Just a' = makeAllele 'a' "no comb"
```

---

### `Gen`

A gene is a pair of alleles for a specific trait.

**Example:**

```haskell
Just a = makeAllele 'A' "comb"
Just a' = makeAllele 'a' "no comb"
Just gen = makeGen "comb" (a, a')
```

---

### `Genotype`

Represents an individual’s full genetic makeup: a list of genes.

**Example:**

```haskell
Just a = makeAllele 'A' "comb"
Just b = makeAllele 'B' "feathered legs"
Just gen1 = makeGen "comb" (a, a)
Just gen2 = makeGen "legs" (b, b)
Just gt = makeGenotype [gen1, gen2]
```

---

### `Phenotype`

Represents observable traits: a list of trait names and their expressed allele.

**Computed via:**

```haskell
phenotypeFromGenotype :: Genotype -> Phenotype
```

---

### `Gamete`

A collection of one allele per trait.

**Example:**

```haskell
Just a = makeAllele 'A' "comb"
Just b = makeAllele 'B' "feathered legs"
Just gam = makeGamete [("comb", a), ("legs", b)]
```

---

### `GametePool`

List of gametes generated from a genotype.

**Example:**

```haskell
gp = gametesFromGenotype gt
```

---

### `Generation`

A list of offspring genotypes resulting from a cross.

---

## Constructor Functions

### `makeAllele :: Char -> TraitSpecification -> Maybe Allele`

Creates an allele from a letter and description.

---

### `makeGen :: TraitName -> (Allele, Allele) -> Maybe Gen`

Creates a gene if both alleles have the same base letter.

---

### `makeGenotype :: [Gen] -> Maybe Genotype`

Creates a genotype if trait names are unique.

---

### `makeGamete :: [(TraitName, Allele)] -> Maybe Gamete`

Validates a gamete’s trait-allele list.

---

### `makeGametePool :: [Gamete] -> Maybe GametePool`

Fails if the list is empty.

---

### `makeGeneration :: [Genotype] -> Maybe Generation`

Fails if empty.

---

## Accessors

### `getGametes :: GametePool -> [Gamete]`

### `getGenotypes :: Generation -> [Genotype]`

### `getTraitName :: Gen -> TraitName`

### `getAlleles :: Gen -> (Allele, Allele)`

### `getTraitSpecification :: Allele -> TraitSpecification`

---

## Core Functions

### `dominantAllele :: Gen -> Allele`

Returns the dominant allele from a gene.

---

### `gametesFromGenotype :: Genotype -> GametePool`

Generates all combinations of gametes from a genotype.

---

### `combineGametes :: Gamete -> Gamete -> Maybe Genotype`

Combines two gametes to form a new genotype (if trait names match).

---

### `crossGametePools :: GametePool -> GametePool -> Maybe [Genotype]`

Produces all valid genotype combinations.

---

### `cross :: Genotype -> Genotype -> Maybe Generation`

Wraps the above in a single call.

---

### `phenotypeFromGenotype :: Genotype -> Phenotype`

Returns the expressed phenotype.

---

### `prettyGenotype :: Genotype -> String`

Returns string like `"AaBb"`.

---

### `prettyPhenotype :: Phenotype -> String`

Returns human-readable trait list.

**Example:**

```
comb: comb
legs: feathered legs
```

---

### `uniqueGenotypes :: Generation -> Generation`

Removes duplicates.

---

### `genotypeRatio :: Generation -> Map Genotype Int`

Counts genotype frequencies.

---

### `phenotypeRatio :: Generation -> Map Phenotype Int`

Counts phenotype frequencies.

---

### `pprintGeneration :: Generation -> IO ()`

Pretty-prints genotypes, phenotypes, and their ratios.

---

## Example: Use Case

```haskell
main :: IO ()
main = do
  let Just a = makeAllele 'A' "comb"
  let Just a' = makeAllele 'a' "no comb"
  let Just b = makeAllele 'B' "feathered legs"
  let Just b' = makeAllele 'b' "bare legs"

  let Just genAABB = makeGenotype =<< sequence
        [ makeGen "comb" (a, a),
          makeGen "legs" (b, b) ]

  let Just genAabb = makeGenotype =<< sequence
        [ makeGen "comb" (a, a'),
          makeGen "legs" (b', b') ]

  let Just f1 = cross genAABB genAabb
  pprintGeneration f1
```

This models:

- Parent 1: homozygous dominant
- Parent 2: heterozygous for comb, recessive for legs
- Result: All F1 hybrids have comb and feathered legs

---
