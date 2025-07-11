# MendelianInheritance

A Haskell library for modeling classical Mendelian inheritance — simulating how traits pass from parents to offspring based on genotype and dominance rules.

👩‍🔬 By: Alina Khisamutdinova, Elizaveta Bubnova, Victoriia Gorbacheva  
📘 [Documentation](./docs/index.md)

---

## Project Goals and Description

This project implements a Haskell library that models genetic inheritance following Mendel's laws of segregation and independent assortment.

It enables users to:

- Define custom traits (like "comb" or "feathered legs")
- Create alleles and combine them into genotypes
- Simulate crosses between individuals (P generation)
- Calculate resulting genotypes and phenotypes for offspring (F1 generation)
- View trait ratios across the resulting generation

The library is minimal but extendable. It is designed to support both educational use cases and automated problem-solving for classical genetics tasks.

---

## Feature Roadmap

### Stage I (Complete)

- [x] Core data structures: Allele, Gen, Genotype, Phenotype, Gamete, Generation
- [x] Custom trait support (`TraitName`, `TraitSpecification`)
- [x] Genotype creation and validation
- [x] Cross two genotypes and generate first generation (`cross`)
- [x] Compute phenotype and genotype ratios
- [x] Solve typical Mendelian problems
- [x] Pretty-printing of genotypes and phenotypes

### Stage II (Planned)

- [ ] Multi-generational simulation (assigned to Victoriia)
- [ ] Punnett square visualization (assigned to Silvia)
- [ ] Parental genotype inference (assigned to Alina)
- [ ] Support for partial/incomplete dominance

---

## Usage Example

This example solves a classical inheritance problem:

**Problem**: Cross a homozygous rooster with comb (A) and feathered legs (B) — genotype `AABB` —  
with a heterozygous hen with comb and bare legs — genotype `Aabb`.  
Determine genotypes and phenotypes of offspring.

```haskell
import MendelInheritance

main :: IO ()
main = do
  -- Define alleles with visible traits
  let Just a = makeAllele 'A' "comb"
  let Just a' = makeAllele 'a' "no comb"
  let Just b = makeAllele 'B' "feathered legs"
  let Just b' = makeAllele 'b' "bare legs"

  -- Parent genotypes: AABB × Aabb
  let Just genAABB =
        makeGenotype =<< sequence
          [ makeGen "comb" (a, a),
            makeGen "legs" (b, b)
          ]

  let Just genAabb =
        makeGenotype =<< sequence
          [ makeGen "comb" (a, a'),
            makeGen "legs" (b', b')
          ]

  -- Print parental genotypes and phenotypes
  putStrLn "Parent 1 (AABB):"
  putStrLn $ prettyGenotype genAABB
  putStrLn $ prettyPhenotype (phenotypeFromGenotype genAABB)

  putStrLn "\nParent 2 (Aabb):"
  putStrLn $ prettyGenotype genAabb
  putStrLn $ prettyPhenotype (phenotypeFromGenotype genAabb)

  -- Cross parents → get F1 generation
  let Just gen1 = cross genAABB genAabb

  -- Display all unique genotype and phenotype combinations
  pprintGeneration gen1
```

## License

This project is licensed under the [MIT License](LICENSE).
