# MendelianInheritance

A Haskell library for modeling classical Mendelian inheritance — simulating how traits pass from parents to offspring based on genotype and dominance rules.

By: Alina Khisamutdinova, Elizaveta Bubnova, Victoriia Gorbacheva  
[Documentation](./docs/documentation.md) <- NOT UP-TO-DATE

---

## Project Goals and Description

This project implements a Haskell library that models genetic inheritance following Mendel's laws of segregation and independent assortment.

It enables users to:

- Define custom traits (like "comb" or "feathered legs")
- Create alleles and combine them into genotypes
- Simulate crosses between individuals (P generation)
- Calculate resulting genotypes and phenotypes for n generations
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

### Stage II (Complete)

- [x] Multi-generational simulation (assigned to Victoriia)
- [x] Punnett square visualization (assigned to Silvia)
- [x] Parental genotype inference (assigned to Alina)

### Future work

- [ ] Support for partial/incomplete dominance

---

## Usage Example

For example, please refer to test/Main.hs

## License

This project is licensed under the [MIT License](LICENSE).
