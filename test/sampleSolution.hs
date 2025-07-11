import MendelInheritance

main :: IO()
main = do
    -- Alleles
    let Just a = makeAllele 'A' "comb"
    let Just a' = makeAllele 'a' "no comb"
    let Just b = makeAllele 'B' "feather-legged"
    let Just b' = makeAllele 'b' "non-feathered legs"

    -- Parents: AABB × Aabb
    let Just genAABB =
        makeGenotype
            =<< sequence
                [ makeGene "comb" (a, a),
                    makeGene "legs" (b, b)
                ]
    let Just genAabb =
        makeGenotype
            =<< sequence
                [ makeGene "comb" (a, a'),
                    makeGene "legs" (b', b')
                ]

    -- Crossing
    let Just gene1 = cross genAABB genAabb
    
    -- Pretty print of generation solution
    pprintGeneration gene1
