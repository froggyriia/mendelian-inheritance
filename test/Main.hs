module Main where

import MendelInheritance

main :: IO ()
main = do
  -- Аллели
  let Just a = makeAllele 'A' "гребень"
  let Just a' = makeAllele 'a' "нет гребня"
  let Just b = makeAllele 'B' "оперённые ноги"
  let Just b' = makeAllele 'b' "голые ноги"

  -- Родительские генотипы: AABB × Aabb
  let Just genAABB =
        makeGenotype
          =<< sequence
            [ makeGen "comb" (a, a),
              makeGen "legs" (b, b)
            ]
  let Just genAabb =
        makeGenotype
          =<< sequence
            [ makeGen "comb" (a, a'),
              makeGen "legs" (b', b')
            ]

  putStrLn "\n--- Родители ---"
  putStrLn "Петух (AABB):"
  putStrLn $ "Генотип: " ++ prettyGenotype genAABB
  putStrLn $ "Фенотип:\n" ++ prettyPhenotype (phenotypeFromGenotype genAABB)

  putStrLn "\nКурица (Aabb):"
  putStrLn $ "Генотип: " ++ prettyGenotype genAabb
  putStrLn $ "Фенотип:\n" ++ prettyPhenotype (phenotypeFromGenotype genAabb)

  -- Первое поколение
  let Just gen1 = cross genAABB genAabb
  let gen1List = getGenotypes gen1

  putStrLn "\n--- Первое поколение ---"
  mapM_
    ( \g -> do
        putStrLn $ "Генотип: " ++ prettyGenotype g
        putStrLn $ "Фенотип:\n" ++ prettyPhenotype (phenotypeFromGenotype g)
    )
    gen1List
    putStrLn
    "\n--- Соотношение по генотипам ---"
    Map.foldrWithKey
    (\g n acc -> putStrLn (prettyGenotype g ++ " : " ++ show n) >> acc)
    (return ())
    (genotypeRatio gen1)
