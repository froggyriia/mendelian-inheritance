import System.IO (hFlush, stdout)
import Control.Monad (forM, forM_, when)
import Data.Char (isUpper, toLower, toUpper)
import Data.List (intercalate, foldl')
import Text.Read (readMaybe)

-- Типы данных
type Trait = String
type TraitName = String
type Letter = Char

data Allele = Dominant Letter Trait | Recessive Letter Trait deriving (Show, Eq)
data Gen = Gen TraitName Allele Allele deriving (Show)
data Genotype = Genotype [Gen] deriving (Show)
data Gamete = Gamete [Allele] deriving (Show)
data Individual = Individual Genotype 
data Parents = Parents Individual Individual deriving (Show)
data Generation = Generation [Individual] 

-- Экземпляры Show для красивого вывода
instance Show Individual where
    show (Individual (Genotype genes)) =
        let phenotype = map getPhenotype genes
            genotypeStr = concatMap (\(Gen _ a1 a2) -> [alleleChar a1, alleleChar a2]) genes
            phenotypeStr = intercalate " " phenotype
        in genotypeStr ++ " - " ++ phenotypeStr
      where
        alleleChar (Dominant c _) = toUpper c
        alleleChar (Recessive c _) = toLower c
        getPhenotype (Gen _ (Dominant _ t) _) = t  -- Доминантный признак
        getPhenotype (Gen _ _ (Recessive _ t)) = t -- Рецессивный признак

instance Show Generation where
    show (Generation individuals) = 
        "F1:\n" ++ intercalate "\n" (map show individuals)

-- Основные функции
main :: IO ()
main = do
    putStrLn "=== Регистрация генов ==="
    genes <- registerGenes
    
    putStrLn "\n=== Ввод родителей ==="
    parent1 <- createAnimal genes "Родитель 1"
    parent2 <- createAnimal genes "Родитель 2"
    
    let parents = Parents parent1 parent2
    putStrLn "\n=== Генотипы родителей ==="
    print parents
    
    putStrLn "\n=== Гаметы родителей ==="
    displayGametes parent1 parent2
    
    putStrLn "\n=== Первое поколение потомков (F1) ==="
    generation <- cross parents
    putStrLn $ show generation
    
    -- Добавляем анализ поколения
    analyzeGeneration generation

-- Необходимые дополнительные импорты


-- Регистрация генов и создание животных (остаются без изменений)
registerGenes :: IO [Gen]
registerGenes = do
    n <- readPositive "Введите количество генов: "
    forM [1..n] $ \i -> do
        putStrLn $ "\nГен #" ++ show i
        registerGene

registerGene :: IO Gen
registerGene = do
    name <- getInput "Название признака: "
    
    putStrLn "\nДоминантный аллель:"
    domLetter <- getLetter "Буква (заглавная): " >>= validateDominant
    domTrait <- getInput "Описание признака: "
    
    let recLetter = toLower domLetter
    putStrLn $ "\nРецессивный аллель будет: " ++ [recLetter]
    recTrait <- getInput "Описание признака: "
    
    return $ Gen name (Dominant domLetter domTrait) (Recessive recLetter recTrait)

createAnimal :: [Gen] -> String -> IO Individual
createAnimal genes name = do
    putStrLn $ "\n" ++ name ++ ":"
    putStrLn "Доступные гены:"
    mapM_ (putStrLn . formatGene) genes
    
    genotype <- forM genes $ \gen@(Gen traitName (Dominant d _) (Recessive r _)) -> do
        allelesStr <- getInput $ "Введите аллели для '" ++ traitName ++ "' (например, Aa): "
        case parseAlleles gen allelesStr of
            Just (a1, a2) -> return $ Gen traitName a1 a2
            Nothing -> do
                putStrLn "Ошибка ввода! Попробуйте снова."
                getGenInput gen traitName
    
    return $ Individual (Genotype genotype)
  where
    getGenInput gen traitName = do
        allelesStr <- getInput $ "Введите аллели для '" ++ traitName ++ "' (например, Aa): "
        case parseAlleles gen allelesStr of
            Just alleles -> return $ Gen traitName (fst alleles) (snd alleles)
            Nothing -> getGenInput gen traitName

-- Функция скрещивания
cross :: Parents -> IO Generation
cross (Parents p1 p2) = do
    let gametes1 = generateGametes p1
        gametes2 = generateGametes p2
        offspring = createOffspring p1 p2 gametes1 gametes2
    return $ Generation offspring
  where
    createOffspring (Individual (Genotype genes1)) _ gametes1 gametes2 =
        [ createChild genes1 g1 g2 | g1 <- gametes1, g2 <- gametes2 ]
      where
        createChild genes (Gamete alleles1) (Gamete alleles2) =
            let combinedGenes = zipWith3 combineGenes genes alleles1 alleles2
            in Individual (Genotype combinedGenes)
        
        combineGenes (Gen name _ _) a1 a2 = Gen name a1 a2

-- Остальные вспомогательные функции
displayGametes :: Individual -> Individual -> IO ()
displayGametes p1 p2 = do
    let gametes1 = generateGametes p1
        gametes2 = generateGametes p2
    
    putStrLn $ "\nРодитель 1 (" ++ genotypeStr p1 ++ "):"
    putStrLn $ "Всего гамет: " ++ show (length gametes1)
    putStrLn $ "Варианты: " ++ intercalate ", " (map gameteStr gametes1)
    
    putStrLn $ "\nРодитель 2 (" ++ genotypeStr p2 ++ "):"
    putStrLn $ "Всего гамет: " ++ show (length gametes2)
    putStrLn $ "Варианты: " ++ intercalate ", " (map gameteStr gametes2)
  where
    genotypeStr (Individual (Genotype genes)) = 
        concatMap (\(Gen _ a1 a2) -> [alleleChar a1, alleleChar a2]) genes
    gameteStr (Gamete alleles) = map alleleChar alleles
    alleleChar (Dominant c _) = c
    alleleChar (Recessive c _) = c

generateGametes :: Individual -> [Gamete]
generateGametes (Individual (Genotype genes)) =
    map Gamete (sequence (map (\(Gen _ a1 a2) -> [a1, a2]) genes))

parseAlleles :: Gen -> String -> Maybe (Allele, Allele)
parseAlleles (Gen _ (Dominant domChar domTrait) (Recessive recChar recTrait)) input =
    case input of
        [a1, a2] -> 
            let first  = if a1 == domChar 
                         then Dominant a1 domTrait 
                         else Recessive a1 recTrait
                second = if a2 == domChar
                         then Dominant a2 domTrait
                         else Recessive a2 recTrait
            in Just (first, second)
        _ -> Nothing

-- Вспомогательные функции ввода
getInput :: String -> IO String
getInput prompt = do
    putStr prompt
    hFlush stdout
    getLine

getLetter :: String -> IO Char
getLetter prompt = do
    str <- getInput prompt
    case str of
        [c] -> return c
        _   -> do
            putStrLn "Ошибка: введите одну букву"
            getLetter prompt

validateDominant :: Char -> IO Char
validateDominant c
    | isUpper c = return c
    | otherwise = do
        putStrLn "Ошибка: требуется заглавная буква"
        getLetter "Буква (заглавная): " >>= validateDominant

readPositive :: String -> IO Int
readPositive prompt = do
    str <- getInput prompt
    case readMaybe str of
        Just n | n > 0 -> return n
        _ -> do
            putStrLn "Ошибка: введите положительное число"
            readPositive prompt

formatGene :: Gen -> String
formatGene (Gen name (Dominant d _) (Recessive r _)) =
    name ++ ": " ++ [d] ++ "/" ++ [r] ++ " (Д/р)"
    
analyzeGeneration :: Generation -> IO ()
analyzeGeneration (Generation individuals) = do
    when (not $ null individuals) $ do
        let Individual (Genotype genes) = head individuals
            genesCount = length genes
        
        putStrLn "\nСтатистика по проявленным признакам в F1:"
        
        forM_ [0..genesCount-1] $ \geneIndex -> do
            let phenotypes = map (getPhenotype geneIndex) individuals
                total = length phenotypes
                domCount = length $ filter isDominantPhenotype phenotypes
                recCount = total - domCount
                domPercent = percent domCount total
                recPercent = percent recCount total
            
            when (total > 0) $ do
                let Gen name (Dominant d _) (Recessive r _) = genes !! geneIndex
                
                putStr $ "Ген " ++ name ++ ": "
                when (domPercent > 0) $ putStr $ show domPercent ++ "% " ++ [toUpper d] ++ " (" ++ domTrait genes geneIndex ++ ")"
                when (domPercent > 0 && recPercent > 0) $ putStr ", "
                when (recPercent > 0) $ putStr $ show recPercent ++ "% " ++ [toLower r] ++ " (" ++ recTrait genes geneIndex ++ ")"
                putStrLn ""
  where
    getPhenotype geneIndex (Individual (Genotype genes)) =
        let (Gen _ a1 a2) = genes !! geneIndex
        in if isDominant a1 || isDominant a2 
           then Dominant ' ' ""  -- Доминантный фенотип
           else Recessive ' ' ""  -- Рецессивный фенотип
    
    isDominantPhenotype (Dominant _ _) = True
    isDominantPhenotype _ = False
    
    domTrait genes idx = 
        case genes !! idx of
            Gen _ (Dominant _ t) _ -> t
    
    recTrait genes idx = 
        case genes !! idx of
            Gen _ _ (Recessive _ t) -> t
    
    isDominant (Dominant _ _) = True
    isDominant _ = False
    
    isRecessive (Recessive _ _) = True
    isRecessive _ = False
    
    percent count total = round ((fromIntegral count / fromIntegral total) * 100)
