{-# LANGUAGE UndecidableInstances, MonadComprehensions, FlexibleInstances,
             DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module GenAlg where

import Prelude
import System.IO()
import System.Environment 
import Data.Functor
import Data.Function
import Control.Monad.Random
import Control.Monad.Writer.Lazy
import Data.BitVector (BitVector)

import qualified Data.BitVector as BV
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L

type Rnd a = Rand StdGen a

type Profit = Int
type Weight = Int
type Item = (Profit, Weight)

-- | This functions chooses a random number for bit extraction
randNumBits :: BitVector -- ^ BitVector is question
            -> Rnd Int
randNumBits c = getRandomR (0, BV.size c)

-- | This function creates an initial solution list
createInitialPopulation :: [Item] -- ^ Items to choose from
                        -> Int -- ^ Number of initial chromosomes
                        -> Rnd [BitVector] 
createInitialPopulation items popNum = 
    let len = length items in
    mapM (\_ -> 
            do 
                let upperBound = fromIntegral (BV.maxNat len :: Int) :: Int
                num <- getRandomR (0, upperBound)
                return (BV.bitVec len num)) [0..popNum]

-- | This function randomly combines two chromosomes.
breed :: BitVector -- ^ MSB bits come from this chromosome
      -> BitVector  -- ^ LSB bits come from this chromosome
      -> Rnd BitVector
breed c1 c2 = do
    numC2Bits <- randNumBits c2
    let numC1Bits = BV.size c1 - numC2Bits
    if numC2Bits == 0 then return $ BV.most numC1Bits c1 else 
        if numC2Bits == BV.size c2 then return $ BV.least numC2Bits c2 else 
            return $ BV.most numC1Bits c1 BV.# BV.least numC2Bits c2

-- | This function randomly mutates a chromosome
mutate :: BitVector -- ^ Chromosome to be mutated
       -> Int -- ^ Mutation rate
       -> Rnd BitVector
mutate c m =  BV.fromBits <$> mapM randMutate (BV.toBits c)
    where randMutate b = do 
            mutateNum <- getRandomR(0, 99)
            if mutateNum < m then return (not b) else return b

-- | This function selects a high quality chromosome based on total fitness
rouletteSelect :: [BitVector] -- ^ Chromosomes to choose from
               -> [Int] -- ^ Fitness list
               -> Rnd BitVector
rouletteSelect pop fitnessList = 
    let fitnessSum = sum fitnessList in
    do
        num <- getRandomR(0, fitnessSum)
        return $ go pop fitnessList (num - 1)
        where go :: [BitVector] -> [Int] -> Int -> BitVector
              go (x : xs) (y : ys) n | n - y <= 0 = x
                                     | otherwise = go xs ys (n - y)
              go _ _ _ = error "Something went wrong in rouletteSelect"

-- | This function evaluates the fitness of a chromosome
fitness :: BitVector -- ^ Chromosome to be evaluated
        -> [Item] -- ^ Items to choose from
        -> Int -- ^ Capacity of knapsack
        -> Int
fitness c items capacity = 
    let profit = sum $ zipWith (\x (y,_) -> if x then y else 0) 
                (BV.toBits c) items in
    let weight = sum $ zipWith (\x (_,z) -> if x then z else 0) 
                (BV.toBits c) items in
    if weight > capacity then 0 else profit

-- | This function assesses the fitness of the chromosomes
populationFitness:: [BitVector] -- ^ Chromosome list
                 -> [Item] -- ^ Item list
                 -> Int -- ^ Knapsack capacity
                 -> [Int]
populationFitness chromosomes items capacity =
    map (\c -> fitness c items capacity) chromosomes

-- | This function pulls the elite chromosomes from the population
getElite :: [BitVector] -- ^ Chromosome list
         -> [Int] -- ^ Fitness list
         -> BitVector
getElite chromosomes fitnessList = 
    let fitnessAssociatedList = zip chromosomes fitnessList in
        fst $ head (L.sortBy (flip compare `on` snd) fitnessAssociatedList)

-- | This function finds the most frequent fitness value and correlates 
-- | it to a chromosome
mostFrequentElement :: [BitVector] -- ^ Chromosome list
                    -> [Int] -- ^ Fitness list
                    -> (BitVector, Int, Int)
mostFrequentElement pop l =
    if null l then error "Something went wrong in mostFrequentElement" else
        let partitioned = L.group $ L.sort l in
        go partitioned
        where go :: [[Int]] -> (BitVector, Int, Int)
              go [] = error "Something went wrong in mostFrequentElement"
              go [x] = (find (L.head x) (zip pop l), L.head x, L.length x)
                 where find :: Int -> [(BitVector, Int)] -> BitVector
                       find i ((a,b): xs) = if b == i then a else find i xs
                       find _ _ = error "Something happened here"
              go (x : y : ys) = 
                  if L.length x > L.length y then  go (x : ys) else go (y : ys)
                  
-- | This function determines if a solution chromosome is optimal
optimalSolution :: [BitVector] -- ^ Chromosome list
                -> [Int] -- ^ Fitness list
                -> Double -- ^ Homogeneity
                -> (Bool, BitVector, Int)
optimalSolution pop fitnessList homogeneity =
    let (repBV, mostFreqElt, freq) = mostFrequentElement pop fitnessList in
    let dblFreq = fromIntegral freq :: Double in
    let dblLength = fromIntegral (L.length fitnessList) :: Double in
    if (dblFreq / dblLength) * 100 >= homogeneity  && mostFreqElt /= 0
        then (True, repBV, mostFreqElt) else (False, repBV, mostFreqElt)

-- | This function runs one generation of the simulation
runOneGeneration :: [BitVector] -- ^ Chromosome list
                 -> [Item] -- ^ Item list
                 -> Int -- ^ Mutation rate
                 -> Int -- ^ Cross over
                 -> Int -- ^ Capacity
                 -> WriterT String IO [BitVector]
runOneGeneration pop items mutRate crossOver capacity =
    let fitnessList = populationFitness pop items capacity in
    let elite = getElite pop fitnessList in
    do
        rest <- liftIO $ evalRandIO $ go pop items fitnessList mutRate (L.length pop - 1) capacity []
        let final = elite : rest
        tell ("Generation: " ++ show final ++ "\n")
        return final
    where go :: [BitVector] -- ^ Population to be altered
             -> [Item] -- ^ Item list
             -> [Int] -- ^ Fitness List
             -> Int -- ^ Mutation rate
             -> Int -- ^ Chromosomes to be bred
             -> Int  -- ^ Capacity constraint
             -> [BitVector] -- ^ Outputted population
             -> Rnd [BitVector]
          go _ _ _ _ 0 _ l = return l
          go p i f m n c l =
            do
                parent1 <- rouletteSelect pop f
                parent2 <- rouletteSelect pop f
                num <- getRandomR (0, 100) :: Rnd Int
                child <- breed parent1 parent2
                mutChild <- mutate child m
                if num <= crossOver then go p i f m (n-1) c (mutChild : l) else 
                    go p i f m (n-1) c (parent1 : l)

-- | This function runs all generations of the simulation
runAllGenerations :: [BitVector] -- ^ Chromosome list
                  -> [Item] -- ^ Item list
                  -> Int -- ^ Mutation rate
                  -> Int -- ^ Generation limit
                  -> Int -- ^ Cross over
                  -> Double -- ^ Homogeneity
                  -> Int -- ^ Capacity
                  -> WriterT String IO ()
runAllGenerations pop items mutRate n crossOver homogeneity capacity =
    if n > 10000 then do 
        let maxFitness = populationFitness pop items capacity
        let (_, maxOptC, maxOptVal) = optimalSolution pop maxFitness homogeneity
        liftIO $ putStrLn 
            ("Running too long, best guess is: " ++ show maxOptVal
              ++ ". Optimal items are: " ++ show (getItems (BV.toBits maxOptC) items []))
    else do
        newPop <- runOneGeneration pop items mutRate crossOver capacity
        let popFitness = populationFitness newPop items capacity
        let (isOpt, optC, optVal) = optimalSolution newPop popFitness homogeneity
        if isOpt && n <= 0 then liftIO $ putStrLn 
                ("Optimal value is: " ++ show optVal
                    ++ ". Optimal items are: " 
                    ++ show (getItems (BV.toBits optC) items [])) else
            runAllGenerations newPop items mutRate (n - 1) crossOver homogeneity capacity
            where getItems :: [Bool] -- ^ Bitvector converted to a boolean list
                           -> [Item] -- ^ Item list
                           -> [Item] -- ^ Item list to be returned
                           -> [Item]
                  getItems (x : xs) (y : ys) l = getItems xs ys (if x then y : l else l)
                  getItems [] [] l = l
                  getItems _ _ _ = error "Nope"

-- | This function runs a user inputted command
runCommand :: String -- ^ Command
           -> [Item] -- ^ Items to choose from
           -> Int -- ^ Mutation rate
           -> Int -- ^ Population number
           -> Int -- ^ Generation limit
           -> Int -- ^ Cross over
           -> Double -- ^ Homogeneity
           -> Int -- ^ Capacity
           -> WriterT String IO ()
runCommand prompt items mutRate popNum genLimit crossOver homogeneity capacity =
    do
        liftIO $ putStrLn prompt
        command <- liftIO getLine
        case command of
            "runAll"  -> 
                if notEnoughCapacity capacity items then
                    liftIO $ putStrLn "No items fit in knapsack"
                else do
                    initialPop <- liftIO $ evalRandIO $ createInitialPopulation items popNum
                    tell ("Initial Population: " ++ show initialPop ++ "\n")
                    runAllGenerations initialPop items mutRate genLimit crossOver homogeneity capacity
            "quit"    -> liftIO $ putStrLn "quitting simulation..."
            _         -> runCommand prompt items mutRate popNum
                                    genLimit crossOver homogeneity capacity

main :: IO ()
main = 
  do  
      args <- getArgs
      case args of 
          [file] -> 
              do
                  contents <- C.readFile file
                  let items = readTuples contents
                  putStrLn ("Items: " ++ show items)
                  popNum <- getPopNum 
                      "Enter a population number" 
                  mutRate <- getMutRate 
                      "Enter a mutation rate between 0 and 100%"
                  genLimit <- getGenLimit
                      "Enter a generation limit"   
                  capacity <- getCapacity
                      "Enter a knapsack capacity"  
                  crossOver <- getCrossOver
                      "Enter a cross over rate between 0 and 100%" 
                  homogeneity <- getHomogeneity
                      "Enter a homogeneity requirement between 0 and 100%" 
                  (_, l) <- runWriterT $ runCommand "Enter a valid command" 
                                             items mutRate popNum genLimit crossOver homogeneity capacity
                  writeFile "output.txt" l
          _      -> putStrLn "Need to enter an item file"

-- | This function checks to see if no items fit in the knapsack
notEnoughCapacity :: Int -- ^ The capacity constraint
                  -> [Item] -- ^ The item list
                  -> Bool
notEnoughCapacity _ [] = True
notEnoughCapacity c ((_, weight) : xs) = c < weight && notEnoughCapacity c xs 

-- | This function reads a bytestring as a tuple
readTuples :: C.ByteString -- ^ The bytestring in question
           -> [Item]
readTuples s =
    let items = C.lines s in
    map (\i -> 
            let [a,b] = C.split ',' i in
            (read (C.unpack a), read (C.unpack b))) items


-- The following functions read user inputs

getPopNum :: String -> IO Int
getPopNum prompt =
    do
        putStrLn prompt
        numS <- getLine
        let num = read numS
        if num < 1 then getPopNum prompt else return num

getMutRate :: String -> IO Int
getMutRate prompt =
    do
        putStrLn prompt
        numS <- getLine
        let num = read numS
        if num < 0 || num > 100 
            then getMutRate prompt
            else return num

getGenLimit :: String -> IO Int
getGenLimit prompt =
    do
        putStrLn prompt
        numS <- getLine
        let num = read numS
        if num < 0 then getGenLimit prompt else return num

getCapacity :: String -> IO Int
getCapacity prompt =
    do
        putStrLn prompt
        numS <- getLine
        let num = read numS
        if num < 0 then getCapacity prompt else return num

getCrossOver :: String -> IO Int
getCrossOver prompt =
    do
        putStrLn prompt
        numS <- getLine
        let num = read numS
        if num < 0  || num > 100 
          then getCrossOver prompt 
          else return num

getHomogeneity :: String -> IO Double
getHomogeneity prompt =
    do
        putStrLn prompt
        numS <- getLine
        let num = read numS
        if num < 0  || num > 100 
          then getHomogeneity prompt 
          else return num
