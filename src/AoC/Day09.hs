module AoC.Day09 (solution1, solution2) where

data File = File {idx :: Int, fileSize :: Int} deriving (Show)

newtype Space = Space {spaceSize :: Int} deriving (Show)

data DiskData = DiskFile File | DiskSpace Space deriving (Show)

type DiskMap = [DiskData]

parseFile :: Int -> String -> DiskMap
parseFile _ [] = []
parseFile idx' (x : xs) =
  case read [x] :: Int of
    0 -> parseSpace idx' xs
    n -> DiskFile File {idx = idx', fileSize = n} : parseSpace (idx' + 1) xs

parseSpace :: Int -> String -> DiskMap
parseSpace _ [] = []
parseSpace idx' (x : xs) =
  case read [x] :: Int of
    0 -> parseFile idx' xs
    n -> DiskSpace Space {spaceSize = n} : parseFile idx' xs

fileToBlocks :: File -> [Int]
fileToBlocks file = replicate (fileSize file) (idx file)

fileToBlocksWithLimit :: Int -> File -> (File, [Int])
fileToBlocksWithLimit limit file =
  (File {idx = idx file, fileSize = fileSize file - limit}, replicate limit (idx file))

moveUp :: [Int] -> Int -> DiskMap -> (DiskMap, [Int])
moveUp _ _ [] = ([], [])
moveUp acc diskSpace (DiskFile file : xs)
  | fileSize file < diskSpace =
      moveUp
        (fileToBlocks file)
        (diskSpace - fileSize file)
        xs
  | fileSize file == diskSpace =
      (xs, acc ++ fileToBlocks file)
  | otherwise =
      let (file', acc') = fileToBlocksWithLimit diskSpace file
       in (DiskFile file' : xs, acc ++ acc')
moveUp acc diskSpace (DiskSpace _ : xs) =
  moveUp acc diskSpace xs

compacting :: DiskMap -> [Int]
compacting [] = []
compacting (DiskFile file : xs) =
  fileToBlocks file ++ compacting xs
compacting (DiskSpace space : xs) =
  let (xs', res) = moveUp [] (spaceSize space) (reverse xs)
   in res ++ compacting (reverse xs')

singleInteger :: Int -> [Int]
singleInteger x = map (read . (: [])) (show x)

singleIntegers :: [Int] -> [Int]
singleIntegers = foldr (\x acc -> singleInteger x ++ acc) []

checkSum :: [Int] -> Int
checkSum xs = sum (zipWith (*) (singleIntegers xs) [0 ..])

solution1 :: [String] -> Int
solution1 input = checkSum (compacting (parseFile 0 (head input)))

solution2 :: [String] -> [Int]
solution2 input =
  [] -- singleIntegers (compacting (parseFile 0 (head input)))
