module AoC.Day09 (solution1, solution2) where

data File = File {idx :: Int, fileSize :: Int} deriving (Show)

newtype Space = Space {spaceSize :: Int} deriving (Show)

(.+.) :: Space -> Space -> Space
(.+.) s1 s2 =
  Space {spaceSize = spaceSize s1 + spaceSize s2}

data DiskData = DiskFile File | DiskSpace Space deriving (Show)

type DiskMap = [DiskData]

-- TODO: Handle size = 0
parseFile :: Int -> String -> DiskMap
parseFile _ [] = []
parseFile idx' (x : xs) =
  DiskFile File {idx = idx', fileSize = read [x] :: Int} : parseSpace (idx' + 1) xs

-- TODO: Handle size = 0
parseSpace :: Int -> String -> DiskMap
parseSpace _ [] = []
parseSpace idx' (x : xs) =
  DiskSpace Space {spaceSize = read [x] :: Int} : parseFile idx' xs

fileToBlocks :: File -> [Int]
fileToBlocks file = replicate (fileSize file) (idx file)

fileToBlocksWithLimit :: Int -> File -> (File, [Int])
fileToBlocksWithLimit limit file =
  (File {idx = idx file, fileSize = fileSize file - limit}, replicate limit (idx file))

moveUp :: [Int] -> Space -> Int -> DiskMap -> (Space, DiskMap, [Int])
moveUp _ endSpace _ [] = (endSpace, [], [])
moveUp acc endSpace diskSpace (DiskFile file : xs)
  | fileSize file < diskSpace =
      moveUp
        (fileToBlocks file)
        endSpace
        (diskSpace - fileSize file)
        xs
  | fileSize file == diskSpace =
      (endSpace, xs, acc ++ fileToBlocks file)
  | otherwise =
      let (file', acc') = fileToBlocksWithLimit diskSpace file
       in (endSpace, DiskFile file' : xs, acc ++ acc')
moveUp acc endSpace diskSpace (DiskSpace space : xs) =
  moveUp acc (endSpace .+. space) diskSpace xs

compacting :: Space -> DiskMap -> [Int]
compacting _ [] = []
compacting endSpace (DiskFile file : xs) =
  fileToBlocks file ++ compacting endSpace xs
compacting endSpace (DiskSpace space : xs) =
  let (endSpace', xs', res) = moveUp [] endSpace (spaceSize space) (reverse xs)
   in res ++ compacting endSpace' (reverse xs')

checkSum :: [Int] -> Int
checkSum xs = sum (zipWith (*) xs [0 ..])

solution1 :: [String] -> Int
solution1 input = checkSum (compacting (Space {spaceSize = 0}) (parseFile 0 (head input)))

solution2 :: [String] -> Int
solution2 input = 0
