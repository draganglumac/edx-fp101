module HOFs where

-- Excercise 11 - unfold
unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
-- p :: (b -> Bool) - termination predicate
-- h :: (b -> a)    - mapping from b to a
-- t :: (b -> b)    - generation of next seed
-- x :: b           - initial seed
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
--int2bin 0 = []
--int2bin n = n `mod` 2 : int2bin (n `div` 2)
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop8 = unfold null (take 8) (drop 8)


-- Excerise 12
unfoldMap :: (a -> b) -> [a] -> [b]
unfoldMap f = unfold null (f . head) tail


-- Excercise 13
unfoldIterate :: (a -> a) -> a -> [a]
unfoldIterate f = unfold (const False) id f
