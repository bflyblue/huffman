module Main where

import Data.List
import Data.Maybe

data Symbol = Sym1 | Sym2 | Sym3 | Sym4
    deriving (Show, Eq, Ord)

data Bit = O | I
    deriving (Show, Eq)

data Tree a = Node Int (Tree a) (Tree a)
            | Leaf Int a
    deriving (Show, Eq)

instance Eq a => Ord (Tree a) where
    compare x y = compare (weight x) (weight y)

type FreqTable a = [(a, Int)]
type SymTable a = [(a, [Bit])]

count :: Eq a => a -> [a] -> Int
count s = length . filter (==s)

freqtable :: Eq a => [a] -> FreqTable a
freqtable syms = zip uniq freq
    where
        uniq = nub syms
        c s  = count s syms
        freq = map c uniq

weight :: Tree a -> Int
weight (Node w _ _) = w
weight (Leaf w _) = w

node :: Tree a -> Tree a -> Tree a
node x y = Node (weight x + weight y) x y

pqueue :: Eq a => FreqTable a -> [Tree a]
pqueue = sort . map mkLeaf
    where
        mkLeaf (sym, w) = Leaf w sym

hufftree :: Eq a => [Tree a] -> Tree a
hufftree [n] = n
hufftree (a:b:cs) = hufftree pq
    where
        n = node a b
        pq = insert n cs

huffman :: Eq a => [a] -> Tree a
huffman = hufftree . pqueue . freqtable

symtable :: Tree a -> SymTable a
symtable = symtable' []
    where
        symtable' p (Leaf _ sym) = [(sym, reverse p)]
        symtable' p (Node _ l r) = symtable' (O:p) l ++ symtable' (I:p) r

encode :: Eq a => SymTable a -> [a] -> [Bit]
encode tbl = concatMap (fromJust . flip lookup tbl)

decode :: Tree a -> [Bit] -> [a]
decode ht = decode' ht ht
    where
        decode' t (Leaf _ a)   bs     = a : decode t bs
        decode' t (Node _ l _) (O:bs) = decode' t l bs
        decode' t (Node _ _ r) (I:bs) = decode' t r bs
        decode' _ _            []     = []

main :: IO ()
main = do
    let msg = [Sym1, Sym2, Sym1, Sym2, Sym3, Sym2, Sym4]
        ht  = huffman msg
        st  = symtable ht
        encoded = encode st msg
        decoded = decode ht encoded

    putStrLn $ "Original message: " ++ show msg
    putStrLn $ "Encoded bitstring: " ++ show encoded
    putStrLn $ "Decoded message: " ++ show decoded

