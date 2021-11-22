{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Data.List (unfoldr)
import GHC.Base (join, VecCount)
import Data.Maybe (fromJust)
import Control.Applicative
import Data.Foldable (Foldable(fold))
import GHC.Float (int2Float, float2Int)
import GHC.IO.Handle.FD (openBinaryFile)
import GHC.IO.IOMode (IOMode(ReadMode, WriteMode))
import GHC.IO.Handle (hGetChar, hGetLine, hPutStr)
import Data.Char (ord, chr)
import GHC.Stack (whoCreated)
main :: IO ()
main = do
    handle <- openBinaryFile "path" ReadMode
    handle2 <- openBinaryFile "path2" WriteMode

    c <- hGetLine handle

    let image = stringToImage c

    let ave = clipAndTrance 256 256 $ aveFiltering image
    hPutStr handle2 $ imageToString ave

    print "end"

--平均化フィルタ(3*3)
--中心を左上から真ん中に移動している
avePattern :: Pattern Float
avePattern = iright $ idown $ fromJust $ listsToPattern [[1,1,1],[1,1,1],[1,1,1]]
aveFiltering :: Image Float -> Image Float
aveFiltering = filterToFiltering $ patternToFilter avePattern

--平均化フィルタ(5*5)
--中心を左上から真ん中に移動している
avePattern' :: Pattern Float
avePattern' = iright.iright.idown.idown $ fromJust $ listsToPattern [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]]
aveFiltering' :: Image Float -> Image Float
aveFiltering' = filterToFiltering $ patternToFilter avePattern'

--y = x^3/65536 の式で表せるトーンカーブ
toneCurve :: Image Float -> Image Float
toneCurve = fmap (\x -> x * x * x / 65536)

--ネガポジ変換
negaPosiTrance :: Image Float -> Image Float
negaPosiTrance = fmap (255 - )

--------以下ImageやZipperの定義など--------

--デバッグ用Image
testImage :: Image Float 
testImage = fromJust $ listsToImage' 1 [[3,3,3,3],[3,4,4,3],[3,4,4,3],[3,3,3,3]]

--画像の切り抜きと型変換を行う
clipAndTrance :: Int -> Int -> Image Float -> Image Int
clipAndTrance x y i = itake' 0 (x-1) 0 (y-1) $ float2Int <$> i

--画像を文字列に変換
imageToString :: Image Int -> [Char]
imageToString (Image (Zipper lzs xz rzs)) =  foldr (\a s ->s ++ show' a) "" lzs ++ show' xz ++ foldr (\a s -> show' a  ++ s) "" rzs
    where
        show' :: Zipper Int -> [Char]
        show' (Zipper ls x rs) = foldr (\a s -> s ++ [chr a]) "" ls ++ [chr x] ++ foldr (\a s -> [chr a] ++ s) "" rs

--文字列を画像に変換、四方八方を0で埋める
stringToImage :: [Char] -> Image Float
stringToImage poppo = fromJust $ listsToImage $ fmap int2Float <$> splitList 256 (ord <$> poppo)

--文字列を画像に変換、四方八方を与えられた値で埋める
stringToImage' ::Float -> [Char] -> Image Float
stringToImage' f poppo = fromJust $ listsToImage' f $ fmap int2Float <$> splitList 256 (ord <$> poppo)

--Listを与えられた数の長さに分割する
splitList :: Int -> [a] -> [[a]]
splitList i [] = []
splitList i as = take i as : splitList i (drop i as)
data Zipper a = Zipper [a] a [a]

--ピックアップしている値を右にずらす
right :: Zipper a -> Zipper a
right (Zipper ls x rs) = Zipper (x:ls) (head rs) (tail rs)

--ピックアップしている値を左にずらす
left :: Zipper a -> Zipper a
left (Zipper ls x rs) = Zipper(tail ls) (head ls) (x:rs)

--中心から左右何個分かの要素を取ったZipperを返す
ztake :: Int -> Zipper a -> Zipper a
ztake n (Zipper ls x rs) = Zipper (take n ls) x (take n rs)

ztake' :: Int -> Int -> Zipper a -> Zipper a
ztake' l r (Zipper ls x rs) = Zipper (take l ls) x (take r rs)

--近傍の畳み込み計算を一般化したデータ構造
class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)

--Zipperはファンクタである
instance Functor Zipper where
    --リストのすべての元に与えられた関数を適用
    fmap f (Zipper ls x rs) = Zipper (fmap f ls) (f x) (fmap f rs)

--ZipperはApplicativeファンクタである
instance Applicative Zipper where
    pure a = Zipper [] a []
    liftA2 f (Zipper ls1 x1 rs1) (Zipper ls2 x2 rs2) = Zipper (h f ls1 ls2) (f x1 x2) (h f rs1 rs2) where
        h g l1 l2 =  uncurry g <$> zip l1 l2

--Zipperは文字列に変換できる    
instance (Show a) => Show (Zipper a) where
    show (Zipper ls x rs) = foldr (\a s -> s ++ show a) "" ls ++"["++ show x ++ "]" ++ foldr (\a s -> show a ++ s) "" rs

--Zipperはコモナドである。
instance Comonad Zipper where
    --ピックアップしている元の抽出
    extract (Zipper _ x _) = x
    --Zipperを要素とするZipperを生成、各Zipperは注目する元が一つずつずれる
    duplicate z = Zipper lzs xz rzs
        where
            lzs = tail $ iterate left z
            xz = z
            rzs = tail $ iterate right z

--二重Zipper型
--画像データをこの型で扱う
newtype Image a = Image(Zipper (Zipper a))

--注目している点を上にずらす
iup :: Image a -> Image a
iup (Image z) = Image $ left z

--注目している点を下にずらす
idown :: Image a -> Image a
idown (Image z) = Image $ right z

--注目している点を左にずらす
ileft :: Image a -> Image a
ileft (Image z) = Image $ left <$> z

--注目している点を右にずらす
iright :: Image a -> Image a
iright (Image z) = Image $ right <$> z

--Imageはファンクタである
instance Functor Image where
    --画像のすべての点に与えられた関数を適用
    --つまり画素ごとの濃淡変換
    fmap f (Image z) = Image $ fmap f <$> z

--ImageはApplicativeファンクタである
instance Applicative Image where
    pure a = Image (Zipper [] (Zipper [] a []) [])
    liftA2 f (Image z1) (Image z2) =  Image $ liftA2 (liftA2 f) z1 z2

--Imageはコモナドである
instance Comonad Image where
    extract (Image z) = extract $ extract z
    duplicate i = Image $ fmap dheight (dwidth i)

--注目している点を上下にずらしたZipperを生成
dheight :: Image a -> Zipper (Image a)
dheight i =Zipper(tail(iterate ileft i))i(tail(iterate iright i))

--注目している点を左右にずらしたZipperを生成
dwidth :: Image a -> Zipper (Image a)
dwidth i =Zipper(tail(iterate iup i))i(tail(iterate idown i))

--ベクトル空間
--Floatも体で一般化したかった
class Vector v where
    vadd :: v -> v -> v
    smult :: Float -> v -> v
    vone :: v
    --ベクトル空間の公理にはないけど必要そう
    vmult :: v -> v -> v

vzero :: (Vector v) => v
vzero = smult 0 vone

--FloatはFloat上のベクトル空間である
instance Vector Float where
    vadd = (+)
    smult = (*)
    vone = 1
    vmult = (*)

--Vectorを要素とするZipperはVectorである
instance (Vector v) => Vector (Zipper v) where
    vadd = liftA2 vadd
    smult k v = fmap (smult k) v
    vone = Zipper (repeat vone) vone (repeat vone)
    vmult = liftA2 vmult

--Vectorを要素とするImageはVectorである
instance (Vector v) => Vector (Image v) where
    vadd = liftA2 vadd
    smult k v = fmap (smult k) v
    vone = Image $ Zipper (repeat vone) vone (repeat vone)
    vmult = liftA2 vmult

--Imageは文字列に変換できる
--デバッグ用
instance Show a => Show (Image a) where
    show (Image (Zipper lzs xz rzs)) =  foldr (\a s ->s ++ show' a ++"\n") "" lzs ++ show xz ++ "\n" ++ foldr (\a s -> show' a ++ "\n" ++ s) "" rzs where
        show' (Zipper ls x rs) = foldr (\a s -> s ++ show a) "" ls ++ show x ++ foldr (\a s -> show a ++ s) "" rs

--ListからZipperを生成
--リストの外側はすべてzeroで埋める
listToZipper :: Vector v => [v] -> Maybe (Zipper v)
listToZipper (a:as) = Just $ Zipper (repeat vzero) a $ as ++ repeat vzero
listToZipper [] = Nothing

--二重リストからImageを生成
--リストの外側はすべてzeroで埋める
listsToImage :: Vector v => [[v]] -> Maybe (Image v)
listsToImage ls = Image <$> (join $ fmap listToZipper $ maybeLift $ fmap listToZipper ls)

--ListからZipperを生成
--リストの外側はすべてzeroで埋める
listToZipper' :: Vector v => v -> [v] -> Maybe (Zipper v)
listToZipper' v (a:as) = Just $ Zipper (repeat v) a $ as ++ repeat v
listToZipper' v [] = Nothing

--二重リストからImageを生成
--リストの外側はすべてzeroで埋める
listsToImage' :: Vector v => v -> [[v]] -> Maybe (Image v)
listsToImage' v ls = Image <$> (join $ fmap (listToZipper' (Zipper (repeat v) v (repeat v))) $ maybeLift $ fmap (listToZipper' v) ls)

itake :: Int -> Int -> Image a -> Image a
itake x y (Image z) = Image $ ztake y $ fmap (ztake x) z

itake' :: Int -> Int -> Int -> Int -> Image a -> Image a
itake' l r u d (Image z)= Image $ ztake' u d $ fmap (ztake' l r) z

--ListとMaybeの交換
maybeLift :: [Maybe a] -> Maybe [a]
maybeLift = foldr f (Just []) where
    f :: Maybe a -> Maybe [a] -> Maybe [a]
    f (Just a) (Just as) = Just $ a:as
    f Nothing _ = Nothing
    f _ Nothing = Nothing

--画像用のZipperとフィルタに使うZipperを分けて書く
type Pattern a = Image a

--ListからPatternを生成
listsToPattern :: Vector v => [[v]] -> Maybe (Pattern v)
listsToPattern ls = Image <$> (join $ fmap list $ maybeLift $ fmap list ls) where
    list (a:as) = Just $ Zipper [] a as
    list [] = Nothing

--デバッグ用Zipper
testZipper :: Zipper Float
testZipper = fromJust $ listToZipper [1,2,3,4,5]

--パターンをフィルタ関数に変換
patternToFilter :: (Vector v) => Pattern v -> Image v -> v
patternToFilter i1 i2 = smult (1 / int2Float (countPattern i1)) (sumImage $ applyToImage i1 i2)
        where
            applyToImage (Image (Zipper ls1 x1 rs1)) (Image (Zipper ls2 x2 rs2)) = Image $ Zipper (applyToZipper <$> zip ls1 ls2) (applyToZipper (x1,x2)) (applyToZipper <$> zip rs1 rs2)
            applyToZipper (Zipper ls1 x1 rs1,Zipper ls2 x2 rs2) = Zipper (uncurry vmult <$> zip ls1 ls2) (vmult x1 x2) (uncurry vmult <$> zip rs1 rs2)
            sumZipper (Zipper ls x rs) = foldr vadd (foldr vadd x ls) rs
            sumImage (Image (Zipper ls x rs)) = foldr (\a b -> vadd b (sumZipper a)) (foldr (\a b -> vadd b (sumZipper a)) (sumZipper x) ls) rs
            countZipper (Zipper ls x rs) = length ls + 1 + length rs
            countPattern (Image (Zipper ls x rs)) = sum(countZipper <$> ls) + countZipper x + sum(countZipper <$> rs)

--フィルタ関数を画像から画像へのフィルタリング関数へ変換
--Imageの持つComonadの性質のみで定義できるほどに抽象的な関数になっている
filterToFiltering :: Comonad f => (f a -> b) -> f a -> f b
filterToFiltering f i = fmap f (duplicate i)