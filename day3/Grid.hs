module Grid where

import ListZipper

type Row = ListPath Int
type Grid = ListPath Row

infList :: [Int]
infList = 0 : infList

row :: Row
row = ListPath infList 0 infList

rowMain :: Row
rowMain = ListPath infList 1 infList

infRow :: [Row]
infRow = row : infRow

initialGrid :: Grid
initialGrid =  (ListPath infRow rowMain infRow)
