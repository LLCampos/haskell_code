import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Enum, Eq)

data Month = Jan | Feb  | Mar | Apr | May  | Jun | Jul | Agu | Set | Oct | Nov | Dez deriving (Show, Enum) 

nextW Sunday = Monday
nextW x = succ x 

dSemana = dSemana' Monday 
       where 
       dSemana' x = x : (dSemana' $ nextW x)

janeiro = zip [1..31] $ repeat Jan
fevereiro1 = zip [1..28] $ repeat Feb
fevereiro2 = zip [1..29] $ repeat Feb
marco = zip [1..31] $ repeat Mar
abril = zip [1..30] $ repeat Apr
maio = zip [1..31] $ repeat May
junho = zip [1..30] $ repeat Jun
julho = zip [1..31] $ repeat Jul
agosto = zip [1..31] $ repeat Agu
setembro = zip [1..30] $ repeat Set
outubro = zip [1..31] $ repeat Oct
novembro = zip [1..30] $ repeat Nov
dezembro = zip [1..31] $ repeat Dez

ano1 :: [(Int, Month)]
ano1 = janeiro ++ fevereiro1 ++ marco ++ abril ++ maio ++ junho ++ julho ++ agosto ++ setembro ++ outubro ++ novembro ++ dezembro
ano2 = janeiro ++ fevereiro2 ++ marco ++ abril ++ maio ++ junho ++ julho ++ agosto ++ setembro ++ outubro ++ novembro ++ dezembro

juntar3 :: Int -> (Int,Month) -> (Int, Month, Int)
juntar3 z (x,y) = (x,y,z) 

datas = concat $ map bomba [1900..2000]
    where
    bomba x 
      | x == 1900 = map (juntar3 x) ano1
      | (mod x 4 == 0) = map (juntar3 x) ano2
      | otherwise = map (juntar3 x) ano1
 

mySuperZip :: [(Int,Month,Int)] -> [Weekday] -> [(Weekday,Int,Month,Int)]
mySuperZip [] _ = []
mySuperZip ((x,y,z):xs) (a:as) = (a,x,y,z) : mySuperZip xs as

problem19 = length $ filter (\x -> fst' x == Sunday && snd' x == 1) $ drop 365 $ mySuperZip datas dSemana
       where 
       fst' (x,y,z,a) = x 
       snd' (x,y,z,a) = y 



problem19' = map help [(toModifiedJulianDay $ fromGregorian 1900 1 1)..(toModifiedJulianDay $ fromGregorian 2000 12 31)]
   where
   help x = toGregorian $ ModifiedJulianDay 























