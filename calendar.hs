{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Path.Metafont
import Diagrams.Prelude hiding (pad)
import Data.List.Split
import Data.Time.Clock
import Data.Time.Calendar
import System.Environment
import Control.Applicative
import Data.Time.Calendar.WeekDate

correction = translateY (-0.1)
opcty = opacity 0.19

class Format a where
        format :: a -> a -> Diagram B 

data Week = None | Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Eq, Enum, Show)

instance Format Week where
        format d x = txt <> bck
                    where c = if d == x then blue else white
                          txt = text (" " ++ show x ++ " ") # fontSize (local 0.4) # font "inconsolata" # fc c # correction
                          bck = rect 1 0.6 # lc white # opcty # fc white


data Date = Blank | Date Int deriving (Eq)

instance Format Date where
        format d d' = txt <> bck
                             where c = if d == d' then red else white
                                   t = case d' of 
                                            Blank -> "     "
                                            (Date n) -> if n <= 9 then "   " ++ show n ++ " " else "  " ++ show n ++ " "
                                   txt = text t # font "droid" # fontSize (local 0.4) # fc c # correction
                                   bck = square 1 # lc white # opcty # fc black



formatCal h f i n = map h [Sun .. Sat] : map (map f) theDates
    where theDates =  chunksOf 7 $ replicate i Blank ++ map Date [1..n]

calendar x = vcat $ (txt :) $ map hcat $ formatCal (format None) (format (Date d)) (i `mod` 7) n
    where (y,m,d) = toGregorian x 
          n = gregorianMonthLength y m
          (_, _, i) = toWeekDate $ fromGregorian y m 1
          txt = (text (mname ++ ", "++ show y) # font "droid" # fontSize (local 0.4) # fc white # translateX 3 <> rect 14 0.2 # fc black) # translateY (-0.5)
          mname = show $ (toEnum (m-1) :: Month)

calendarIO x = (calendar x # translate ( -2.1 ^& (-2.8)) # scale 0.7  <> rect 13.66 7.68 # fc black :: Diagram B)

main = do x <- (utctDay) <$> getCurrentTime
          mainWith (calendarIO x)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Enum)

instance Show Month where
        show Jan = "January"
        show Feb = "February"
        show Mar = "March"
        show Apr = "April"
        show May = "May"
        show Jun = "June"
        show Jul = "July"
        show Aug = "August"
        show Sep = "September"
        show Oct = "October"
        show Nov = "November"
        show Dec = "December"

