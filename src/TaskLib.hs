module TaskLib where

import Data.Char (isDigit)
import Data.List
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Text.Printf (printf)

eotTok :: String
eotTok = "---"

descTok :: String
descTok = "Description:"

cdTok :: String
cdTok = "Date of Creation:"

ddTok :: String
ddTok = "Due Date:"

data Date = Date
  { day :: Int,
    month :: Int,
    year :: Int
  }
  deriving (Show, Eq)

dateStr :: Date -> String
dateStr (Date d m y) = printf "%02d-%02d-%04d" d m y

fromStr :: String -> Date
fromStr s =
  let d = read ds
      m = read ms
      y = read ys
   in Date d m y
  where
    cleanedStr = filter isDigit s
    (ds, r1) = splitAt 2 cleanedStr
    (ms, ys) = splitAt 2 r1

today :: IO Date
today = do
  (y, m, d) <- toGregorian . utctDay <$> getCurrentTime
  pure (Date (fromIntegral d) (fromIntegral m) (fromIntegral y))

data Task = Task
  { description :: String,
    creationDate :: Date,
    dueDate :: Maybe Date
  }
  deriving (Show, Eq)

createTask :: String -> Date -> Maybe Date -> Task
createTask = Task

toString :: Task -> String
toString (Task d cd mabye_dd) = if length sdd > 1 then sd <> scd <> sdd <> eot else sd <> scd <> eot
  where
    sd = descTok <> d <> "\n"
    scd = cdTok <> dateStr cd <> "\n"
    sdd = case mabye_dd of
      Just dd -> ddTok <> dateStr dd <> "\n"
      Nothing -> ""
    eot = eotTok <> "\n"

parseTasks :: String -> Maybe [Task]
parseTasks txt = undefined
  where
    lines' = lines txt
    raw = rawTasks lines'

rawTasks :: [String] -> [[String]]
rawTasks [] = []
rawTasks xs = filter (not . null) $ foldl f [[]] xs
  where
    f acc x
      | x == eotTok = [] : acc
      | otherwise = [x : head acc] <> tail acc

t1 = Task {description = "Feed Dog", creationDate = (Date 12 1 2022), dueDate = (Just $ Date 13 1 2022)}

t2 = Task {description = "Trip with Aunt", creationDate = (Date 12 1 2022), dueDate = Nothing}
